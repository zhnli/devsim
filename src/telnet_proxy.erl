-module(telnet_proxy).
-export([start/1, new_conn_nb/1, send_msg_a2c/2, send_msg_c2a/2]).

-include("devsim.hrl").
-include("log.hrl").

-record(state, {acceptor_map}).
-record(acceptor_state, {stage, prompt, buff}).

%% ========================================
%% API
%% ========================================

start(ConfigOrig) ->
    {ok, Addr} = inet:parse_address(ConfigOrig#config.remote_addr),
    Config = ConfigOrig#config{remote_addr=Addr},
    {ok, State} = init(Config),
    Pid = spawn(fun() -> loop(State, Config) end),
    register(?MODULE, Pid),
    {ok, Pid}.

init(Config) ->
    ?info("Config=~p", [Config]),
    telnet_server:start(Config#config.local_port_telnet),
    ets:new(a2c_table, [set, named_table, public]),
    ets:new(c2a_table, [set, named_table, public]),
    ets:new(telnet_record, [set, named_table, public]),
    State = #state{acceptor_map=maps:new()},
    {ok, State}.

new_conn_nb(Acceptor) ->
    ?info("telnet_proxy: new_conn_nb()"),
    ?MODULE ! {self(), {new_conn_nb, Acceptor}}.

send_msg_a2c(Acceptor, Msg) ->
    ?MODULE ! {self(), {send_msg_a2c, Acceptor, Msg}}.

send_msg_c2a(Client, Msg) ->
    ?MODULE ! {self(), {send_msg_c2a, Client, Msg}}.

%% ========================================
%% private functions
%% ========================================

change_acceptor_state(State, Acceptor, NewAccState) ->
    NewMap = maps:put(Acceptor, NewAccState, State#state.acceptor_map),
    State#state{acceptor_map=NewMap}.

get_system_prompt() ->
    case data_store:read_telnet(system_prompt__) of
    [] ->
        ?error("Failed to find system_prompt__"),
        <<"\r\n#">>;
    [Prompt | _] ->
        Prompt
    end.

handle_new_conn_nb(Config, State, Acceptor) ->
    ?info("Handle new connection from north"),
    case Config#config.mode of
    sim ->
        AccState = #acceptor_state{stage=username_prompt, prompt= <<"">>, buff= <<"">>},
        NewState = change_acceptor_state(State, Acceptor, AccState),
        case data_store:read_telnet(username_prompt__) of
        [] ->
            ?error("Failed to find username_prompt__.");
        [Resp | _] ->
            telnet_server:send_msg(Acceptor, Resp)
        end;
    _ ->
        AccState = #acceptor_state{stage=new_conn, prompt= <<"">>, buff= <<"">>},
        NewState = change_acceptor_state(State, Acceptor, AccState),
        Pid = telnet_client:start(Config#config.remote_addr,
                                  Config#config.remote_port_telnet),
        ets:insert(a2c_table, {Acceptor, Pid}),
        ets:insert(c2a_table, {Pid, Acceptor})
    end,
    NewState.

handle_sim_command_response(State, Acceptor, Msg) ->
    AcceptorState = maps:get(Acceptor, State#state.acceptor_map),
    case AcceptorState#acceptor_state.stage of
    username_prompt ->
        NewAccState = AcceptorState#acceptor_state{stage=password_prompt},
        NewState = change_acceptor_state(State, Acceptor, NewAccState),
        PasswordPrompt = data_store:read_telnet(password_prompt__),
        telnet_server:send_msg(Acceptor, PasswordPrompt);
    password_prompt ->
        NewAccState = AcceptorState#acceptor_state{stage=run_command},
        NewState = change_acceptor_state(State, Acceptor, NewAccState),
        telnet_server:send_msg(Acceptor, get_system_prompt());
    run_command ->
        case binary_part(Msg, {byte_size(Msg), -2}) =:= <<"\r\n">> of
        true ->
            ValidMsg = string:trim(Msg);
        false ->
            case binary_part(Msg, {byte_size(Msg), -2}) =:= <<13,0>> of
            true ->
                ValidMsg = binary_part(Msg, {0, byte_size(Msg)-2});
            false ->
                ValidMsg = Msg
            end
        end,
        case data_store:read_telnet(ValidMsg) of
        [] ->
            ?error("Failed to find response. Cmd=~p", [ValidMsg]),
            SystemPrompt = get_system_prompt(),
            telnet_server:send_msg(Acceptor, <<"\r\nUnknown Input", SystemPrompt/binary>>);
        [Resp | _] ->
            telnet_server:send_msg(Acceptor, <<"\r\n", Resp/binary>>)
        end,
        NewState = State
    end,
    NewState.

msg_a2c(Config, State, Acceptor, Msg) ->
    ?info("telnet_proxy: send_msg_a2c(Acceptor=~p, Msg=~p)", [Acceptor, Msg]),
    case Config#config.mode of
    default ->
        [{_, Client} | _] = ets:lookup(a2c_table, Acceptor),
        ?info("Send from acceptor to client. Client=~p", [Client]),
        telnet_client:send_msg(Client, Msg),
        NewState = State;
    record ->
        case binary_part(Msg, {0,1}) =/= <<255>> of
        true ->
            case ets:lookup(telnet_record, Acceptor) of
            [] ->
                ets:insert(telnet_record, {Acceptor, Msg});
            [{_, PrevMsg} | _] ->
                ets:insert(telnet_record, {Acceptor, <<PrevMsg/binary, Msg/binary>>})
            end;
        false ->
            ok
        end,
        [{_, Client} | _] = ets:lookup(a2c_table, Acceptor),
        ?info("telnet_proxy: send_msg_a2c() - Found Client=~p", [Client]),
        telnet_client:send_msg(Client, Msg),
        NewState = State;
    sim ->
        case byte_size(Msg) >= 2 andalso binary_part(Msg, {0,2}) =:= <<255,251>> of
        true ->
            telnet_server:send_msg(Acceptor,
                <<255,251,1,255,251,3,255,253,24,255,253,31>>),
            telnet_server:send_msg(Acceptor,
                <<255,254,37,75,101,114,98,101,114,111,115,58,32,78,111,32,
                  100,101,102,97,117,108,116,32,114,101,97,108,109,32,100,
                  101,102,105,110,101,100,32,102,111,114,32,75,101,114,98,
                  101,114,111,115,33,13,10,255,250,24,1,255,240>>),
            telnet_server:send_msg(Acceptor,
                <<255,253,33,255,250,33,0,255,240>>),
            telnet_server:send_msg(Acceptor,
                <<255,254,34,255,254,39,255,252,5,255,254,35>>),
            NewState = State;
        false ->
            case byte_size(Msg) >= 2 andalso
                 (binary_part(Msg, {byte_size(Msg), -2}) =:= <<"\r\n">> orelse
                  binary_part(Msg, {byte_size(Msg), -2}) =:= <<13,0>>) of
            false ->
                telnet_server:send_msg(Acceptor, Msg),
                case ets:lookup(telnet_record, Acceptor) of
                [] ->
                    ets:insert(telnet_record, {Acceptor, Msg});
                [{_, PrevMsg} | _] ->
                    ets:insert(telnet_record, {Acceptor, <<PrevMsg/binary, Msg/binary>>})
                end,
                NewState = State;
            true ->
                case ets:lookup(telnet_record, Acceptor) of
                [] ->
                    NewMsg = Msg;
                [{_, PrevMsg} | _] ->
                    NewMsg = <<PrevMsg/binary, Msg/binary>>
                end,
                clear_input_buff(Acceptor),
                NewState = handle_sim_command_response(State, Acceptor, NewMsg)
            end
        end
    end,
    NewState.

is_input_submitted(Acceptor) ->
    case ets:lookup(telnet_record, Acceptor) of
    [] ->
        ?error("Failed to find the Command"),
        false;
    [{_, Command} | _] ->
        byte_size(Command) >= 2 andalso
        (binary_part(Command, {byte_size(Command), -2}) =:= <<"\r\n">> orelse
         binary_part(Command, {byte_size(Command), -2}) =:= <<13,0>>)
    end.

clear_input_buff(Acceptor) ->
    ets:delete(telnet_record, Acceptor).

handle_command_response(State, Acceptor, AccState, Msg) ->
    case ets:lookup(telnet_record, Acceptor) of
    [] ->
        ?error("Failed to find the Command of this response. Cmd=~p", [Msg]),
        NewState = State;
    [{_, Command} | _] ->
        case byte_size(Command) >= 2 andalso
             (binary_part(Command, {byte_size(Command), -2}) =:= <<"\r\n">> orelse
              binary_part(Command, {byte_size(Command), -2}) =:= <<13,0>>) of
        true ->
            StrippedPrompt = string:trim(AccState#acceptor_state.prompt),
            PromptSize = byte_size(StrippedPrompt),
            case byte_size(Msg) >= PromptSize andalso
                 binary_part(Msg, {byte_size(Msg), -PromptSize}) =:= StrippedPrompt of
            true ->
                FullMsg = <<(AccState#acceptor_state.buff)/binary, Msg/binary>>,
                NewAccState = AccState#acceptor_state{buff= <<"">>},
                NewState = change_acceptor_state(State, Acceptor, NewAccState),
                clear_input_buff(Acceptor),
                ValidCmd = string:trim(binary_part(Command, {0, byte_size(Command)-2})),
                ?info("Record command and response. Cmd=~p, Resp=~p",
                      [binary_to_list(ValidCmd), FullMsg]),
                data_store:write_telnet(ValidCmd, FullMsg);
            false ->
                NewBuff = <<(AccState#acceptor_state.buff)/binary, Msg/binary>>,
                NewAccState = AccState#acceptor_state{buff=NewBuff},
                NewState = change_acceptor_state(State, Acceptor, NewAccState)
            end;
        false ->
            NewState = State
        end
    end,
    NewState.

msg_c2a(Config, State, Client, Msg) ->
    ?info("telnet_proxy: send_msg_c2a(Client=~p, Msg=~p)", [Client, Msg]),
    [{_, Acceptor} | _] = ets:lookup(c2a_table, Client),
    ?info("telnet_proxy: send_msg_c2a() - Found Acceptor=~p", [Acceptor]),
    telnet_server:send_msg(Acceptor, Msg),

    case Config#config.mode of
    record ->
        % exclude telnet control commands
        % http://users.cs.cf.ac.uk/Dave.Marshall/Internet/node141.html
        case Msg =/= <<"\r\n">> andalso
             binary_part(Msg, {0,1}) =/= <<255>> of
        true ->
            AcceptorState = maps:get(Acceptor, State#state.acceptor_map),
            case AcceptorState#acceptor_state.stage of
            new_conn ->
                data_store:write_telnet(username_prompt__, Msg),
                NewAccState = AcceptorState#acceptor_state{stage=username_prompt},
                NewState = change_acceptor_state(State, Acceptor, NewAccState);
            username_prompt ->
                case is_input_submitted(Acceptor) of
                true ->
                    clear_input_buff(Acceptor),
                    case string:find(Msg, "timeout expired") of
                    nomatch ->
                        data_store:write_telnet(password_prompt__, Msg),
                        NewAccState = AcceptorState#acceptor_state{stage=password_prompt},
                        NewState = change_acceptor_state(State, Acceptor, NewAccState);
                    _ ->
                        NewState = State
                    end;
                false ->
                    NewState = State
                end;
            password_prompt ->
                case is_input_submitted(Acceptor) of
                true ->
                    clear_input_buff(Acceptor),
                    case string:find(Msg, "Login invalid") of
                    nomatch ->
                        data_store:write_telnet(system_prompt__, Msg),
                        NewAccState = AcceptorState#acceptor_state{stage=run_command, prompt=Msg},
                        NewState = change_acceptor_state(State, Acceptor, NewAccState);
                    _ ->
                        NewAccState = AcceptorState#acceptor_state{stage=username_prompt},
                        NewState = change_acceptor_state(State, Acceptor, NewAccState)
                    end;
                false ->
                    NewState = State
                end;
            run_command ->
                NewState = handle_command_response(State, Acceptor, AcceptorState, Msg)
            end;
        false ->
            NewState = State
        end;
    sim_record ->
        case ets:lookup(telnet_record, Acceptor) of
        [] ->
            data_store:write_telnet(after_conn__, Msg);
        [{_, Command} | _] ->
            data_store:write_telnet(Command, Msg)
        end,
        NewState = State;
    _ ->
        NewState = State
    end,
    NewState.

loop(State, Config) ->
    ?info("State=~p", [State]),
    receive
    {_From, {new_conn_nb, Acceptor}} ->
        NewState = handle_new_conn_nb(Config, State, Acceptor),
        loop(NewState, Config);

    {_From, {send_msg_a2c, Acceptor, Msg}} ->
        NewState = msg_a2c(Config, State, Acceptor, Msg),
        loop(NewState, Config);

    {_From, {send_msg_c2a, Client, Msg}} ->
        NewState = msg_c2a(Config, State, Client, Msg),
        loop(NewState, Config);

    _ -> loop(State, Config)
    end.
