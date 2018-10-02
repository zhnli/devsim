-module(telnet_proxy).
-export([start/1, loop/2, new_conn_nb/1, send_msg_a2c/2, send_msg_c2a/2]).

-include("devsim.hrl").
-include("log.hrl").

-record(state, {s2c_table, c2s_table}).

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
    State = #state{},
    {ok, State}.

new_conn_nb(Acceptor) ->
    ?info("telnet_proxy: new_conn_nb()"),
    ?MODULE ! {self(), {new_conn_nb, Acceptor}}.

send_msg_a2c(Acceptor, Msg) ->
    ?info("telnet_proxy: send_msg_a2c(Acceptor=~p, Msg=~p)", [Acceptor, Msg]),
    [{_, Client} | _] = ets:lookup(a2c_table, Acceptor),
    ?info("telnet_proxy: send_msg_a2c() - Found Client=~p", [Client]),
    telnet_client:send_msg(Client, Msg).

send_msg_c2a(Client, Msg) ->
    ?info("telnet_proxy: send_msg_c2a(Client=~p, Msg=~p)", [Client, Msg]),
    [{_, Acceptor} | _] = ets:lookup(c2a_table, Client),
    ?info("telnet_proxy: send_msg_c2a() - Found Acceptor=~p", [Acceptor]),
    telnet_server:send_msg(Acceptor, Msg).

loop(State, Config) ->
    receive
        {_From, {new_conn_nb, Acceptor}} ->
            ?info("Received new_conn_nb"),
            Pid = telnet_client:start(Config#config.remote_addr,
                                      Config#config.remote_port_telnet),
            ets:insert(a2c_table, {Acceptor, Pid}),
            ets:insert(c2a_table, {Pid, Acceptor}),
            % dump ets tables here
            % ?info("a2c_table: ~p", dump_table(a2c_table)),
            % ?info("c2a_table: ~p", dump_table(c2a_table)),
            loop(State, Config);

        _ -> loop(State, Config)
    end.

% dump_table(Table) ->
%     ets:match_object(Table, {'$0', '$1'}).
