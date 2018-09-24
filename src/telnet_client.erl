-module(telnet_client).
-export([start/2, send_msg/2]).
-include("devsim.hrl").
-record(client_state, {addr, port, socket, worker}).
-record(worker_state, {addr, port, socket, client}).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}]).

start(Addr, Port) ->
    spawn(fun() -> client_start(init(Addr, Port)) end).

    % register(telent_client,
    %          spawn(fun() -> server_loop(init(Addr, Port)) end)).

init(Addr, Port) ->
    ?info("Telnet connecting to ~p:~p", [Addr, Port]),
    {ok, Socket} = gen_tcp:connect(Addr, Port, ?TCP_OPTIONS),
    WState = #worker_state{addr=Addr, port=Port, socket=Socket},
    Pid = spawn(fun() -> worker_start(WState) end),
    #client_state{addr=Addr, port=Port, socket=Socket, worker=Pid}.

client_start(State=#client_state{worker=Worker}) ->
    Worker ! {assign_client, self()},
    client_loop(State).

client_loop(State=#client_state{socket=Socket, worker=_Worker}) ->
    receive
        {send_msg, Msg} ->
            gen_tcp:send(Socket, Msg),
            client_loop(State);
        {received_msg, Msg} ->
            telnet_proxy:send_msg_c2a(self(), Msg),
            client_loop(State);
        close ->
            gen_tcp:close(Socket);
        _ ->
            client_loop(State)
    end.

% worker_main(WState=#worker_state{socket=Socket}) ->
%     {ok, Socket} = gen_tcp:accept(Socket),
%     spawn(fun() -> worker_loop(Socket, []) end),
%     worker_main(WState).

worker_start(WState) ->
    worker_loop(WState).

worker_loop(WState) ->
    receive
        {assign_client, Client} ->
            recv_loop(WState#worker_state{client=Client}, []);
        _ ->
            worker_loop(WState)
    end.

% recv_loop(WState=#worker_state{socket=Socket, client=Client}, Bs) ->
    % case gen_tcp:recv(Socket, 0) of
    %     {ok, B} ->
    %         recv_loop(WState, [Bs, B]);
    %     {error, closed} ->
    %         Client ! {received_msg, list_to_binary(Bs)}
    %         % {ok, list_to_binary(Bs)}
    % end.

recv_loop(WState=#worker_state{socket=Socket, client=Client}, _Bs) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Client ! {received_msg, Data},
            recv_loop(WState, []);
        {error, closed} ->
            ok
    end.

send_msg(Client, Msg) ->
    Client ! {send_msg, Msg}.

% client() ->
%     SomeHostInNet = "localhost", % to make it runnable on one machine
%     {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5678,
%                                  [binary, {packet, 0}]),
%     ok = gen_tcp:send(Sock, "Some Data"),
%     ok = gen_tcp:close(Sock).