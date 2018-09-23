-module(telnet_server).
-export([start/1, send_msg/2]).

-record(server_state, {port, worker, config}).
-record(worker_state, {port, socket}).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    register(telnet_server, spawn(fun() -> server_loop(init(Port)) end)).

init(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    WState = #worker_state{port=Port, socket=LSocket},
    Pid = spawn(fun() -> acceptor_loop(WState) end),
    #server_state{port=Port, worker=Pid}.

server_loop(SState) ->
    receive
        _ -> server_loop(SState)
    end.

acceptor_loop(WState=#worker_state{socket=LSocket}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Worker = spawn(fun() -> worker_start(Socket) end),
    telnet_proxy:new_conn_nb(Worker),
    acceptor_loop(WState).

worker_start(Socket) ->
    Pid = self(),
    spawn(fun() -> recv_loop(Pid, Socket) end),
    worker_loop(Socket).

worker_loop(Socket) ->
    receive
        {send_msg, Msg} ->
            io:format("=> ~w:~w - send msg=~p~n", [?MODULE, ?LINE, Msg]),
            gen_tcp:send(Socket, Msg),
            worker_loop(Socket);
        {received_msg, Msg} ->
            io:format("=> ~w:~w - received data=~p~n", [?MODULE, ?LINE, Msg]),
            telnet_proxy:send_msg_a2c(self(), Msg),
            worker_loop(Socket);
        _ ->
            io:format("=> ~w:~w - unknown msg~n", [?MODULE, ?LINE]),
            worker_loop(Socket)
    end.

recv_loop(Worker, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            % gen_tcp:send(Socket, Data),
            io:format("=> telnet_server: recv_loop() - receive data=~p~n", [Data]),
            Worker ! {received_msg, Data},
            recv_loop(Worker, Socket);
        {error, closed} ->
            ok
    end.

send_msg(Acceptor, Msg) ->
    Acceptor ! {send_msg, Msg}.
