-module(snmp_proxy).
-behaviour(gen_server).

-export([start_link/1, handle_request/3, handle_response/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("devsim.hrl").
-include("log.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-define(SERVER,   ?MODULE).
-define(USER,     ?MODULE).
-define(USER_MOD, ?MODULE).

-record(state, {parent, config}).

%% ========================================
%% API
%% ========================================

start_link(Config) ->
    ?info("Starting SNMP Proxy."),
    % {ok, State} = init(Config),
    % Pid = spawn(fun() -> loop(State, Config) end),
    % register(?MODULE, Pid),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Config], []).
    % {ok, Pid}.

init([Parent, Config]) ->
    ?info("Initializing. Config=~p", [Config]),
    % snmp_mgr:start(Config#config.remote_addr, Config#config.remote_port_snmp),
    snmp_agent:start_link(Config),
    snmp_mgr:start_link(Config),
    % ets:new(a2m_table, [set, named_table, public]),
    % ets:new(m2a_table, [set, named_table, public]),
    State = #state{parent=Parent, config=Config},
    {ok, State}.

% loop(State, Config) ->
%     receive
        % {_From, {new_conn_nb, Acceptor}} ->
        %     Pid = snmp_agent:start(Config#config.remote_addr,
        %                            Config#config.remote_port),
        %     ets:insert(a2c_table, {Acceptor, Pid}),
        %     ets:insert(c2a_table, {Pid, Acceptor}),
        %     % dump ets tables here
        %     io:format("a2c_table: ~p~n", dump_table(a2c_table)),
        %     io:format("c2a_table: ~p~n", dump_table(c2a_table));

    %     _ -> loop(State, Config)
    % end.

handle_request(Proxy, Cntx, Pdu) ->
    Proxy ! {handle_request, Cntx, Pdu}.

handle_response(Proxy, Cntx, Resp) ->
    Proxy ! {handle_response, Cntx, Resp}.

%% ========================================
%% gen_server callbacks
%% ========================================

handle_call(E, _From, State) ->
    ?info("Recived unexpected call. Event=~p", [E]),
    {noreply, State}.

handle_cast(E, State) ->
    ?info("Recived unexpected cast. Event=~p", [E]),
    {noreply, State}.

handle_info({handle_request, Cntx, Pdu}, State) ->
    #snmp_cntx{from=From, version=Ver, community=Community} = Cntx,
    ?info("Recived request. From=~p, Version=~p, Community=~p, Pdu=~p", [From, Ver, Community, Pdu]),
    process_request(Cntx, Pdu),
    % case (catch snmp_pdus:dec_pdu(PduRaw)) of
    %     Pdu when is_record(Pdu, pdu) ->
    %         ?info("Decoded PDU. PDU=~p", [Pdu]);
    %     {'EXIT', Reason} ->
	%         ?info("PDU decode exit: ~p",[Reason]);
	%         % {discarded, Reason};
	%     _TrapPdu ->
    %         ?info("Trap PDU recived")
    %         % {discarded, trap_pdu}
    % end,
    {noreply, State};
handle_info({handle_response, Cntx, Resp}, State) ->
    ?info("Recived response. Resp=~p", [Resp]),
    process_response(Cntx, Resp),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    ?info("Terminated. Reason=~p", [_Reason]).

%% ========================================
%% Internal Funcs
%% ========================================

process_request(Cntx, Pdu) ->
    Type = Pdu#pdu.type,
    ReqId = Pdu#pdu.request_id,
    Varbinds = Pdu#pdu.varbinds,
    NewCntx = Cntx#snmp_cntx{req_id=ReqId},

    case Type of
        'get-request' ->
            ?info("Received get-request"),
            Oids = lists:map(fun(V) -> V#varbind.oid end, Varbinds),
            ?info("ReqId=~p, Oids=~p", [ReqId, Oids]),
            snmp_mgr:send_get_req(NewCntx, Oids);
        'get-next-request' ->
            ?info("Received get-next-request"),
            Oids = lists:map(fun(V) -> V#varbind.oid end, Varbinds),
            ?info("ReqId=~p, Oids=~p", [ReqId, Oids]),
            snmp_mgr:send_get_next_req(NewCntx, Oids);
        'get-bulk-request' ->
            ?info("Received get-bulk-request"),
            Oids = lists:map(fun(V) -> V#varbind.oid end, Varbinds),
            NonRep = Pdu#pdu.error_status,
            MaxRep = Pdu#pdu.error_index,
            ?info("ReqId=~p, NonRep=~p, MaxRep=~p, Oids=~p", [ReqId, NonRep, MaxRep, Oids]),
            snmp_mgr:send_get_bulk_req(NewCntx, NonRep, MaxRep, Oids);
        'get-response' ->
            ?info("Received get-response");
        'set-request' ->
                ?info("Received get-request");
        'inform-request' ->
            ?info("Received inform-request");
        'snmpv2-trap' ->
            ?info("Received snmpv2-trap");
        '_' ->
            ?info("Received unknown request")
    end.

process_response(Cntx, Resp) ->
    {ErrSt, ErrIdx, Varbinds} = Resp,
    Pdu = #pdu{type='get-response',
               request_id=Cntx#snmp_cntx.req_id,
               error_status=ErrSt,
               error_index=ErrIdx,
               varbinds=Varbinds},
    snmp_agent:send_response(Cntx, Pdu).
