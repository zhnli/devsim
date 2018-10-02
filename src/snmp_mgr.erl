-module(snmp_mgr).

-behaviour(gen_server).
-behaviour(snmpm_user).

-export([start_link/1, send_get_req/2,
         send_get_next_req/2, send_get_bulk_req/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% Manager callback API:
-export([handle_error/3,
         handle_agent/5,
         handle_pdu/4,
         handle_trap/3,
         handle_inform/3,
         handle_report/3, 
	     handle_invalid_result/3]).

-include("devsim.hrl").

-define(SERVER,   ?MODULE).
-define(USER,     ?MODULE).
-define(USER_MOD, ?MODULE).

-record(state, {parent, addr, port, comm, user, pwd, timer}).

%% ========================================
%% API
%% ========================================

start_link(Config) ->
    ?info("Starting SNMP manager. Config=~p", [Config]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Config], []).

init([Parent, Config]) ->
    process_flag(trap_exit, true),
    case (catch do_init(Config)) of
        {ok, State} ->
            {ok, State#state{parent = Parent}};
        {error, Reason} ->
            {stop, Reason};
	    Unexpected ->
	        {stop, Unexpected}
    end.

do_init(Config) ->
    ?info("SNMP manager init."),
    Opts = [std_mib("STANDARD-MIB")],
    {Dir, MgrConf, MgrOpts} = parse_opts(Opts),
    % snmp:start(),
    write_config(Dir, MgrConf),
    ok = start_manager(MgrOpts),
    ok = register_user(),
    {ok, AddrTup} = inet:parse_address(Config#config.remote_addr),
    Addr = tuple_to_list(AddrTup),
    ?info("Addr=~p", [Addr]),
    ok = register_agent(Addr,
                        Config#config.remote_port_snmp,
                        Config#config.snmp_community),

    Timer = erlang:send_after(3000, self(), timer),

    {ok, #state{addr=Addr,
                port=Config#config.remote_port_snmp,
                comm=Config#config.snmp_community,
                timer=Timer}}.

write_config(Dir, Conf) ->
    case snmp_config:write_manager_config(Dir, "", Conf) of
	    ok ->
	        ok;
	    Error ->
	        error({failed_writing_config, Error})
    end.

start_manager(Opts) ->
    ?info("Start manager Opts=~p", [Opts]),
    case snmpm:start_link(Opts) of
	    ok ->
	        ok; 
	    Error ->
	        error({failed_starting_manager, Error})
    end.

register_user() ->
    case snmpm:register_user(?USER, ?USER_MOD, self()) of
	    ok ->
	        ok;
	    Error ->
	        error({failed_register_user, Error})
    end.

register_agent(Addr, Port, Comm) ->
    Opts = [{engine_id, "snmp_engine"},
            {address,   Addr},
            {port,      Port},
            {community, Comm},
            {version,   v2},
            {sec_model, v2c}],
    snmpm:register_agent(?USER, Addr, Opts).
    % snmpm:register_agent(User, Pwd, Opts).

send_get_req(Cntx, Oids) ->
    gen_server:cast(snmp_mgr, {send_get_req, Cntx, Oids}).

send_get_next_req(Cntx, Oids) ->
    gen_server:cast(snmp_mgr, {send_get_next_req, Cntx, Oids}).

send_get_bulk_req(Cntx, NonRep, MaxRep, Oids) ->
    gen_server:cast(snmp_mgr, {send_get_bulk_req, Cntx, NonRep, MaxRep, Oids}).

% prepare(Version, Address, Community) ->
%     Options = [
%         {engine_id, "simple_snmp_engine"},
%         {community, Community},
%         {version, Version}
%         |
%         case Address of
%             {Host, Port} ->
%                 [
%                     {address, Host},
%                     {port, Port}
%                 ];
%             Host ->
%                 {ok, HostAddress} = inet:parse_address(Host),
%                 [{address, HostAddress}]
%         end
%     ],
%     case snmpm:which_agents(simple_snmp_user) of
%         [] -> ok;
%         _ ->
%             snmpm:unregister_agent(simple_snmp_user, "simple_snmp_user")
%     end,
%     snmpm:register_agent(simple_snmp_user, "simple_snmp_user", Options).

%% ========================================================================
%% SNMPM user callback functions
%% ========================================================================

handle_error(ReqId, Reason, Server) when is_pid(Server) ->
    report_callback(Server, handle_error, {ReqId, Reason}),
    ignore.


handle_agent(Addr, Port, Type, SnmpInfo, Server) when is_pid(Server) ->
    report_callback(Server, handle_agent, {Addr, Port, Type, SnmpInfo}),
    ignore.


handle_pdu(TargetName, ReqId, SnmpResponse, Server) when is_pid(Server) ->
    report_callback(Server, handle_pdu, {TargetName, ReqId, SnmpResponse}),
    ignore.


handle_trap(TargetName, SnmpTrap, Server) when is_pid(Server) ->
    report_callback(Server, handle_trap, {TargetName, SnmpTrap}),
    ok.

handle_inform(TargetName, SnmpInform, Server) when is_pid(Server) ->
    report_callback(Server, handle_inform, {TargetName, SnmpInform}),
    ok.


handle_report(TargetName, SnmpReport, Server) when is_pid(Server) ->
    report_callback(Server, handle_inform, {TargetName, SnmpReport}),
    ok.

handle_invalid_result(In, Out, Server) when is_pid(Server) ->
    report_callback(Server, handle_invalid_result, {In, Out}),
    ok.

report_callback(Pid, Tag, Info) ->
    Pid ! {snmp_callback, Tag, Info}.

%% ========================================
%% gen_server callbacks
%% ========================================

handle_call(E, _From, State) ->
    ?info("Recived unexpected call. Event=~p", [E]),
    {noreply, State}.

% handle_cast({get_req, Oids}, State) ->
%     ?info("Recived cast. Event=~p", [{get_req, Oids}]),
%     % {ok, Reply, _Remaining} = snmpm:sync_get(State#state.user, State#state.pwd, Oids),
%     {ok, Reply, _Remaining} = snmpm:sync_get(?USER, State#state.addr, Oids),
%     ?info("Recived getRequest response. Resp=~p", [Reply]),
%     {noreply, State};
handle_cast({send_get_req, Cntx, Oids}, State) ->
    ?info("Recived cast. Event=~p", [{send_get_req, Oids}]),
    % {ok, Reply, _Remaining} = snmpm:sync_get(?USER, State#state.addr, Oids),
    case snmpm:sync_get(?USER, State#state.addr, Oids) of
        {ok, Reply, _Remaining} ->
            ?info("Recived get_req response. Resp=~p", [Reply]),
            snmp_proxy:handle_response(State#state.parent, Cntx, Reply);
        {error, {send_failed, _X, tooBig}} ->
            ?info("Recived get_req response failed. Error=tooBig"),
            snmp_proxy:handle_response(State#state.parent, Cntx, {tooBig, 0, []})
    end,
    {noreply, State};
handle_cast({send_get_next_req, Cntx, Oids}, State) ->
    ?info("Recived cast. Event=~p", [{send_get_next_req, Oids}]),
    % {ok, Reply, _Remaining} = snmpm:sync_get_next(?USER, State#state.addr, Oids),
    case snmpm:sync_get_next(?USER, State#state.addr, Oids) of
        {ok, Reply, _Remaining} ->
            ?info("Recived get_next_req response. Resp=~p", [Reply]),
            snmp_proxy:handle_response(State#state.parent, Cntx, Reply);
        {error, {send_failed, _X, tooBig}} ->
            ?info("Recived get_next_req response failed. Error=tooBig"),
            snmp_proxy:handle_response(State#state.parent, Cntx, {tooBig, 0, []})
    end,
    {noreply, State};
handle_cast({send_get_bulk_req, Cntx, NonRep, MaxRep, Oids}, State) ->
    ?info("Recived cast. Event=~p", [{send_get_next_req, Oids}]),
    % {ok, Reply, _Remaining} = snmpm:sync_get_bulk(?USER, State#state.addr, NonRep, MaxRep, Oids),
    case snmpm:sync_get_bulk(?USER, State#state.addr, NonRep, MaxRep, Oids) of
        {ok, Reply, _Remaining} ->
            ?info("Recived get_bulk_req response. Resp=~p", [Reply]),
            snmp_proxy:handle_response(State#state.parent, Cntx, Reply);
        {error, {send_failed, _X, tooBig}} ->
            ?info("Recived get_bulk_req response failed. Error=tooBig"),
            snmp_proxy:handle_response(State#state.parent, Cntx, {tooBig, 0, []})
    end,
    {noreply, State};
handle_cast(E, State) ->
    ?info("Recived unexpected cast. Event=~p", [E]),
    {noreply, State}.

handle_info(timer, State) ->
    ?info("Recived timer info."),
    % get_req([[1,3,6,1,2,1,1,5,0]]),
    {noreply, State};
handle_info(E, State) ->
    ?info("Recived unexpected info. Event=~p", [E]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    ?info("SNMP Manager terminated. Reason=~p", [_Reason]).

%% ========================================
%% Internal utility functions
%% ========================================

parse_opts(Opts) ->
    Port     = get_opt(port,             Opts, 5172), % Changed from 5000 to avoid clashing
    EngineId = get_opt(engine_id,        Opts, "mgrEngine"),
    MMS      = get_opt(max_message_size, Opts, 484),

    MgrConf = [{port,             Port},
               {engine_id,        EngineId},
               {max_message_size, MMS}],

    %% Manager options
    Mibs      = get_opt(mibs,     Opts, []),
    Vsns      = get_opt(versions, Opts, [v1, v2, v3]),
    {ok, Cwd} = file:get_cwd(),
    Dir       = get_opt(dir, Opts, Cwd),
    MgrOpts   = [{mibs,     Mibs},
		         {versions, Vsns}, 
		         {server,   [{verbosity, trace}]}, 
		         {config,   [{verbosity, trace}, 
			     {dir, Dir}, {db_dir, Dir}]}],
    
    {Dir, MgrConf, MgrOpts}.

get_opt(Key, Opts, Def) ->
    case lists:keyfind(Key, 1, Opts) of
        {value, {Key, Val}} ->
            Val;
        false ->
            Def
    end.

std_mib(MibName) ->
    join(std_dir(), MibName).

std_dir() -> join(code:priv_dir(snmp), "mibs").

join(Dir, Filename) ->
    filename:join(Dir, Filename).
