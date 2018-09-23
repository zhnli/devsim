-module(snmp_agent).

-behaviour(gen_server).

-export([start_link/1, send_response/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("log.hrl").
-include("devsim.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-define(SERVER,   ?MODULE).
-define(USER,     ?MODULE).
-define(USER_MOD, ?MODULE).

-record(state, {parent, socket, addr, port, comm, user, pwd, timer}).

%% ========================================
%% API
%% ========================================

start_link(Config) ->
    ?info("Starting SNMP Agent. Config=~p", [Config]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Config], []).

init([Parent, Config]) ->
    ?info("SNMP Agent init", []),
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
    {ok, Socket} = gen_udp:open(Config#config.local_port_snmp, [binary]),
    {ok, #state{socket=Socket,
                addr=Config#config.remote_addr,
                port=Config#config.remote_port_snmp,
                comm=Config#config.snmp_community,
                user=Config#config.snmp_user,
                pwd=Config#config.snmp_pwd}}.

send_response(Cntx, Pdu) ->
    gen_server:cast(snmp_agent, {send_response, Cntx, Pdu}).

%% ========================================
%% gen_server callbacks
%% ========================================

handle_call(E, _From, State) ->
    ?info("Recived unexpected call. Event=~p", [E]),
    {noreply, State}.

handle_cast({send_response, Cntx, Pdu}, State) ->
    ?info("Recived send_response cast. Pdu=~p", [Pdu]),
    PduBytes = snmp_pdus:enc_pdu(Pdu),
    Message = #message{version   = Cntx#snmp_cntx.version, 
				       community = Cntx#snmp_cntx.community, 
                       data      = PduBytes},
    case catch list_to_binary(snmp_pdus:enc_message_only(Message)) of
		{'EXIT', Reason} ->
            ?info("Failed to encode messsage. Reason=~p", [Reason]);	
		Packet ->
            ?info("EncodedPdu=~p", [Packet]),
            From = Cntx#snmp_cntx.from,
            gen_udp:send(From#msg_from.sock, From#msg_from.addr, From#msg_from.port, Packet)
    end,
    {noreply, State};
handle_cast(E, State) ->
    ?info("Recived unexpected cast. Event=~p", [E]),
    {noreply, State}.

handle_info(timeout, State) ->
    ?info("Recived timeout info.", []),
    {noreply, State};
handle_info({udp, Socket, Addr, Port, Packet}, State) ->
    ?info("Recived UDP packet. Addr=~p, Packet=~p", [Addr, Packet]),
    case catch snmp_pdus:dec_message_only(binary_to_list(Packet)) of
        #message{version = Ver, community = Community, data = Data} ->
            ?info("Decoded packet. Ver=~p, Community=~p, Data=~p", [Ver, Community, Data]),
            Cntx = #snmp_cntx{from=#msg_from{addr=Addr, port=Port, sock=Socket},
                              version=Ver,
                              community = Community},
            case (catch snmp_pdus:dec_pdu(Data)) of
                Pdu when is_record(Pdu, pdu) ->
                    ?info("Decoded PDU. PDU=~p", [Pdu]),
                    snmp_proxy:handle_request(State#state.parent, Cntx, Pdu);
                {'EXIT', Reason} ->
	                ?info("PDU decode exit: ~p",[Reason]);
	                % {discarded, Reason};
	            _TrapPdu ->
                    ?info("Trap PDU recived")
                    % {discarded, trap_pdu}
            end;
            % Pdu = snmp_pdus:dec_pdu(Data),
            % case (catch snmp_pdus:dec_pdu(Data)) of
            % Pdu = snmp_pdus:dec_pdu(Data),
            % ?info("Decoded PDU. PDU=~p", [Pdu]);

        {'EXIT', {bad_version, Vsn}} ->
	        ?info("exit: bad version: ~p",[Vsn]);

	    {'EXIT', Reason} ->
	        ?info("exit: ~p", [Reason]);

	    UnknownMessage ->
	        ?info("Unknown message: ~n   ~p"
	    	"~nwhen"
	    	"~n   State: ~p", [UnknownMessage, State])
    end,
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
