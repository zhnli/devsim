-module(data_store).
-behaviour(gen_server).

-export([
    start_link/1,
    read_telnet/1,
    read_snmp/1,
    write_telnet/2,
    write_snmp/2
]).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, code_change/3, terminate/2
]).

-include("devsim.hrl").

-define(SERVER,   ?MODULE).
-define(USER,     ?MODULE).
-define(USER_MOD, ?MODULE).

-record(state, {parent, config}).
-record(devsim_telnet, {command, result}).
-record(devsim_snmp, {request, response}).

%% ========================================
%% API
%% ========================================

start_link(Config) ->
    ?info("Starting Data Store."),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Config], []).

init([Parent, Config]) ->
    ?info("Initializing. Config=~p", [Config]),
    setup_db([node()]),
    State = #state{parent=Parent, config=Config},
    {ok, State}.

setup_db(Nodes) ->
    ?info("Setting up DB."),
    case mnesia:create_schema(Nodes) of
    ok ->
        ?info("Created DB schema.");
    {error, Reason} ->
        ?info("Schema already exists. Reason=~p", [Reason])
    end,

    rpc:multicall(Nodes, application, start, [mnesia]),
    % mnesia:start(),
    ?info("Mnesia started."),
    case mnesia:create_table(devsim_telnet,
        [{attributes, record_info(fields, devsim_telnet)},
         {disc_copies, Nodes}]) of
    {atomic, ok} ->
        ?info("Created table devsim_telnet.");
    {already_exists, _Table_name} ->
        ?info("Found table devsim_telnet.");
    {aborted, Reason1} ->
        ?error("Table devsim_telnet NOT created. Reason=~p", [Reason1])
    end,
    case mnesia:create_table(devsim_snmp,
        [{attributes, record_info(fields, devsim_snmp)},
         {disc_copies, Nodes}]) of
    {atomic, ok} ->
        ?info("Created table devsim_snmp.");
    {already_exists, _Table_name1} ->
        ?info("Found table devsim_snmp.");
    {aborted, Reason2} ->
        ?error("Table devsim_snmp NOT created. Reason=~p", [Reason2])
    end.

read_telnet(Command) ->
    ?info("Read Telnet from DB. Command=~p", [Command]),
    gen_server:call(data_store, {read_telnet, Command}).

read_snmp(Request) ->
    ?info("Read SNMP from DB. Request=~p, Re=~p", [Request]),
    gen_server:call(data_store, {read_snmp, Request}).

write_telnet(Command, Result) ->
    ?info("Write Telnet into DB. Command=~p, Result=~p", [Command, Result]),
    gen_server:call(data_store, {write_telnet, Command, Result}).

write_snmp(Request, Response) ->
    ?info("Write SNMP into DB. Command=~p, Result=~p", [Request, Response]),
    gen_server:call(data_store, {write_snmp, Request, Response}).

%% ========================================
%% gen_server callbacks
%% ========================================

handle_call({read_telnet, Command}, _From, State) ->
    F = fun() ->
        case mnesia:read({devsim_telnet, Command}) of
            [#devsim_telnet{result=Res}] ->
                [Res];
            [] ->
                []
        end
    end,
    R = mnesia:activity(transaction, F),
    {reply, R, State};
handle_call({read_snmp, Request}, _From, State) ->
    F = fun() ->
        mnesia:read({devsim_snmp, Request})
    end,
    R = mnesia:activity(transaction, F),
    {reply, R, State};
handle_call({write_telnet, Command, Result}, _From, State) ->
    F = fun() ->
        mnesia:write({devsim_telnet, Command, Result})
    end,
    R = mnesia:activity(transaction, F),
    {reply, R, State};
handle_call({write_snmp, Request, Response}, _From, State) ->
    F = fun() ->
        mnesia:write({devsim_snmp, Request, Response})
    end,
    R = mnesia:activity(transaction, F),
    {reply, R, State};
handle_call(E, _From, State) ->
    ?info("Recived unexpected call. Event=~p", [E]),
    {noreply, State}.

handle_cast(E, State) ->
    ?info("Recived unexpected cast. Event=~p", [E]),
    {noreply, State}.

handle_info(E, State) ->
    ?info("Recived unexpected info. Event=~p", [E]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    ?info("Terminated. Reason=~p", [_Reason]).

%% ========================================
%% private functions
%% ========================================
