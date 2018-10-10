-module(http_client).
-behaviour(gen_server).

-export([start_link/1, send_request/7, fetch_response_body/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("devsim.hrl").

-define(SERVER,   ?MODULE).
-define(USER,     ?MODULE).
-define(USER_MOD, ?MODULE).

-record(state, {parent, config}).

%% ========================================
%% API
%% ========================================

start_link(Config) ->
    ?info("Starting HTTP Client."),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Config], []).

init([Parent, Config]) ->
    ?info("Initializing. Config=~p", [Config]),
    State = #state{parent=Parent, config=Config},
    {ok, State}.

send_request(Handler, Req, Opts, Method, URL, Headers, Payload) ->
    ?info("Send request"),
    gen_server:cast(http_client, {send_request, Handler, Req, Opts, Method, URL, Headers, Payload}).

fetch_response_body(Handler, Req, Opts, {StatusCode, RespHeaders, Ref}) ->
    gen_server:cast(http_client, {fetch_response_body, Handler, Req, Opts, {StatusCode, RespHeaders, Ref}}).

%% ========================================
%% gen_server callbacks
%% ========================================

handle_call(E, _From, State) ->
    ?info("Recived unexpected call. Event=~p", [E]),
    {noreply, State}.

handle_cast({send_request, Handler, Req, Opts, Method, URL, Headers, Payload}, State) ->
    Options = [
        {follow_redirect, true},   {max_redirect, 5},
        {timeout, 1800000},        {max_connections, 100},
        {insecure, true}
    ],
    ?info("Sending request. Method=~p, URL=~p, Headers=~p, Payload=~p",
        [Method, URL, Headers, Payload]),
    case hackney:request(Method, URL, Headers, Payload, Options) of
        {ok, StatusCode, RespHeaders, ClientRef} ->
            ?info("Got response. Status=~p, Headers=~p", [StatusCode, RespHeaders]),
            % Do not read whole body here, stream it later instead.
            % {ok, Body} = hackney:body(ClientRef),
            http_proxy:handle_response(Handler, Req, Opts, {StatusCode, RespHeaders, ClientRef});
        {ok, StatusCode, RespHeaders} ->
            ?info("Got response without Ref. Status=~p, Headers=~p", [StatusCode, RespHeaders]),
            http_proxy:handle_response_with_body(Handler, Req, Opts, {StatusCode, RespHeaders, undefined, <<"">>});
        {error, Reason} ->
            ?info("Failed to send request. Error=~p", [Reason])
    end,
    {noreply, State};
handle_cast({fetch_response_body, Handler, Req, Opts, {StatusCode, RespHeaders, Ref}}, State) ->
    case read_body(unlimited, Ref, <<"">>) of
        {ok, Data} ->
            http_proxy:handle_response_with_body(
                Handler, Req, Opts, {StatusCode, RespHeaders, Ref, Data});
        {error, Reason} ->
            ?info("Failed to read response body. Error=~p", [Reason])
    end,
    {noreply, State};
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
%% Private Functions
%% ========================================

read_body(MaxLength, Ref, Acc) when MaxLength == unlimited; MaxLength > byte_size(Acc) ->
	case hackney:stream_body(Ref) of
		{ok, Data} ->
			read_body(MaxLength, Ref, << Acc/binary, Data/binary >>);
		done ->
			{ok, Acc};
		{error, Reason} ->
			{error, Reason}
    end.
