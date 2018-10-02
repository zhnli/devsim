-module(http_proxy).
-behaviour(gen_server).

-export([
    start_link/1,
    handle_request/3,
    handle_request_with_body/8,
    handle_response/4,
    handle_response_with_body/4
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

%% ========================================
%% API
%% ========================================

start_link(Config) ->
    ?info("Starting HTTP Proxy."),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Config], []).

init([Parent, Config]) ->
    ?info("Initializing. Config=~p", [Config]),
    http_server:start_link(Config),
    http_client:start_link(Config),
    State = #state{parent=Parent, config=Config},
    {ok, State}.

handle_request(Handler, Req, Opts) ->
    ?info("Handle request"),
    gen_server:cast(http_proxy, {handle_request, Handler, Req, Opts}).

handle_request_with_body(Handler, Req, Opts, Method, URL, Headers, Payload, More) ->
    ?info("Handle request with body"),
    gen_server:cast(http_proxy, {
        handle_request_with_body, Handler, Req, Opts, Method, URL, Headers, Payload, More
    }).

handle_response(Handler, Req, Opts, Resp) ->
    ?info("Handle response"),
    gen_server:cast(http_proxy, {handle_response, Handler, Req, Opts, Resp}).

handle_response_with_body(Handler, Req, Opts, Resp) ->
    ?info("Handle response with body"),
    gen_server:cast(http_proxy, {handle_response_with_body, Handler, Req, Opts, Resp}).
        
%% ========================================
%% gen_server callbacks
%% ========================================

handle_call(E, _From, State) ->
    ?info("Recived unexpected call. Event=~p", [E]),
    {noreply, State}.

handle_cast({handle_request, Handler, Req, Opts}, State) ->
    ?info("Preparing request"),
    Method = convert_method(maps:get(method, Req)),
    Port = State#state.config#config.remote_port_http,
    URL = case maps:get(qs, Req) of
        {badkey, _Key} ->
            iolist_to_binary([
                list_to_binary("https://" ++ State#state.config#config.remote_addr),
                ?check(Port == 443, <<"">>, list_to_binary(":" ++ integer_to_list(Port))),
                maps:get(path, Req)
            ]);
        <<"">> ->
            iolist_to_binary([
                list_to_binary("https://" ++ State#state.config#config.remote_addr),
                ?check(Port == 443, <<"">>, list_to_binary(":" ++ integer_to_list(Port))),
                maps:get(path, Req)
            ]);
        Qs ->
            iolist_to_binary([
                list_to_binary("https://" ++ State#state.config#config.remote_addr),
                ?check(Port == 443, <<"">>, list_to_binary(":" ++ integer_to_list(Port))),
                maps:get(path, Req),
                <<"?">>,
                Qs
            ])
    end,

    Headers0 = maps:to_list(maps:get(headers, Req)),
    % Remove "host" header, it will trick hackney to send request to port 8443
    Headers = [{K, V} || {K, V} <- Headers0, K /= <<"host">>],
    case cowboy_req:has_body(Req) of
        true ->
            ?info("Fetching request body"),
            Handler ! {fetch_body, Opts, Method, URL, Headers};
        false ->
            http_client:send_request(Handler, Req, Opts, Method, URL, Headers, <<"">>)
    end,
    {noreply, State};
handle_cast({handle_request_with_body,
             Handler, Req, Opts, Method, URL, Headers, Payload, More}, State) ->
    http_client:send_request(Handler, Req, Opts, Method, URL, Headers, Payload),
    case More of
        true ->
            Handler ! {fetch_body, Opts, Method, URL, Headers};
        false ->
            ok
    end,
    {noreply, State};
handle_cast({handle_response, Handler, Req, Opts, Resp}, State) ->
    http_client:fetch_response_body(Handler, Req, Opts, Resp),
    {noreply, State};
handle_cast({handle_response_with_body, Handler, Req, Opts, Resp}, State) ->
    http_server:send_response(Handler, Req, Opts, Resp),
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
%% private functions
%% ========================================

convert_method(Method) ->
    case Method of
        <<"GET">> ->
            get;
        <<"POST">> ->
            post;
        <<"PUT">> ->
            put;
        <<"DELETE">> ->
            delete;
        <<"HEAD">> ->
            head;
        <<"CONNECT">> ->
            connect;
        <<"OPTIONS">> ->
            options;
        <<"TRACE">> ->
            trace
    end.
