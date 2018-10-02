-module(http_server).
-behaviour(gen_server).

-export([start_link/1, send_response/4]).

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
    ?info("Starting HTTP Server."),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Config], []).

init([Parent, Config]) ->
    ?info("Initializing. Config=~p", [Config]),
    Dispatch = cowboy_router:compile([
		{'_', [
			{'_', http_handler, []}
		]}
    ]),
    {ok, CWD} = file:get_cwd(),
    PrivDir = CWD ++ "/priv",

    % For release build.
    % case code:priv_dir(devsim) of
    %     {error, bad_name} ->
    %         {ok, CWD} = file:get_cwd(),
    %         PrivDir = CWD ++ "/priv/";
    %     PrivDir ->
    %         ok
    % end,
    
    {ok, _} = cowboy:start_tls(
        my_http_listener,
        [
            {port, 8443},
            {certfile, PrivDir ++ "/" ++ Config#config.server_cert},
            {keyfile, PrivDir ++ "/" ++ Config#config.priv_key}
        ],
        #{
            env => #{dispatch => Dispatch}%,
            % stream_handlers => [cowboy_compress_h, cowboy_stream_h]
        }
    ),

    % To start HTTP server.
    % {ok, _} = cowboy:start_clear(my_http_listener,
    %     [{port, 8080}],
    %     #{env => #{dispatch => Dispatch}}
    % ),

    State = #state{parent=Parent, config=Config},
    {ok, State}.

send_response(Handler, Req, Opts, Resp) ->
    ?info("Send response"),
    gen_server:cast(http_server, {send_response, Handler, Req, Opts, Resp}).

%% ========================================
%% gen_server callbacks
%% ========================================

handle_call(E, _From, State) ->
    ?info("Recived unexpected call. Event=~p", [E]),
    {noreply, State}.

handle_cast({send_response, Handler, _Req, _Opts, Resp}, State) ->
    Handler ! {reply, Resp},
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
