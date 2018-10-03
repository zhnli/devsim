-module(http_handler).

-include("devsim.hrl").

-export([init/2, info/3, terminate/3]).

init(Req, State) ->
	?info("Received request. Req=~p, State=~p", [Req, State]),
	http_proxy:handle_request(self(), Req, State),
	{cowboy_loop, Req, State, hibernate}.

info({fetch_body, Opts, Method, URL, Headers}, Req0, State) ->
	?info("Fetch request body"),
	{ok, Req, Data} = read_req_body(unlimited, Req0, <<"">>),
	http_proxy:handle_request_with_body(
		self(), Req, Opts, Method, URL, Headers, Data, false),
	{ok, Req, State};
info({reply, Resp}, Req0, State) ->
	?info("Received response. Resp=~p", [Resp]),
	{StatusCode, RespHeaders, _Ref, Body} = Resp,

	% Remove header ""Transfer-Encoding". We do not stream chunked payloads yet.
	Headers0 = maps:from_list([
		{string:lowercase(K), V}
			|| {K, V} <- RespHeaders, 
		   		K /= <<"set-cookie">>,
				K /= <<"Set-Cookie">>,
				K /= <<"Transfer-Encoding">>
	]),

	% Convert cookies into a format that cowboy can handle.
	Headers = case lists:keyfind(<<"Set-Cookie">>, 1, RespHeaders) of
		false ->
			Headers0;
		{_CookieKey, CookieStr} ->
			case cow_cookie:parse_cookie(CookieStr) of
				false ->
					Headers0;
				CookieList when is_list(CookieList) ->
					maps:put(<<"set-cookie">>,
						[erlang:iolist_to_binary([K, <<"=">>, V]) || {K, V} <- CookieList],
						Headers0)
			end
	end,

	?info("Sending response. Status=~p, Headers=~p", [StatusCode, Headers]),
    Req = cowboy_req:reply(StatusCode, Headers, Body, Req0),
    {stop, Req, State};
info(Msg, Req, State) ->
	?info("Unexpected message. Msg=~p", [Msg]),
	{ok, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
	?info("HTTP client terminated"),
    ok.

%% ========================================
%% Private Functions
%% ========================================

read_req_body(MaxLength, Req0, Acc) when MaxLength == unlimited; MaxLength > byte_size(Acc) ->
	case cowboy_req:read_body(Req0) of
        {more, Data, Req} ->
			read_req_body(MaxLength, Req, << Acc/binary, Data/binary >>);
        {ok, Data, Req} ->
			{ok, Req, << Acc/binary, Data/binary >>}
	end.
