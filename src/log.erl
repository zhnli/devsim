-module(log).
% -include_lib("kernel/include/logger.hrl").
-export([log/3]).

log(info, Txt, Fmt) ->
    io:format("==> INFO "++Txt, Fmt);
    % lager:error("==> INFO "++Txt, Fmt);
    % ?LOG_ERROR("==> INFO "++Txt, Fmt);
log(debug, Txt, Fmt) ->
    io:format("==> DEBUG "++Txt, Fmt).
