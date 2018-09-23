
% -define(info(F),
%     lager:info(F).
%     log:log(info, "~p:~p:~p - " ++ F ++ "~n",[self(),?MODULE,?LINE])).

% -define(info(F,A),
%     log:log(info, "~p:~p:~p - " ++ F ++ "~n",[self(),?MODULE,?LINE]++A)).

% -define(debug(F),
%     log:log(debug, "~p:~p:~p - " ++ F ++ "~n",[self(),?MODULE,?LINE])).

% -define(debug(F,A),
%     log:log(debug, "~p:~p:~p - " ++ F ++ "~n",[self(),?MODULE,?LINE]++A)).
