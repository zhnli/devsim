%%%-------------------------------------------------------------------
%% @doc devsim top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(devsim_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Config) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpec1 = #{id => telnet_proxy,
                   start => {telnet_proxy, start, [Config]},
                   restart => permanent,
                   shutdown => brutal_kill,
                   type => worker,
                   modules => [telnet_proxy]},
    ChildSpec2 = #{id => snmp_proxy,
                   start => {snmp_proxy, start_link, [Config]},
                   restart => permanent,
                   shutdown => brutal_kill,
                   type => worker,
                   modules => [snmp_proxy]},
    {ok, {SupFlags, [ChildSpec1, ChildSpec2]}}.

%%====================================================================
%% Internal functions
%%====================================================================
