%%%-------------------------------------------------------------------
%% @doc devsim public API
%% @end
%%%-------------------------------------------------------------------

-module(devsim_app).

-behaviour(application).

-include("devsim.hrl").
-include("log.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    % lager:start(),
    lager:error("Some message"),
    ?info("Starting devsim."),
    Config = #config{remote_addr="172.25.101.143",
                     local_port_telnet=8023,
                     remote_port_telnet=23,
                     local_port_snmp=8161,
                     remote_port_snmp=161,
                     local_port_trap=8162,
                     remote_port_trap=162,
                     snmp_community="cisco"
                    },
    devsim_sup:start_link(Config).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
