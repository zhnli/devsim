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
    io:format("Starting dependencise~n"),
    ?info("Starting deps"),
    ok = application:start(crypto),
    % ok = application:start(mnesia),

    % larger
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:start(lager),
    % ok = application:start(crypto),

    % cowboy
    ok = application:start(sasl),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),

    % hackney
    ok = application:start(metrics),
    ok = application:start(ssl_verify_fun),
    ok = application:start(certifi),
    ok = application:start(mimerl),
    ok = application:start(unicode_util_compat),
    ok = application:start(idna),
    ok = application:start(hackney),

    % ok = application:start(devsim),

    ?info("Starting devsim."),
    Config = #config {
        mode=default,
        remote_addr="172.25.101.140",
        % Telnet
        local_port_telnet=8023,
        remote_port_telnet=23,
        % SNMP
        local_port_snmp=8161,
        remote_port_snmp=161,
        local_port_trap=8162,
        remote_port_trap=162,
        snmp_community="cisco",
        % HTTP
        local_port_http=8443,
        remote_port_http=443,
        % Certs should be in DER format, PEM won't work
        ca_cert="certs/ca.crt",
        server_cert="certs/server.crt",
        priv_key="certs/server.key"
    },
    devsim_sup:start_link(Config).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
