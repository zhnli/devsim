%% devsim.hrl

-define(info(F), lager:log(info,
                           [{pid, erlang:self()},
                             {module, ?MODULE},
                             {function, atom_to_list(?FUNCTION_NAME) ++ "/" ++ integer_to_list(?FUNCTION_ARITY)},
                             {line, ?LINE}],
                           "- " ++ F, [])).
-define(info(F, A), lager:log(info,
                              [{pid, erlang:self()},
                               {module, ?MODULE},
                               {function, atom_to_list(?FUNCTION_NAME) ++ "/" ++ integer_to_list(?FUNCTION_ARITY)},
                               {line, ?LINE}],
                              "- " ++ F, A)).

% -define(info(F, A), larger:lager_log(error,F, A)).

-record(config,
        {remote_addr,
         local_port_telnet, remote_port_telnet,
         local_port_snmp, remote_port_snmp,
         local_port_trap, remote_port_trap,
         snmp_community, snmp_user, snmp_pwd}).

-record(msg_from,
        {addr,
         port,
         sock}).

-record(snmp_cntx,
        {from,
         version,
         community,
         req_id}).