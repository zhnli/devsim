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
-define(error(F), lager:log(error,
                           [{pid, erlang:self()},
                             {module, ?MODULE},
                             {function, atom_to_list(?FUNCTION_NAME) ++ "/" ++ integer_to_list(?FUNCTION_ARITY)},
                             {line, ?LINE}],
                           "- " ++ F, [])).
-define(error(F, A), lager:log(error,
                              [{pid, erlang:self()},
                               {module, ?MODULE},
                               {function, atom_to_list(?FUNCTION_NAME) ++ "/" ++ integer_to_list(?FUNCTION_ARITY)},
                               {line, ?LINE}],
                              "- " ++ F, A)).

% -define(info(F, A), larger:lager_log(error,F, A)).

-define(check(Pred,Ex1,Ex2), (case (Pred) of true -> (Ex1); false -> (Ex2) end)).

-record(config,
        {mode, % default, record, sim, sim_record
         remote_addr,
         local_port_telnet, remote_port_telnet,
         local_port_snmp, remote_port_snmp,
         local_port_trap, remote_port_trap,
         snmp_community ,
         local_port_http, remote_port_http,
         remote_addr_http,
         ca_cert, server_cert, priv_key}).

-record(msg_from,
        {addr,
         port,
         sock}).

-record(snmp_cntx,
        {from,
         version,
         community,
         req_id,
         req_type,
         varbinds}).
