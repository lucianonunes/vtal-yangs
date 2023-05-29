-define(log(Fmt),
        oncnotif_log_writer:print("~w:~w " ++ Fmt ++ "~n",
                                         [?MODULE, ?LINE])).
-define(log(Fmt, Args),
        oncnotif_log_writer:print("~w:~w " ++ Fmt ++ "~n",
                                         [?MODULE, ?LINE | Args])).

-define(FHzPath(EndPoint, Path),
        ['central-frequency', 'central-frequency',
          {erlang:list_to_binary(EndPoint ++ "_otsi")}, 'otsi-config',
          ['urn:onf:otcc:yang:tapi-photonic-media' |
              'otsia-connectivity-service-end-point-spec'],
          {erlang:list_to_binary(EndPoint)}, 'end-point' | Path]).

-define(TRxPath(EndPoint, Path),
        ['total-power', 'transmit-power',
            {erlang:list_to_binary(EndPoint ++  "_otsi")}, 'otsi-config',
            ['urn:onf:otcc:yang:tapi-photonic-media' |
                'otsia-connectivity-service-end-point-spec'],
             {erlang:list_to_binary(EndPoint)}, 'end-point' | Path]).

-define(LCSPath(Path), ['lifecycle-state' | Path]).

-define(CSPath(UUID), [{UUID},'connectivity-service',
                        ['urn:onf:otcc:yang:tapi-connectivity'|
                        'connectivity-context'],
                        ['urn:onf:otcc:yang:tapi-common'|context]]).