{application, oncnotif, [ {description,  "ONC simulator"}
                        , {vsn,          "1.0"}
                        , {modules,      [oncnotif_actions, oncnotif_app, oncnotif_subscriber, oncnotif, oncnotif_notifications, oncnotif_utils, oncnotif_log_writer, oncnotif_sup]}
                        , {mod,          {oncnotif_app, []}}
                        , {registered,   []}
                        , {applications, [stdlib, kernel, crypto, econfd]}
                        ]}.

