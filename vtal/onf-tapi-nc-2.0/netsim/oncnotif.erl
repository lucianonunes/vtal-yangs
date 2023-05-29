-module(oncnotif).

-export([create_cs/1
        , delay_process/0
        ]).

-include("oncnotif.hrl").
-include_lib("econfd.hrl").
-include("econfd_errors.hrl").
-include("tapi-common@2020-04-23.hrl").

-on_load(on_load/0).

on_load() ->
    {ok, _} = application:ensure_all_started(oncnotif),
    ok.

create_cs(Path) ->

    M = oncnotif_utils:new_maapi(),

    try econfd_maapi:start_trans(M, ?CONFD_RUNNING, ?CONFD_READ_WRITE) of
        {ok, TH} ->
            set_frequency(M, TH, Path),
            set_tx_power(M, TH, Path),
            set_lifecycle_state(M, TH, Path, ?tapi_common_PLANNED),
            ok = econfd_maapi:apply_trans(M, TH, false),
            ok = econfd_maapi:finish_trans(M, TH);
        Error ->
            ?log("ERROR start_trans got: ~p", [Error]),
            error
    after
        econfd_maapi:close(M)
    end,
    oncnotif_notifications:send_notification(?LCSPath(Path), ?CONFD_ENUM_VALUE(1)),
    schedule_install(Path).

set_frequency(M, TH, Path) ->
    case get_frequency(M, TH, Path) of
        notset ->
            Fz_path1 = ?FHzPath("EndPoint12", Path),
            Fz_path2 = ?FHzPath("EndPoint22", Path),
            ok = econfd_maapi:set_elem(M, TH, Fz_path1, ?CONFD_UINT64(1961000)),
            ok = econfd_maapi:set_elem(M, TH, Fz_path2, ?CONFD_UINT64(1961000));
        Value ->
            ?log("Frequency Value is ~p", [Value]),
            ok
    end.

get_frequency(M, TH, Path) ->
    oncnotif_utils:get_elem(M, TH, ?FHzPath("EndPoint12", Path), notset).

set_tx_power(M, TH, Path) ->
    TRx_path1 = ?TRxPath("EndPoint12", Path),
    TRx_path2 = ?TRxPath("EndPoint22", Path),
    ok = econfd_maapi:set_elem(M, TH, TRx_path1,
                                 ?CONFD_DECIMAL64({-110000000, 0})),
    ok = econfd_maapi:set_elem(M, TH, TRx_path2,
                                 ?CONFD_DECIMAL64({-110000000, 0})).

set_lifecycle_state(M, TH, Path, Value) ->
    ok = econfd_maapi:set_elem(M, TH, ?LCSPath(Path), ?CONFD_ENUM_VALUE(Value)).

schedule_install(Path) ->
    case is_notification_disabled() of
        true ->
            ?log("notification disabled."),
            ok;
        false ->
            Val = get_notification_delay(),
            ?log("Delay notification ~p for ~p Sec", [Path, Val]),
            DelayProcessPid = spawn(?MODULE, delay_process, []),
            TimerRef = erlang:send_after(Val * 1000, DelayProcessPid,
                                        {Path, ?tapi_common_INSTALLED}),
            ?log("Proccess ~p is_alive: ~p , send_after TimerRef: ~p ",
                [DelayProcessPid, is_process_alive(DelayProcessPid), TimerRef])
    end.

get_notification_delay() ->
    M = oncnotif_utils:new_maapi(),
    try econfd_maapi:start_trans(M, ?CONFD_RUNNING, ?CONFD_READ) of
        {ok, TH} ->
            Path = [delay , ['onc-netsim-settings' | 'onc-netsim-settings']],
            oncnotif_utils:get_elem(M, TH, Path , 2);
        Error ->
            ?log("ERROR start_trans got: ~p", [Error]),
            error
    after
        econfd_maapi:close(M)
    end.

delay_process() ->
    receive
      Request ->
        {Path, LifCycState} = Request,
        M = oncnotif_utils:new_maapi(),
        try econfd_maapi:start_trans(M, ?CONFD_RUNNING, ?CONFD_READ_WRITE) of
            {ok, TH} ->
                set_lifecycle_state(M, TH, Path, LifCycState),
                ok = econfd_maapi:apply_trans(M, TH, false),
                ok = econfd_maapi:finish_trans(M, TH);
            Error ->
                ?log("ERROR start_trans got: ~p", [Error]),
                error
        after
            econfd_maapi:close(M)
        end,
        oncnotif_notifications:send_notification(?LCSPath(Path), ?CONFD_ENUM_VALUE(1), ?MODULE)
    end.

is_notification_disabled() ->
    M = oncnotif_utils:new_maapi(),
    try econfd_maapi:start_trans(M, ?CONFD_RUNNING, ?CONFD_READ) of
        {ok, TH} ->
            Path = [disable , ['onc-netsim-settings' | 'onc-netsim-settings']],
            oncnotif_utils:get_elem(M, TH, Path , false);
        Error ->
            ?log("ERROR start_trans got: ~p", [Error]),
            error
    after
        econfd_maapi:close(M)
    end.
