-module(oncnotif_actions).

-export([ start_link/0
        , stop/0
        ,lifecycle_state_change/4
        ]).

-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).

-behaviour(gen_server).

-include_lib("econfd.hrl").
-include_lib("oncnotif.hrl").
-include("onc-netsim-settings.hrl").
-include("tapi-common@2020-04-23.hrl").

-define(SERVER, ?MODULE).

-record(params, { path
                , lifecycle_state
                }).

%% === API =====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    stop(?SERVER).

stop(Server) ->
    gen_server:call(Server, stop).

%% === gen_server callbacks ====================================================

init([]) ->
    ?log("Starting ~p", [?MODULE]),
    {ok, Daemon} = econfd:init_daemon(
                     oncnotif_actions,
                     ?CONFD_TRACE,
                     user,
                     none,
                     {127, 0, 0, 1},
                     ?CONFD_PORT),

    ok = econfd:register_action_cb(
            Daemon
            , #confd_action_cb{
                actionpoint = 'lifecycle-state-change'
                , action = fun ?MODULE:lifecycle_state_change/4
            }
        ),

    ok = econfd:register_done(Daemon),
    {ok, Daemon}.

handle_call(stop, _From, Daemon) ->
    econfd:stop_daemon(Daemon),
    {stop, normal, stopped, Daemon};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% === Internal functions=======================================================

lifecycle_state_change(_UInfo, _Name, _IKeyPath, Params) ->
    Params2 = parse_params(Params),
    ?log("Params recieved ~p", [Params2]),
    set_lifecycle_state(Params2#params.path, Params2#params.lifecycle_state),
    oncnotif_notifications:send_notification(?LCSPath(Params2#params.path), ?CONFD_ENUM_VALUE(1)).

set_lifecycle_state(Path, LifCycState) ->
    M = oncnotif_utils:new_maapi(),
    try econfd_maapi:start_trans(M, ?CONFD_RUNNING, ?CONFD_READ_WRITE) of
        {ok, TH} ->
            ok = econfd_maapi:set_elem(M, TH, ?LCSPath(Path),
                                        ?CONFD_ENUM_VALUE(LifCycState)),
            ok = econfd_maapi:apply_trans(M, TH, false),
            ok = econfd_maapi:finish_trans(M, TH);
        Error ->
            ?log("ERROR start_trans got: ~p", [Error]),
            error
    after
        econfd_maapi:close(M)
    end.

parse_params(Params) ->
    parse_params(Params, #params{}).

parse_params([{[_ | 'lifecycle-state'],
         ?CONFD_ENUM_VALUE(?onc_netsim_settings_PLANNED)} | T], Acc) ->
    parse_params(T, Acc#params{lifecycle_state = ?tapi_common_PLANNED});
parse_params([{[_ | 'lifecycle-state'],
         ?CONFD_ENUM_VALUE(?onc_netsim_settings_INSTALLED)} | T], Acc) ->
    parse_params(T, Acc#params{lifecycle_state = ?tapi_common_INSTALLED});
parse_params([{[_ | 'lifecycle-state'],
         ?CONFD_ENUM_VALUE(?onc_netsim_settings_PENDING_REMOVAL)} | T], Acc) ->
    parse_params(T, Acc#params{lifecycle_state = ?tapi_common_PENDING_REMOVAL});

parse_params([{[_ | uuid], UUID} | T], Acc) ->
    parse_params(T, Acc#params{path = ?CSPath(UUID)});

parse_params([], Acc) ->
    Acc.