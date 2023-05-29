-module(oncnotif_notifications).

-export([ send_notification/2
        , send_notification/3
        , start_link/0
        , stop/0
        ]).

-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2]).

-include_lib("econfd.hrl").
-include("oncnotif.hrl").

-behaviour(gen_server).

-record(state, {notification_context}).

-define(SERVER, ?MODULE).
-define(NOTNS,  'urn:ietf:params:xml:ns:yang:ietf-netconf-notifications').

%% === API =====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(Module) ->
    gen_server:call(Module, stop).

stop() ->
    stop(?SERVER).

send_notification(Path, Op) ->
    send_notification(Path, Op, undefined).

send_notification(Path, Op, Source) ->
    gen_server:cast(?SERVER, {send_notification, Path, Op, Source}).

%% === gen_server callbacks ====================================================

init([]) ->
    ?log("Starting ~p", [?MODULE]),
    {ok, Daemon} = econfd:init_daemon(
                     oncnotif_notifications,
                     ?CONFD_TRACE,
                     user,
                     none,
                     {127, 0, 0, 1},
                     ?CONFD_PORT),
    {ok, NotificationContext} = econfd:register_notification_stream(
                                  Daemon,
                                  #confd_notification_stream_cbs{
                                     streamname = 'CONC_NETCONF'}),
    ok = econfd:register_done(Daemon),
    {ok, #state{notification_context = NotificationContext}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_notification, IKP, Op, Source}, State) ->
    send_notification(State, IKP, Op, Source),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% === Internal ================================================================

send_notification(State, Path, Op, _Source) ->
    Xml = create_notification(Path, Op),
    send(State, Xml).

create_notification(Path, Op) ->
    [{[?NOTNS|'netconf-config-change'], start},
        {[?NOTNS|'changed-by'], start},
            {[?NOTNS|server], leaf},
        {[?NOTNS|'changed-by'], stop},
        {[?NOTNS|'datastore'], ?CONFD_ENUM_VALUE(0)},
        {[?NOTNS|'edit'], start},
            {[?NOTNS|target], ?CONFD_INSTANCE_IDENTIFIER(Path)},
            {[?NOTNS|operation], Op},
        {[?NOTNS|'edit'], stop} |
    [{[?NOTNS|'netconf-config-change'], stop}]].

send(State, Xml) ->
    NotificationContext = State#state.notification_context,
    DateTime = dateTime(),
    case econfd:notification_send(NotificationContext, DateTime, Xml) of
        ok ->
            ?log("Successfully sent notification ~p", [Xml]),
            ?log("Sent at ~p", [DateTime]),
            ok;
        Error ->
            ?log("Error sending notification: ~p", [Error]),
            Error
    end.


dateTime() ->
    Now = erlang:timestamp(),
    {_, _, Micro} = Now,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_local_time(Now),
    case calendar:now_to_universal_time(Now) of
        {{Y, Mo, D}, {UH, UMi, S}} ->
            Tz = H - UH,
            Tzm = Mi - UMi;
        _ ->
            %% timezone info isn't known
            Tz = [],
            Tzm = 0
    end,
    {?C_DATETIME, {Y, Mo, D, H, Mi, S, Micro, Tz, Tzm}}.