-module(oncnotif_subscriber).

-export([ start_link/0]).


-include("oncnotif.hrl").
-include_lib("econfd.hrl").
-include("econfd_errors.hrl").

-define(SOURCE, subscriber).

%% === API =====================================================================

start_link() ->
    ?log("Starting ~p", [?MODULE]),
    {ok, proc_lib:spawn_link(fun subscriber/0)}.

%% === Internal functions=======================================================

subscriber() ->
    {ok, Sock} = econfd_cdb:connect({127,0,0,1}, ?CONFD_PORT),
    ok = econfd_cdb:wait_start(Sock),
    {ok, SubscriberSock} = econfd_cdb:connect({127,0,0,1}),
    {ok, Sub} = econfd_cdb:subscribe_session(SubscriberSock),
    subscribe(Sub, "/tapi-common:context/tapi-connectivity:"
                "connectivity-context/tapi-connectivity:connectivity-service"),
    ok = econfd_cdb:subscribe_done(Sub),
    ?log("Subscribe done"),
    subscribe_loop(Sock, SubscriberSock, Sub).

subscribe(Session, Path) ->
    {ok, Id} = econfd_cdb:subscribe(Session, 1, Path),
    ?log("Subscribed to ~p", [Path]),
    Id.

subscribe_loop(S, SubscriberSock, Sub) ->
    Reader = fun(Update) -> reader(Update, Sub) end,
    try
        econfd_cdb:wait(Sub, 20000, Reader)
    catch X:Y ->
        ?log("ERR reader died ~p", [{X, Y}])
    end,
    subscribe_loop(S, SubscriberSock, Sub).

reader(Points, SubscriptionSocket) ->
    reader(Points, SubscriptionSocket, []).

reader([], _, Acc0) ->
    Acc = lists:reverse(Acc0),
    ?log("Created = ~p", [Acc]),
    proc_lib:spawn(fun() -> post_iter(Acc) end),
    ?CDB_DONE_PRIORITY;
reader([Point | T], SubscriptionSocket, Acc) ->
    {ok, NewAcc} = econfd_cdb:diff_iterate(SubscriptionSocket,
                                           Point,
                                           fun iter/5,
                                           ?CDB_ITER_WANT_PREV,
                                           Acc),
    reader(T, SubscriptionSocket, NewAcc).

iter(IKP = [{_Service}, 'connectivity-service',
        ['urn:onf:otcc:yang:tapi-connectivity'|
        'connectivity-context'] | _], ?MOP_CREATED, _, _, State) ->
    Fun = fun() ->
               ok = oncnotif:create_cs(IKP)
          end,
    ?log("Create at ~p", [IKP]),
    {ok, ?ITER_CONTINUE, [Fun | State]};
iter(IKP = [{_Service}, 'connectivity-service',
        ['urn:onf:otcc:yang:tapi-connectivity'|
        'connectivity-context'] | _],  ?MOP_DELETED, _, _, State) ->
    spawn(fun() ->
               timer:sleep(5000),
               ok = oncnotif_notifications:send_notification(IKP, ?CONFD_ENUM_VALUE(3))
          end),
    ?log("Delete at ~p", [IKP]),
    {ok, ?ITER_CONTINUE, State};
iter(_IKP, _Op, _OldValue, _Value, State)  ->
    ?log("Ignore ~p at ~p", [_Op, _IKP]),
    {ok, ?ITER_RECURSE, State}.

post_iter([Fun | Tail])->
    Fun(),
    post_iter(Tail);
post_iter([]) ->
    ok.

