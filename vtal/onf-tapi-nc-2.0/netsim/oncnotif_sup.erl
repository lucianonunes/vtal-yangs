-module(oncnotif_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I,
                         {I, start_link, []},
                         permanent,
                         5000,
                         Type,
                         [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = [?CHILD(oncnotif_notifications, worker)
                ,?CHILD(oncnotif_log_writer, worker)
               ,?CHILD(oncnotif_actions, worker)
               , ?CHILD(oncnotif_subscriber, worker)
               ],

    {ok, {SupFlags, Children}}.
