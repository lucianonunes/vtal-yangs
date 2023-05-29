-module(oncnotif_log_writer).

-export([ print/1
        , print/2
        , start_link/0
        , stop/0
        ]).

-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).

-behaviour(gen_server).

-include("oncnotif.hrl").

-define(SERVER, ?MODULE).

%% === API =====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    stop(?SERVER).

stop(Server) ->
    gen_server:call(Server, stop).

print(Fmt) ->
    print(Fmt, []).

print(Fmt, Args) ->
    gen_server:cast(?SERVER, {log, Fmt, Args}).

%% === gen_server callbacks ====================================================

init([]) ->
    ?log("Starting ~p", [?MODULE]),
    {ok, _Fd} = file:open("./logs/oncnotif.log", [write]).

handle_call(stop, _From, Fd) ->
    file:close(Fd),
    {stop, normal, stopped, Fd};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log, Fmt, Args}, Fd) ->
    try
        io:format(Fd, Fmt, Args),
        file:sync(Fd)
    catch
        _:W ->
            io:format(Fd,
                      "~p: ERROR: The arguments (~p,~p) caused ~p ~n",
                      [?MODULE, Fmt, Args, W]),
            file:sync(Fd)
    end,
    {noreply, Fd};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
