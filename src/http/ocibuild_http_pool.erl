%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP pool coordinator for parallel operations.
%%%
%%% This gen_server coordinates parallel HTTP operations with bounded
%%% concurrency. It provides a `pmap/2,3` interface similar to the
%%% existing `pmap_bounded/3` but uses supervised HTTP workers.
%%%
%%% Key features:
%%% - Bounded concurrency (default: 4 workers)
%%% - Results returned in original order
%%% - Fail-fast: one worker crash fails entire operation
%%% - Clean shutdown via OTP supervision
%%% @end
%%%-------------------------------------------------------------------
-module(ocibuild_http_pool).

-behaviour(gen_server).

%% API
-export([start_link/0, pmap/2, pmap/3, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_WORKERS, 4).

-record(state, {
    max_workers = ?DEFAULT_MAX_WORKERS :: pos_integer(),
    %% Active workers: Pid -> {TaskIndex, MonitorRef}
    active = #{} :: #{pid() => {pos_integer(), reference()}},
    %% Pending tasks: [{TaskIndex, Task}]
    pending = [] :: [{pos_integer(), term()}],
    %% Collected results: TaskIndex -> Result
    results = #{} :: #{pos_integer() => term()},
    %% Caller waiting for pmap result
    caller = undefined :: {pid(), reference()} | undefined,
    %% Total number of tasks in current pmap
    total_tasks = 0 :: non_neg_integer(),
    %% Counter for unique worker IDs
    next_worker_id = 1 :: pos_integer(),
    %% The function to execute (captured for spawning workers)
    fun_to_call = undefined :: function() | undefined
}).

%%%===================================================================
%%% API functions
%%%===================================================================

-doc """
Start the HTTP pool coordinator.
""".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc """
Execute function on each item in parallel, return results in order.

Uses default max concurrency of 4 workers.
""".
-spec pmap(Fun :: fun((term()) -> term()), Items :: [term()]) -> [term()].
pmap(Fun, Items) ->
    pmap(Fun, Items, ?DEFAULT_MAX_WORKERS).

-doc """
Execute function on each item in parallel with specified max concurrency.

Returns results in the same order as input items.
If any worker crashes or returns an error, the entire operation fails.
""".
-spec pmap(Fun :: fun((term()) -> term()), Items :: [term()], MaxWorkers :: pos_integer()) ->
    [term()].
pmap(Fun, Items, MaxWorkers) ->
    gen_server:call(?SERVER, {pmap, Fun, Items, MaxWorkers}, infinity).

-doc """
Stop the pool coordinator.

This will cause the supervisor to shut down via auto_shutdown.
""".
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({pmap, _Fun, [], _MaxWorkers}, _From, State) ->
    %% Empty list - return immediately
    {reply, [], State};
handle_call({pmap, Fun, Items, MaxWorkers}, From, State) ->
    %% Create indexed items for ordered results
    IndexedItems = lists:zip(lists:seq(1, length(Items)), Items),

    NewState = State#state{
        max_workers = MaxWorkers,
        pending = IndexedItems,
        results = #{},
        caller = From,
        total_tasks = length(Items),
        active = #{},
        fun_to_call = Fun
    },

    %% Spawn initial batch of workers
    FinalState = spawn_workers(NewState),
    {noreply, FinalState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({worker_result, Pid, _Index, {error, {Class, Reason, Stack}}}, State) ->
    %% Worker completed with error - fail-fast
    Active = maps:remove(Pid, State#state.active),

    %% Stop all other workers
    lists:foreach(
        fun({P, {_, MonRef}}) ->
            erlang:demonitor(MonRef, [flush]),
            exit(P, shutdown)
        end,
        maps:to_list(Active)
    ),

    %% Reply with error - the caller will re-raise it
    gen_server:reply(State#state.caller, {error, {Class, Reason, Stack}}),
    {noreply, State#state{
        caller = undefined,
        active = #{},
        pending = [],
        fun_to_call = undefined
    }};
handle_info({worker_result, Pid, Index, {ok, Value}}, State) ->
    %% Worker completed successfully - store result
    Active = maps:remove(Pid, State#state.active),
    Results = maps:put(Index, Value, State#state.results),
    NewState = State#state{active = Active, results = Results},

    %% Check if all tasks are done
    case maps:size(Results) =:= NewState#state.total_tasks of
        true ->
            %% All done - collect results in order and reply
            OrderedResults = [maps:get(I, Results) || I <- lists:seq(1, NewState#state.total_tasks)],
            gen_server:reply(NewState#state.caller, OrderedResults),
            {noreply, NewState#state{caller = undefined, fun_to_call = undefined}};
        false ->
            %% More work to do - spawn additional workers
            FinalState = spawn_workers(NewState),
            {noreply, FinalState}
    end;
handle_info({'DOWN', _MonRef, process, Pid, Reason}, State) when Reason =/= normal ->
    %% Worker crashed - fail the entire pmap
    case maps:find(Pid, State#state.active) of
        {ok, {Index, _}} ->
            %% Stop all other workers
            lists:foreach(
                fun(P) ->
                    case P of
                        Pid -> ok;
                        _ -> exit(P, shutdown)
                    end
                end,
                maps:keys(State#state.active)
            ),

            %% Reply with error
            gen_server:reply(State#state.caller, {error, {worker_crashed, Index, Reason}}),
            {noreply, State#state{
                caller = undefined,
                active = #{},
                pending = [],
                fun_to_call = undefined
            }};
        error ->
            %% Unknown worker - ignore
            {noreply, State}
    end;
handle_info({'DOWN', _MonRef, process, _Pid, normal}, State) ->
    %% Normal termination - handled by worker_result message
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Spawn workers up to max concurrency
spawn_workers(#state{active = Active, pending = [], max_workers = _Max} = State) when Active =:= #{} ->
    %% No pending work and no active workers - nothing to do
    State;
spawn_workers(#state{active = Active, pending = Pending, max_workers = Max} = State) ->
    AvailableSlots = Max - maps:size(Active),
    case AvailableSlots > 0 andalso Pending =/= [] of
        true ->
            %% Spawn workers for available slots
            {ToSpawn, Remaining} = lists:split(min(AvailableSlots, length(Pending)), Pending),

            {NewActive, NewNextId} = lists:foldl(
                fun({Index, Item}, {AccActive, AccId}) ->
                    Task = #{
                        fun_to_call => State#state.fun_to_call,
                        item => Item,
                        index => Index,
                        worker_id => AccId
                    },
                    {ok, Pid} = ocibuild_http_sup:start_worker(Task, self()),
                    MonRef = monitor(process, Pid),
                    {maps:put(Pid, {Index, MonRef}, AccActive), AccId + 1}
                end,
                {Active, State#state.next_worker_id},
                ToSpawn
            ),

            State#state{
                active = NewActive,
                pending = Remaining,
                next_worker_id = NewNextId
            };
        false ->
            %% No slots available or no pending work
            State
    end.

