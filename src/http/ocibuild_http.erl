%%%-------------------------------------------------------------------
%%% @doc
%%% Public API for supervised HTTP operations.
%%%
%%% This module provides a clean interface to the HTTP supervision tree.
%%% It handles starting/stopping the supervisor on demand and provides
%%% the parallel map functionality used for layer downloads/uploads.
%%%
%%% Usage:
%%% ```
%%% %% Start is automatic, but can be explicit
%%% ok = ocibuild_http:start(),
%%%
%%% %% Execute HTTP operations in parallel
%%% Results = ocibuild_http:pmap(fun download_layer/1, Layers),
%%%
%%% %% Clean shutdown
%%% ok = ocibuild_http:stop().
%%% ```
%%% @end
%%%-------------------------------------------------------------------
-module(ocibuild_http).

-export([start/0, stop/0, pmap/2, pmap/3]).

-define(DEFAULT_MAX_WORKERS, 4).

%%%===================================================================
%%% API functions
%%%===================================================================

-doc """
Start the HTTP supervisor tree.

This is called automatically by `pmap/2,3` if not already started.
Safe to call multiple times (idempotent).
""".
-spec start() -> ok | {error, term()}.
start() ->
    case whereis(ocibuild_http_sup) of
        undefined ->
            case ocibuild_http_sup:start_link() of
                {ok, _Pid} ->
                    ok;
                {error, {already_started, _Pid}} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        _Pid ->
            ok
    end.

-doc """
Stop the HTTP supervisor tree.

This triggers a clean shutdown cascade:
1. Supervisor terminates all children (pool and workers)
2. Workers clean up their httpc profiles in terminate/2

Safe to call multiple times (idempotent).
""".
-spec stop() -> ok.
stop() ->
    case whereis(ocibuild_http_sup) of
        undefined ->
            ok;
        SupPid ->
            %% Unlink to avoid getting exit signal
            unlink(SupPid),
            %% Stop the supervisor - this will stop pool and all workers
            exit(SupPid, shutdown),
            %% Wait for shutdown to complete
            wait_for_shutdown(100),
            ok
    end.

-doc """
Execute function on each item in parallel, return results in order.

Uses default max concurrency of 4 workers.
Automatically starts the HTTP supervisor if not already running.
""".
-spec pmap(Fun :: fun((term()) -> term()), Items :: [term()]) -> [term()].
pmap(Fun, Items) ->
    pmap(Fun, Items, ?DEFAULT_MAX_WORKERS).

-doc """
Execute function on each item in parallel with specified max concurrency.

Returns results in the same order as input items.
Automatically starts the HTTP supervisor if not already running.

If any worker fails, the error is propagated to the caller.
""".
-spec pmap(Fun :: fun((term()) -> term()), Items :: [term()], MaxWorkers :: pos_integer()) ->
    [term()].
pmap(_Fun, [], _MaxWorkers) ->
    [];
pmap(Fun, Items, MaxWorkers) ->
    ok = start(),
    case ocibuild_http_pool:pmap(Fun, Items, MaxWorkers) of
        {error, {worker_crashed, Index, Reason}} ->
            error({worker_crashed, Index, Reason});
        {error, {Class, Reason, Stack}} ->
            %% Re-raise the exception from the worker
            erlang:raise(Class, Reason, Stack);
        Results when is_list(Results) ->
            Results
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Wait for supervisor to shut down
wait_for_shutdown(0) ->
    ok;
wait_for_shutdown(Retries) ->
    case whereis(ocibuild_http_sup) of
        undefined ->
            ok;
        _Pid ->
            timer:sleep(10),
            wait_for_shutdown(Retries - 1)
    end.
