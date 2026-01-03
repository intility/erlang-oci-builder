-module(ocibuild_http_sup).
-moduledoc """
OTP supervisor for HTTP operations.

This supervisor manages the HTTP pool and workers for parallel
downloads and uploads.

Architecture:
```
ocibuild_http_sup (one_for_one)
├── ocibuild_http_pool (gen_server) [transient]
│   - Coordinates parallel HTTP operations
│   - Enforces max concurrency
│
└── ocibuild_http_worker (gen_server) [temporary]
    - Started dynamically via start_worker/2
    - Owns unique httpc profile
    - Cleans up in terminate/2
```
""".

-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-doc """
Start the HTTP supervisor.

Returns `{ok, Pid}` on success or `{error, Reason}` on failure.
""".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-doc """
Start a new HTTP worker to execute a task.

The worker will execute the task and send the result back to `ReplyTo`.
Workers are temporary and will terminate after completing their task.
""".
-spec start_worker(Task :: map(), ReplyTo :: pid()) -> {ok, pid()} | {error, term()}.
start_worker(Task, ReplyTo) ->
    %% Create a unique ID for this worker to avoid conflicts
    WorkerId = {ocibuild_http_worker, erlang:unique_integer([positive])},
    WorkerSpec = #{
        id => WorkerId,
        start => {ocibuild_http_worker, start_link, [Task, ReplyTo]},
        restart => temporary,
        shutdown => 10000,
        type => worker,
        significant => false,
        modules => [ocibuild_http_worker]
    },
    supervisor:start_child(?SERVER, WorkerSpec).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    PoolSpec = #{
        id => ocibuild_http_pool,
        start => {ocibuild_http_pool, start_link, []},
        restart => transient,  % Restart on abnormal exit, not on normal stop
        shutdown => 5000,
        type => worker,
        significant => false,  % Don't trigger auto_shutdown on pool exit
        modules => [ocibuild_http_pool]
    },

    %% Workers are started dynamically via start_worker/2
    {ok, {SupFlags, [PoolSpec]}}.
