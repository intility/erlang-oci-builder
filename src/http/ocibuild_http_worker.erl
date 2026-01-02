%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP worker for executing single HTTP operations.
%%%
%%% Each worker:
%%% - Owns its own httpc profile (isolation)
%%% - Executes a single task
%%% - Sends result back to pool
%%% - Terminates after completing task
%%% - Properly cleans up httpc in terminate/2
%%%
%%% The httpc profile is set in the process dictionary so that
%%% HTTP helper functions in ocibuild_registry can access it.
%%% @end
%%%-------------------------------------------------------------------
-module(ocibuild_http_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    httpc_pid :: pid() | undefined,
    profile :: atom(),
    task :: map(),
    reply_to :: pid(),
    request_id :: reference() | undefined
}).

%%%===================================================================
%%% API functions
%%%===================================================================

-doc """
Start a new HTTP worker to execute a task.

The worker will:
1. Start its own httpc profile
2. Execute the task function
3. Send result to ReplyTo
4. Terminate
""".
-spec start_link(Task :: map(), ReplyTo :: pid()) -> {ok, pid()} | {error, term()}.
start_link(Task, ReplyTo) ->
    gen_server:start_link(?MODULE, {Task, ReplyTo}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Task, ReplyTo}) ->
    %% Generate unique profile name for this worker
    WorkerId = maps:get(worker_id, Task),
    Profile = list_to_atom("httpc_ocibuild_" ++ integer_to_list(WorkerId)),

    %% Start SSL if needed (idempotent)
    _ = ssl:start(),

    %% Start httpc in stand_alone mode (linked to this worker)
    %% When worker dies, httpc dies too
    case inets:start(httpc, [{profile, Profile}], stand_alone) of
        {ok, HttpcPid} ->
            %% Link to httpc so it dies with us
            link(HttpcPid),

            %% Register profile so httpc:request can find it
            ProfileName = httpc_profile_name(Profile),
            register(ProfileName, HttpcPid),

            State = #state{
                httpc_pid = HttpcPid,
                profile = Profile,
                task = Task,
                reply_to = ReplyTo
            },

            {ok, State, {continue, execute}};
        {error, Reason} ->
            {stop, {httpc_start_failed, Reason}}
    end.

handle_continue(execute, #state{task = Task, reply_to = ReplyTo, profile = Profile} = State) ->
    Fun = maps:get(fun_to_call, Task),
    Item = maps:get(item, Task),
    Index = maps:get(index, Task),

    %% Set profile in process dict for HTTP helpers to use
    put(ocibuild_httpc_profile, Profile),

    %% Execute the function
    Result =
        try
            {ok, Fun(Item)}
        catch
            Class:Reason:Stack ->
                {error, {Class, Reason, Stack}}
        end,

    %% Send result back to pool
    ReplyTo ! {worker_result, self(), Index, Result},

    %% Terminate normally
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{httpc_pid = HttpcPid, profile = Profile, request_id = ReqId}) ->
    %% Cancel any pending request
    case ReqId of
        undefined -> ok;
        _ -> catch httpc:cancel_request(ReqId)
    end,

    %% Unregister profile name
    ProfileName = httpc_profile_name(Profile),
    catch unregister(ProfileName),

    %% Stop httpc - it's linked so will die anyway, but be explicit
    case HttpcPid of
        undefined -> ok;
        Pid when is_pid(Pid) -> catch inets:stop(stand_alone, Pid)
    end,

    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Generate the registered name for an httpc profile (same as OTP internal)
httpc_profile_name(Profile) ->
    list_to_atom("httpc_" ++ atom_to_list(Profile)).
