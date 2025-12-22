%%%-------------------------------------------------------------------
-module(ocibuild_test_adapter).
-moduledoc "Mock adapter for testing ocibuild_release:run/3".

-behaviour(ocibuild_adapter).

%% ocibuild_adapter callbacks
-export([get_config/1, find_release/2, info/2, console/2, error/2]).

%%%===================================================================
%%% ocibuild_adapter callbacks
%%%===================================================================

-spec get_config(map()) -> map().
get_config(State) ->
    State.

-spec find_release(map(), map()) -> {ok, atom(), file:filename()} | {error, term()}.
find_release(#{release_name := Name, release_path := Path}, _Opts) ->
    {ok, Name, Path}.

-spec info(string(), list()) -> ok.
info(_Format, _Args) ->
    %% Suppress output during tests
    ok.

-spec console(string(), list()) -> ok.
console(_Format, _Args) ->
    %% Suppress output during tests
    ok.

-spec error(string(), list()) -> ok.
error(_Format, _Args) ->
    %% Suppress output during tests
    ok.
