%%%-------------------------------------------------------------------
-module(ocibuild_mix).
-moduledoc """
OCI build adapter for Mix (Elixir) projects.

This module implements the `ocibuild_adapter` behaviour for Mix/Elixir
build systems. The Elixir side (Mix.Tasks.Ocibuild and Ocibuild.MixRelease)
extracts configuration and calls into this module.

## Usage from Elixir

```elixir
# Get configuration map to pass to ocibuild_release functions
config = :ocibuild_mix.get_config(state)

# Find release
{:ok, name, path} = :ocibuild_mix.find_release(state, opts)

# Use logging
:ocibuild_mix.info("Building image: ~s", [tag])
```

## State Format

The state passed to this adapter is a map containing:

```erlang
#{
    base_image => <<\"debian:stable-slim\">>,
    workdir => <<\"/app\">>,
    env => #{<<\"LANG\">> => <<\"C.UTF-8\">>},
    expose => [8080],
    labels => #{},
    cmd => <<\"start\">>,
    description => <<\"My app\">>,
    tags => [<<\"myapp:1.0.0\">>],
    output => <<\"myapp-1.0.0.tar.gz\">>,
    push => <<\"ghcr.io/myorg\">>,
    chunk_size => 5242880,
    %% Mix-specific fields
    release_name => myapp,
    release_path => <<\"/path/to/release\">>,
    releases_config => [...]
}
```
""".

-behaviour(ocibuild_adapter).

%%%===================================================================
%%% Exports
%%%===================================================================

%% Behaviour callbacks
-export([get_config/1, find_release/2, info/2, console/2, error/2]).
%% Optional adapter callbacks
-export([get_app_version/1, get_dependencies/1]).

%%%===================================================================
%%% Behaviour Implementation
%%%===================================================================

-doc """
Extract configuration from Mix state.

The state is expected to be a map that was already normalized by the
Elixir Mix task, so we just return it with defaults applied.
""".
-spec get_config(map()) -> ocibuild_adapter:config().
get_config(State) when is_map(State) ->
    Defaults = #{
        base_image => ~"debian:stable-slim",
        workdir => ~"/app",
        env => #{},
        expose => [],
        labels => #{},
        % Elixir uses "start" instead of "foreground"
        cmd => ~"start",
        description => undefined,
        tags => [],
        output => undefined,
        push => undefined,
        chunk_size => undefined,
        uid => undefined,
        app_version => undefined,
        app_name => undefined,
        vcs_annotations => true
    },
    maps:merge(Defaults, State).

-doc """
Find the release directory.

For Mix, the release path is typically passed in the state from Elixir.
If not provided, this function returns an error.
""".
-spec find_release(map(), map()) ->
    {ok, binary(), file:filename()} | {error, term()}.
find_release(State, _Opts) ->
    case {maps:get(release_name, State, undefined), maps:get(release_path, State, undefined)} of
        {undefined, _} ->
            {error, {missing_config, release_name}};
        {_, undefined} ->
            {error, {missing_config, release_path}};
        {Name, Path} when is_atom(Name) ->
            {ok, atom_to_binary(Name, utf8), Path};
        {Name, Path} when is_binary(Name) ->
            {ok, Name, Path};
        {Name, Path} when is_list(Name) ->
            {ok, list_to_binary(Name), Path}
    end.

-doc """
Get application version from Mix state.

The version is passed from Elixir via the state map as `app_version`.
This is used for the `org.opencontainers.image.version` annotation.
""".
-spec get_app_version(map()) -> binary() | undefined.
get_app_version(State) when is_map(State) ->
    case maps:get(app_version, State, undefined) of
        undefined -> undefined;
        Version when is_binary(Version) -> Version;
        Version when is_list(Version) -> list_to_binary(Version);
        _ -> undefined
    end.

-doc """
Get dependencies from Mix state.

The dependencies are passed from Elixir via the state map as `dependencies`.
This is used for smart layer classification (deps vs app code) and future SBOM.
""".
-spec get_dependencies(map()) ->
    {ok, [#{name := binary(), version := binary(), source := binary()}]}
    | {error, term()}.
get_dependencies(State) when is_map(State) ->
    case maps:get(dependencies, State, undefined) of
        undefined ->
            {error, not_available};
        Deps when is_list(Deps) ->
            %% Convert Elixir maps to Erlang maps with binary keys
            ErlDeps = [normalize_dep(D) || D <- Deps],
            {ok, ErlDeps};
        _ ->
            {error, invalid_format}
    end.

%% @private Normalize a dependency map from Elixir format
normalize_dep(Dep) when is_map(Dep) ->
    #{
        name => to_binary(maps:get(name, Dep, maps:get(~"name", Dep, ~"unknown"))),
        version => to_binary(maps:get(version, Dep, maps:get(~"version", Dep, ~"unknown"))),
        source => to_binary(maps:get(source, Dep, maps:get(~"source", Dep, ~"unknown")))
    }.

%% @private Convert to binary
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8).

-doc """
Log an informational message.

Uses standard io:format for output, which will be captured by the Elixir shell.
""".
-spec info(io:format(), [term()]) -> ok.
info(Format, Args) ->
    io:format(Format ++ "~n", Args),
    ok.

-doc """
Print a message to the console.

Uses standard io:format for output.
""".
-spec console(io:format(), [term()]) -> ok.
console(Format, Args) ->
    io:format(Format ++ "~n", Args),
    ok.

-doc """
Log an error message.

Outputs to standard_error for visibility.
""".
-spec error(io:format(), [term()]) -> ok.
error(Format, Args) ->
    io:format(standard_error, Format ++ "~n", Args),
    ok.
