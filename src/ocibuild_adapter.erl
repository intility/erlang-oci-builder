%%%-------------------------------------------------------------------
-module(ocibuild_adapter).
-moduledoc """
Behaviour for OCI build system adapters.

This behaviour defines the interface that build system integrations
(rebar3, Mix, Gleam, LFE, etc.) must implement to work with ocibuild.

Adapters are responsible for:
- Extracting configuration from the build system
- Finding release artifacts
- Logging messages in the build system's format

The common functionality (file collection, image building, save, push)
is provided by `ocibuild_release`.

## Implementing an Adapter

```erlang
-module(my_adapter).
-behaviour(ocibuild_adapter).

-export([get_config/1, find_release/2, info/2, console/2, error/2]).

get_config(State) ->
    %% Extract and normalize config from your build system
    #{base_image => ~"debian:stable-slim", ...}.

find_release(State, Opts) ->
    %% Find the release directory
    {ok, ~"myapp", "/path/to/release"}.

info(Format, Args) ->
    %% Log info message
    io:format(Format ++ "~n", Args).

console(Format, Args) ->
    %% Print to console (user-facing)
    io:format(Format ++ "~n", Args).

error(Format, Args) ->
    %% Log error message
    io:format(standard_error, Format ++ "~n", Args).
```
""".

%%%===================================================================
%%% Constants (shared by all adapters)
%%%===================================================================

%% Chunk size limits for uploads (in MB and bytes)
-define(MIN_CHUNK_SIZE_MB, 1).
-define(MAX_CHUNK_SIZE_MB, 100).
-define(DEFAULT_CHUNK_SIZE_MB, 5).
-define(MIN_CHUNK_SIZE, ?MIN_CHUNK_SIZE_MB * 1024 * 1024).
-define(MAX_CHUNK_SIZE, ?MAX_CHUNK_SIZE_MB * 1024 * 1024).
-define(DEFAULT_CHUNK_SIZE, ?DEFAULT_CHUNK_SIZE_MB * 1024 * 1024).

%% Export macros as functions for use by adapters
-export([
    min_chunk_size_mb/0,
    max_chunk_size_mb/0,
    default_chunk_size_mb/0,
    min_chunk_size/0,
    max_chunk_size/0,
    default_chunk_size/0
]).

%%%===================================================================
%%% Type Definitions
%%%===================================================================

-type config() :: #{
    %% Base image to use (e.g., ~"debian:stable-slim")
    base_image => binary(),
    %% Working directory in container (default: /app)
    workdir => binary(),
    %% Environment variables
    env => #{binary() => binary()},
    %% Ports to expose
    expose => [non_neg_integer()],
    %% Labels to add to image config
    labels => #{binary() => binary()},
    %% Release start command (e.g., ~"foreground" for Erlang, ~"start" for Elixir)
    cmd => binary(),
    %% Image description (OCI annotation)
    description => binary() | undefined,
    %% Image tag (e.g., ~"myapp:1.0.0")
    tag => binary() | undefined,
    %% Output tarball path
    output => binary() | undefined,
    %% Registry to push to (e.g., ~"ghcr.io/myorg")
    push => binary() | undefined,
    %% Chunk size for uploads in bytes
    chunk_size => pos_integer() | undefined,
    %% User ID to run as (default: 65534 for nobody)
    uid => non_neg_integer() | undefined,
    %% Application version for org.opencontainers.image.version annotation
    app_version => binary() | undefined,
    %% Enable/disable automatic VCS annotations (default: true)
    vcs_annotations => boolean(),
    %% SBOM export path (optional, from --sbom CLI flag)
    sbom => binary() | undefined
}.

-export_type([config/0]).

%%%===================================================================
%%% Behaviour Callbacks
%%%===================================================================

-doc """
Extract and normalize configuration from the build system.

The State parameter is build-system-specific (e.g., rebar_state:t() for rebar3,
or a map for Mix).

Returns a normalized config map that ocibuild_release functions can use.
""".
-callback get_config(State :: term()) -> config().

-doc """
Find the release directory for the given configuration.

Returns the release name and path where the release artifacts are located.
The Opts map may contain:
- `release` - specific release name to use (if multiple configured)
""".
-callback find_release(State :: term(), Opts :: map()) ->
    {ok, ReleaseName :: binary(), ReleasePath :: file:filename()} | {error, term()}.

-doc """
Log an informational message.

Used for build progress messages like "Building OCI image: myapp:1.0.0".
""".
-callback info(Format :: io:format(), Args :: [term()]) -> ok.

-doc """
Print a message to the console.

Used for user-facing output like instructions on how to load the image.
""".
-callback console(Format :: io:format(), Args :: [term()]) -> ok.

-doc """
Log an error message.

Used for error conditions that should be displayed to the user.
""".
-callback error(Format :: io:format(), Args :: [term()]) -> ok.

-doc """
Get the application version from the build system.

This is an optional callback. If not implemented, the version annotation
will not be added to the image. Adapters can implement this to provide
the application version for the `org.opencontainers.image.version` annotation.

For rebar3: Parse the `.app.src` file or relx config for version.
For Mix: Return `Mix.Project.config()[:version]`.
""".
-callback get_app_version(State :: term()) -> binary() | undefined.

-doc """
Get dependencies from the build system's lock file.

This is an optional callback. If not implemented, smart dependency layering
is disabled and all files go into a single layer (backward compatible behavior).

Returns a list of dependency maps with name, version, and source.
This data is used for:
1. Smart layer classification (deps layer vs app layer)
2. Future SBOM generation (P6)

For rebar3: Parse `rebar.lock` file.
For Mix: Parse `mix.lock` file.
""".
-callback get_dependencies(State :: term()) ->
    {ok, [#{name := binary(), version := binary(), source := binary()}]}
    | {error, term()}.

%% Mark optional callbacks - adapters don't have to implement these
-optional_callbacks([get_app_version/1, get_dependencies/1]).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

-export([get_app_version/2, get_dependencies/2]).

-doc """
Get application version from an adapter, with fallback for optional callback.

If the adapter implements `get_app_version/1`, calls it and returns the result.
If not implemented, returns `undefined`.

Example:
```erlang
Version = ocibuild_adapter:get_app_version(ocibuild_rebar3, State).
```
""".
-spec get_app_version(module(), term()) -> binary() | undefined.
get_app_version(Adapter, State) ->
    case erlang:function_exported(Adapter, get_app_version, 1) of
        true -> Adapter:get_app_version(State);
        false -> undefined
    end.

-doc """
Get dependencies from an adapter, with fallback for optional callback.

If the adapter implements `get_dependencies/1`, calls it and returns the result.
If not implemented or returns error, returns an empty list (with a warning for errors).

The returned list contains maps with `name`, `version`, and `source` keys,
which can be used for smart layer classification and SBOM generation.

Example:
```erlang
Deps = ocibuild_adapter:get_dependencies(ocibuild_rebar3, State).
%% Returns: [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}, ...]
```
""".
-spec get_dependencies(module(), term()) ->
    [#{name := binary(), version := binary(), source := binary()}].
get_dependencies(Adapter, State) ->
    case erlang:function_exported(Adapter, get_dependencies, 1) of
        true ->
            case Adapter:get_dependencies(State) of
                {ok, Deps} ->
                    Deps;
                {error, Reason} ->
                    %% Log warning for debugging - dependency parsing failed
                    %% Falls back to single-layer mode (backward compatible)
                    io:format(
                        standard_error,
                        "ocibuild: warning: failed to parse dependencies (~p), "
                        "falling back to single-layer mode~n",
                        [Reason]
                    ),
                    []
            end;
        false ->
            []
    end.

%%%===================================================================
%%% Constant accessors
%%%===================================================================

-doc "Minimum chunk size for uploads in MB.".
-spec min_chunk_size_mb() -> pos_integer().
min_chunk_size_mb() -> ?MIN_CHUNK_SIZE_MB.

-doc "Maximum chunk size for uploads in MB.".
-spec max_chunk_size_mb() -> pos_integer().
max_chunk_size_mb() -> ?MAX_CHUNK_SIZE_MB.

-doc "Default chunk size for uploads in MB.".
-spec default_chunk_size_mb() -> pos_integer().
default_chunk_size_mb() -> ?DEFAULT_CHUNK_SIZE_MB.

-doc "Minimum chunk size for uploads in bytes.".
-spec min_chunk_size() -> pos_integer().
min_chunk_size() -> ?MIN_CHUNK_SIZE.

-doc "Maximum chunk size for uploads in bytes.".
-spec max_chunk_size() -> pos_integer().
max_chunk_size() -> ?MAX_CHUNK_SIZE.

-doc "Default chunk size for uploads in bytes.".
-spec default_chunk_size() -> pos_integer().
default_chunk_size() -> ?DEFAULT_CHUNK_SIZE.
