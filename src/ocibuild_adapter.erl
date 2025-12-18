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
    #{base_image => <<"debian:slim">>, ...}.

find_release(State, Opts) ->
    %% Find the release directory
    {ok, <<"myapp">>, "/path/to/release"}.

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
%%% Type Definitions
%%%===================================================================

-type config() :: #{
    %% Base image to use (e.g., <<"debian:slim">>)
    base_image => binary(),
    %% Working directory in container (default: /app)
    workdir => binary(),
    %% Environment variables
    env => #{binary() => binary()},
    %% Ports to expose
    expose => [non_neg_integer()],
    %% Labels to add to image config
    labels => #{binary() => binary()},
    %% Release start command (e.g., <<"foreground">> for Erlang, <<"start">> for Elixir)
    cmd => binary(),
    %% Image description (OCI annotation)
    description => binary() | undefined,
    %% Image tag (e.g., <<"myapp:1.0.0">>)
    tag => binary() | undefined,
    %% Output tarball path
    output => binary() | undefined,
    %% Registry to push to (e.g., <<"ghcr.io/myorg">>)
    push => binary() | undefined,
    %% Chunk size for uploads in bytes
    chunk_size => pos_integer() | undefined
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
