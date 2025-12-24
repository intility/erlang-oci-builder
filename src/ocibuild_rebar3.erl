%%%-------------------------------------------------------------------
-module(ocibuild_rebar3).
-moduledoc """
Rebar3 provider for building OCI container images from releases.

## Usage

```
rebar3 release
rebar3 ocibuild -t myapp:1.0.0
```

Or push directly to a registry:

```
rebar3 ocibuild -t myapp:1.0.0 --push ghcr.io/myorg
```

## Options

  * `-t, --tag` - Image tag (e.g., myapp:1.0.0). Required.
  * `-o, --output` - Output tarball path (default: <tag>.tar.gz)
  * `-p, --push` - Push to registry (e.g., ghcr.io/myorg)
  * `-d, --desc` - Image description (OCI manifest annotation)
  * `--base` - Override base image
  * `--release` - Release name (if multiple configured)

## Configuration

Add to your `rebar.config`:

```
{ocibuild, [
    {base_image, "debian:stable-slim"},
    {workdir, "/app"},
    {env, #{~"LANG" => ~"C.UTF-8"}},
    {expose, [8080]},
    {labels, #{}},
    {description, "My awesome application"}
]}.
```

## Authentication

Set environment variables for registry authentication:

For pushing to registry:

```
export OCIBUILD_PUSH_USERNAME="user"
export OCIBUILD_PUSH_PASSWORD="pass"
```

For pulling private base images (optional, anonymous pull used if unset):

```
export OCIBUILD_PULL_USERNAME="user"
export OCIBUILD_PULL_PASSWORD="pass"
```
""".

%% Note: The provider behaviour is part of rebar3's internal API and is only
%% available at runtime when used as a rebar3 plugin. The "behaviour provider
%% undefined" warning during standalone compilation is expected and harmless.
-behaviour(provider).
-behaviour(ocibuild_adapter).

%% Provider callbacks
-export([init/1, do/1, format_error/1]).

%% Adapter callbacks (ocibuild_adapter behaviour)
-export([get_config/1, find_release/2, info/2, console/2, error/2]).
%% Optional adapter callback
-export([get_app_version/1]).

%% Exports for testing
-ifdef(TEST).
-export([find_relx_release/1, get_base_image/2]).
-endif.

-define(PROVIDER, ocibuild).
-define(DEPS, [release]).
-define(DEFAULT_BASE_IMAGE, ~"debian:stable-slim").
-define(DEFAULT_WORKDIR, ~"/app").

%%%===================================================================
%%% Provider callbacks
%%%===================================================================

-doc "Initialize the provider and register CLI options.".
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {desc, "Build OCI container images from Erlang releases"},
            {short_desc, "Build OCI images"},
            {example, "rebar3 ocibuild -t myapp:1.0.0"},
            {opts, [
                {tag, $t, "tag", string, "Image tag (e.g., myapp:1.0.0)"},
                {output, $o, "output", string, "Output tarball path"},
                {push, $p, "push", string, "Push to registry (e.g., ghcr.io/myorg)"},
                {base, undefined, "base", string, "Override base image"},
                {release, undefined, "release", string, "Release name (if multiple)"},
                {desc, $d, "desc", string, "Image description (manifest annotation)"},
                {chunk_size, undefined, "chunk-size", integer,
                    "Chunk size in MB for uploads (default: 5)"},
                {platform, $P, "platform", string,
                    "Target platforms (e.g., linux/amd64,linux/arm64)"},
                {uid, undefined, "uid", integer, "User ID to run as (default: 65534)"},
                {no_vcs_annotations, undefined, "no-vcs-annotations", boolean,
                    "Disable automatic VCS annotations"}
            ]},
            {profiles, [default, prod]}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-doc "Execute the provider - build OCI image from release.".
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, term()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Config = rebar_state:get(State, ocibuild, []),

    %% Get tag (required)
    case proplists:get_value(tag, Args) of
        undefined ->
            {error, {?MODULE, missing_tag}};
        Tag ->
            do_build(State, Args, Config, list_to_binary(Tag))
    end.

-doc "Format error messages for display.".
-spec format_error(term()) -> iolist().
format_error(missing_tag) ->
    "Missing required --tag (-t) option. Usage: rebar3 ocibuild -t myapp:1.0.0";
format_error({release_not_found, {Name, Path}}) when is_list(Name), is_list(Path) ->
    io_lib:format("Release '~s' not found at ~s. Run 'rebar3 release' first.", [Name, Path]);
format_error({release_not_found, Reason}) ->
    io_lib:format("Failed to find release: ~p", [Reason]);
format_error({no_release_configured, RelxConfig}) ->
    io_lib:format("No release configured in rebar.config. Got: ~p", [RelxConfig]);
format_error({collect_failed, {file_read_error, Path, Reason}}) ->
    io_lib:format("Failed to read file ~s: ~p", [Path, Reason]);
format_error({collect_failed, Reason}) ->
    io_lib:format("Failed to collect release files: ~p", [Reason]);
format_error({build_failed, {base_image_failed, Reason}}) ->
    io_lib:format("Failed to fetch base image: ~p", [Reason]);
format_error({build_failed, Reason}) ->
    io_lib:format("Failed to build image: ~p", [Reason]);
format_error({save_failed, Reason}) ->
    io_lib:format("Failed to save image: ~p", [Reason]);
format_error({push_failed, Reason}) ->
    io_lib:format("Failed to push image: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("OCI build error: ~p", [Reason]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Main build logic - delegates to ocibuild_release:run/3
do_build(State, _Args, _Config, _Tag) ->
    case ocibuild_release:run(?MODULE, State, #{}) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {error, {?MODULE, Reason}}
    end.

%% @private Find release definition in relx config
find_relx_release([]) ->
    error;
find_relx_release([{release, {Name, _Vsn}, _Apps} | _]) ->
    {ok, atom_to_list(Name)};
find_relx_release([{release, {Name, _Vsn}, _Apps, _Opts} | _]) ->
    {ok, atom_to_list(Name)};
find_relx_release([_ | Rest]) ->
    find_relx_release(Rest).

%% @private Get base image from args or config (used by get_config)
get_base_image(Args, Config) ->
    case proplists:get_value(base, Args) of
        undefined ->
            proplists:get_value(base_image, Config, ?DEFAULT_BASE_IMAGE);
        Base ->
            list_to_binary(Base)
    end.

%%%===================================================================
%%% Adapter Callbacks (ocibuild_adapter behaviour)
%%%===================================================================

-doc "Get normalized configuration from rebar state.".
-spec get_config(rebar_state:t()) -> ocibuild_adapter:config().
get_config(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Config = rebar_state:get(State, ocibuild, []),
    #{
        base_image => get_base_image(Args, Config),
        workdir => proplists:get_value(workdir, Config, ?DEFAULT_WORKDIR),
        env => proplists:get_value(env, Config, #{}),
        expose => proplists:get_value(expose, Config, []),
        labels => proplists:get_value(labels, Config, #{}),
        cmd => ~"foreground",
        description => get_description(Args, Config),
        tag => get_tag(Args),
        output => get_output(Args),
        push => get_push_registry(Args),
        chunk_size => get_chunk_size(Args),
        platform => get_platform(Args, Config),
        uid => get_uid(Args, Config),
        app_version => get_app_version(State),
        vcs_annotations => get_vcs_annotations(Args, Config)
    }.

%% @private Get description from args or config
get_description(Args, Config) ->
    case proplists:get_value(desc, Args) of
        undefined ->
            case proplists:get_value(description, Config) of
                undefined -> undefined;
                Descr -> list_to_binary(Descr)
            end;
        Descr ->
            list_to_binary(Descr)
    end.

%% @private Get tag from args
get_tag(Args) ->
    case proplists:get_value(tag, Args) of
        undefined -> undefined;
        Tag -> list_to_binary(Tag)
    end.

%% @private Get output path from args
get_output(Args) ->
    case proplists:get_value(output, Args) of
        undefined -> undefined;
        Path -> list_to_binary(Path)
    end.

%% @private Get push registry from args
get_push_registry(Args) ->
    case proplists:get_value(push, Args) of
        undefined -> undefined;
        Registry -> list_to_binary(Registry)
    end.

%% @private Get chunk size from args (validated to MIN-MAX MB, falls back to default)
get_chunk_size(Args) ->
    MinMB = ocibuild_adapter:min_chunk_size_mb(),
    MaxMB = ocibuild_adapter:max_chunk_size_mb(),
    case proplists:get_value(chunk_size, Args) of
        undefined ->
            undefined;
        Size when is_integer(Size), Size >= MinMB, Size =< MaxMB ->
            Size * 1024 * 1024;
        Size ->
            io:format(
                "Warning: chunk_size ~p MB out of range (~B-~B), using default (~B MB)~n",
                [Size, MinMB, MaxMB, ocibuild_adapter:default_chunk_size_mb()]
            ),
            ocibuild_adapter:default_chunk_size()
    end.

%% @private Get platform(s) from args or config
get_platform(Args, Config) ->
    case proplists:get_value(platform, Args) of
        undefined ->
            %% Check config for platform setting
            case proplists:get_value(platform, Config) of
                undefined -> undefined;
                Platform when is_list(Platform) -> list_to_binary(Platform);
                Platform when is_binary(Platform) -> Platform
            end;
        Platform ->
            list_to_binary(Platform)
    end.

%% @private Get uid from args or config (default applied in ocibuild_release)
get_uid(Args, Config) ->
    case proplists:get_value(uid, Args) of
        undefined -> proplists:get_value(uid, Config);
        Uid -> Uid
    end.

%% @private Get VCS annotations setting: CLI flag takes precedence, then config, then default true
get_vcs_annotations(Args, Config) ->
    case proplists:get_value(no_vcs_annotations, Args) of
        true ->
            %% CLI --no-vcs-annotations disables VCS annotations
            false;
        _ ->
            %% Check config, default to true
            proplists:get_value(vcs_annotations, Config, true)
    end.

-doc """
Get application version from rebar state.

Extracts the version from the first release in the relx configuration.
This is used for the `org.opencontainers.image.version` annotation.
""".
-spec get_app_version(rebar_state:t()) -> binary() | undefined.
get_app_version(State) ->
    RelxConfig = rebar_state:get(State, relx, []),
    get_version_from_relx(RelxConfig).

%% @private Extract version from relx config
get_version_from_relx([]) ->
    undefined;
get_version_from_relx([{release, {_Name, Vsn}, _Apps} | _]) ->
    normalize_version(Vsn);
get_version_from_relx([{release, {_Name, Vsn}, _Apps, _Opts} | _]) ->
    normalize_version(Vsn);
get_version_from_relx([_ | Rest]) ->
    get_version_from_relx(Rest).

%% @private Normalize version to binary
normalize_version(Vsn) when is_list(Vsn) ->
    list_to_binary(Vsn);
normalize_version(Vsn) when is_binary(Vsn) ->
    Vsn;
normalize_version(Vsn) when is_atom(Vsn) ->
    %% Handle relx version atoms like 'semver' or 'git' (symbolic refs)
    atom_to_binary(Vsn);
normalize_version(_) ->
    undefined.

-doc "Find release directory from rebar state.".
-spec find_release(rebar_state:t(), map()) ->
    {ok, binary(), file:filename()} | {error, term()}.
find_release(State, Opts) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    RelxConfig = rebar_state:get(State, relx, []),

    %% Get release name from opts, args, or config
    ReleaseName =
        case maps:get(release, Opts, undefined) of
            undefined -> proplists:get_value(release, Args);
            OptName -> OptName
        end,

    case get_release_name_internal(ReleaseName, RelxConfig) of
        {ok, ResolvedName} ->
            BaseDir = rebar_dir:base_dir(State),
            ReleasePath = filename:join([BaseDir, "rel", ResolvedName]),
            case filelib:is_dir(ReleasePath) of
                true ->
                    {ok, list_to_binary(ResolvedName), ReleasePath};
                false ->
                    {error, {release_not_found, {ResolvedName, ReleasePath}}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Internal release name lookup
get_release_name_internal(undefined, RelxConfig) ->
    case find_relx_release(RelxConfig) of
        {ok, Name} -> {ok, Name};
        error -> {error, {no_release_configured, RelxConfig}}
    end;
get_release_name_internal(Name, _RelxConfig) ->
    {ok, Name}.

-doc "Log an informational message using rebar_api.".
-spec info(io:format(), [term()]) -> ok.
info(Format, Args) ->
    rebar_api:info(Format, Args).

-doc "Print a message to the console using rebar_api.".
-spec console(io:format(), [term()]) -> ok.
console(Format, Args) ->
    rebar_api:console(Format, Args).

-doc "Log an error message using rebar_api.".
-spec error(io:format(), [term()]) -> ok.
error(Format, Args) ->
    rebar_api:error(Format, Args).
