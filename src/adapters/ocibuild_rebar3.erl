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

  * `-t, --tag` - Image tag (e.g., myapp:1.0.0). Can be specified multiple times.
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
    {description, "My awesome application"},
    {tag, ["myapp:1.0.0", "myapp:latest"]}  % or a single string: "myapp:1.0.0"
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

%% eqWalizer suppressions - rebar3 internals (provider, rebar_state, rebar_api)
%% are only available at runtime when loaded as a plugin.
-eqwalizer({nowarn_function, init/1}).
-eqwalizer({nowarn_function, do/1}).
-eqwalizer({nowarn_function, do_push_tarball/3}).
-eqwalizer({nowarn_function, get_config/1}).
-eqwalizer({nowarn_function, get_app_version/1}).
-eqwalizer({nowarn_function, get_dependencies/1}).
-eqwalizer({nowarn_function, find_release/2}).
-eqwalizer({nowarn_function, get_base_image/2}).
-eqwalizer({nowarn_function, get_app_name/2}).
-eqwalizer({nowarn_function, get_project_app_name/1}).
-eqwalizer({nowarn_function, get_description/2}).
-eqwalizer({nowarn_function, get_tags/2}).
-eqwalizer({nowarn_function, get_output/1}).
-eqwalizer({nowarn_function, get_sbom_path/1}).
-eqwalizer({nowarn_function, get_sign_key/2}).
-eqwalizer({nowarn_function, get_push_registry/1}).
-eqwalizer({nowarn_function, get_platform/2}).
-eqwalizer({nowarn_function, parse_rebar_lock/1}).

%% Provider callbacks
-export([init/1, do/1, format_error/1]).

%% Adapter callbacks (ocibuild_adapter behaviour)
-export([get_config/1, find_release/2, info/2, console/2, error/2]).
%% Optional adapter callbacks
-export([get_app_version/1, get_dependencies/1]).

%% Exports for testing
-ifdef(TEST).
-export([find_relx_release/1, get_base_image/2, parse_rebar_lock/1]).
-export([get_tags/2, normalize_tags/1, normalize_tag/1]).
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
                {tag, $t, "tag", string, "Image tag (repeatable, e.g., -t myapp:1.0.0 -t myapp:latest)"},
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
                    "Disable automatic VCS annotations"},
                {sbom, undefined, "sbom", string, "Export SBOM to file path"},
                {sign_key, undefined, "sign-key", string,
                    "Path to cosign private key for image signing"}
            ]},
            {profiles, [default, prod]}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-doc "Execute the provider - build OCI image from release.".
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, term()}.
do(State) ->
    {Args, Rest} = rebar_state:command_parsed_args(State),
    Config = rebar_state:get(State, ocibuild, []),

    %% Check for tarball argument (push existing image mode)
    PushRegistry = get_push_registry(Args),
    TarballPath = detect_tarball_arg(Rest),

    Tags = get_tags(Args, Config),
    case {PushRegistry, TarballPath} of
        {undefined, _} ->
            %% No push registry - normal build mode (at least one tag required)
            case Tags of
                [] ->
                    {error, {?MODULE, missing_tag}};
                _ ->
                    do_build(State, Args, Config)
            end;
        {_Registry, {ok, Path}} ->
            %% Push tarball mode (standalone, no release needed)
            do_push_tarball(State, Args, Path);
        {_Registry, undefined} ->
            %% Build and push mode (at least one tag required)
            case Tags of
                [] ->
                    {error, {?MODULE, missing_tag}};
                _ ->
                    do_build(State, Args, Config)
            end
    end.

%% Detect tarball path from positional arguments
-spec detect_tarball_arg([string()]) -> {ok, string()} | undefined.
detect_tarball_arg([Path | _]) when is_list(Path) ->
    Ext = filename:extension(Path),
    case lists:member(Ext, [".gz", ".tar", ".tgz"]) of
        true -> {ok, Path};
        false -> undefined
    end;
detect_tarball_arg(_) ->
    undefined.

%% Push existing tarball to registry
-spec do_push_tarball(rebar_state:t(), list(), string()) ->
    {ok, rebar_state:t()} | {error, term()}.
do_push_tarball(State, Args, TarballPath) ->
    Config = rebar_state:get(State, ocibuild, []),
    PushRegistry = get_push_registry(Args),
    Tags = get_tags(Args, Config),
    ChunkSize = get_chunk_size(Args),

    Opts = #{
        registry => PushRegistry,
        tags => Tags,
        chunk_size => ChunkSize
    },

    case ocibuild_release:push_tarball(?MODULE, State, TarballPath, Opts) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {error, {?MODULE, Reason}}
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
do_build(State, _Args, _Config) ->
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
    case proplists:get_value(base, Args, proplists:get_value(base_image, Config)) of
        undefined ->
            ?DEFAULT_BASE_IMAGE;
        BaseImage when is_list(BaseImage) ->
            list_to_binary(BaseImage);
        BaseImage when is_binary(BaseImage) ->
            BaseImage
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
        tags => get_tags(Args, Config),
        output => get_output(Args),
        push => get_push_registry(Args),
        chunk_size => get_chunk_size(Args),
        platform => get_platform(Args, Config),
        uid => get_uid(Args, Config),
        app_version => get_app_version(State),
        %% app_name for layer classification - if not set, falls back to release_name
        %% In Erlang, release name usually matches app name, but can be set explicitly
        app_name => get_app_name(State, Config),
        vcs_annotations => get_vcs_annotations(Args, Config),
        sbom => get_sbom_path(Args),
        sign_key => get_sign_key(Args, Config)
    }.

%% @private Get app_name for layer classification
%% Tries: explicit config -> project apps -> undefined (falls back to release_name)
get_app_name(State, Config) ->
    case proplists:get_value(app_name, Config) of
        undefined ->
            %% Try to get from rebar_state project_apps
            get_project_app_name(State);
        Name when is_atom(Name) ->
            atom_to_binary(Name, utf8);
        Name when is_list(Name) ->
            list_to_binary(Name);
        Name when is_binary(Name) ->
            Name
    end.

%% @private Get the primary project application name from rebar_state
%% Returns the first (typically main) app from project_apps, or undefined
get_project_app_name(State) ->
    case rebar_state:project_apps(State) of
        [] ->
            undefined;
        [AppInfo | _] ->
            rebar_app_info:name(AppInfo)
    end.

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

%% @private Get tags as a list of binaries from args or config
%% Args: collects all values for `tag` (supports one or many -t flags)
%% Config: `tag` can be a single string/binary or a list of such values
get_tags(Args, Config) ->
    case proplists:get_all_values(tag, Args) of
        [] ->
            %% No CLI tags, check config (and normalize to binary list)
            normalize_tags(proplists:get_value(tag, Config, []));
        TagStrs ->
            %% CLI tags (all -t occurrences) override config
            [list_to_binary(T) || T <- TagStrs]
    end.

%% @private Normalize tags from config to binary list
%% Handles both single tag (string) and list of tags
normalize_tags(Tag) when is_binary(Tag) ->
    [Tag];
normalize_tags([]) ->
    [];
normalize_tags(Tag) when is_list(Tag) ->
    case io_lib:char_list(Tag) of
        true ->
            %% Single string tag (charlist)
            [list_to_binary(Tag)];
        false ->
            %% List of tags
            [normalize_tag(T) || T <- Tag]
    end;
normalize_tags(_) ->
    [].

%% @private Normalize a single tag to binary
normalize_tag(T) when is_binary(T) -> T;
normalize_tag(T) when is_list(T) -> list_to_binary(T);
normalize_tag(T) when is_atom(T) -> atom_to_binary(T, utf8).

%% @private Get output path from args
get_output(Args) ->
    case proplists:get_value(output, Args) of
        undefined -> undefined;
        Path -> list_to_binary(Path)
    end.

%% @private Get SBOM export path from args
get_sbom_path(Args) ->
    case proplists:get_value(sbom, Args) of
        undefined -> undefined;
        Path -> list_to_binary(Path)
    end.

%% @private Get sign key path from args, env, or config
%% Priority: CLI --sign-key > OCIBUILD_SIGN_KEY env > config sign_key
get_sign_key(Args, Config) ->
    case proplists:get_value(sign_key, Args) of
        undefined ->
            case os:getenv("OCIBUILD_SIGN_KEY") of
                false ->
                    case proplists:get_value(sign_key, Config) of
                        undefined -> undefined;
                        Path when is_list(Path) -> list_to_binary(Path);
                        Path when is_binary(Path) -> Path
                    end;
                EnvPath ->
                    list_to_binary(EnvPath)
            end;
        Path ->
            list_to_binary(Path)
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
    atom_to_binary(Vsn, utf8);
normalize_version(_) ->
    undefined.

-doc """
Get dependencies from rebar.lock file.

Parses the rebar.lock file to extract dependency information including
name, version, and source (hex or git). This data is used for:
1. Smart layer classification (dependencies vs application code)
2. Future SBOM generation

Supports both old format (list only) and new format (version tuple + list).
""".
-spec get_dependencies(rebar_state:t()) ->
    {ok, [#{name := binary(), version := binary(), source := binary()}]}
    | {error, term()}.
get_dependencies(State) ->
    ProjectDir = rebar_state:dir(State),
    LockFile = filename:join(ProjectDir, "rebar.lock"),
    parse_rebar_lock(LockFile).

%% @private Parse rebar.lock file
-spec parse_rebar_lock(file:filename()) ->
    {ok, [#{name := binary(), version := binary(), source := binary()}]}
    | {error, term()}.
parse_rebar_lock(LockFile) ->
    case file:consult(LockFile) of
        {ok, []} ->
            {ok, []};
        {ok, [{_Version, Deps} | _Rest]} when is_list(Deps) ->
            %% New format: {"1.2.0", [{~"pkg", {pkg, ~"pkg", ~"1.0.0"}, 0}, ...]}
            {ok, [parse_dep_entry(D) || D <- Deps]};
        {ok, [Deps | _Rest]} when is_list(Deps) ->
            %% Old format: [{~"pkg", {pkg, ~"pkg", ~"1.0.0"}, 0}, ...]
            {ok, [parse_dep_entry(D) || D <- Deps]};
        {error, enoent} ->
            %% No lock file - no dependencies
            {ok, []};
        {error, Reason} ->
            {error, {lock_parse_failed, Reason}}
    end.

%% @private Parse a single dependency entry from rebar.lock
-spec parse_dep_entry(tuple()) ->
    #{name := binary(), version := binary(), source := binary()}.
parse_dep_entry({Name, {pkg, _PkgName, Version}, _Depth}) ->
    #{name => Name, version => Version, source => ~"hex"};
parse_dep_entry({Name, {git, Url, {ref, Ref}}, _Depth}) ->
    #{name => Name, version => to_binary(Ref), source => to_binary(Url)};
parse_dep_entry({Name, {git, Url, {tag, Tag}}, _Depth}) ->
    #{name => Name, version => to_binary(Tag), source => to_binary(Url)};
parse_dep_entry({Name, {git, Url, {branch, Branch}}, _Depth}) ->
    #{name => Name, version => to_binary(Branch), source => to_binary(Url)};
parse_dep_entry({Name, _, _Depth}) ->
    #{name => Name, version => ~"unknown", source => ~"unknown"}.

%% @private Convert to binary
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8).

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
