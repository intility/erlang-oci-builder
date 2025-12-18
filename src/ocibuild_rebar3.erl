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
    {base_image, "debian:slim"},
    {workdir, "/app"},
    {env, #{<<"LANG">> => <<"C.UTF-8">>}},
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

%% Legacy exports for backward compatibility (delegate to ocibuild_release)
-export([collect_release_files/1, build_image/7, build_image/8]).
-export([get_push_auth/0, get_pull_auth/0]).
-export([
    make_progress_callback/0, format_progress/2, format_bytes/1, is_tty/0, clear_progress_line/0
]).

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
                    "Chunk size in MB for uploads (default: 5)"}
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
format_error({release_not_found, Name, Path}) ->
    io_lib:format("Release '~s' not found at ~s. Run 'rebar3 release' first.", [Name, Path]);
format_error({no_release_configured, RelxConfig}) ->
    io_lib:format("No release configured in rebar.config. Got: ~p", [RelxConfig]);
format_error({file_read_error, Path, Reason}) ->
    io_lib:format("Failed to read file ~s: ~p", [Path, Reason]);
format_error({save_failed, Reason}) ->
    io_lib:format("Failed to save image: ~p", [Reason]);
format_error({push_failed, Reason}) ->
    io_lib:format("Failed to push image: ~p", [Reason]);
format_error({base_image_failed, Reason}) ->
    io_lib:format("Failed to fetch base image: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("OCI build error: ~p", [Reason]).

%%%===================================================================
%%% Delegated API (for backward compatibility with Mix tasks)
%%%===================================================================

-doc "Collect all files from a release directory. Delegates to ocibuild_release.".
-spec collect_release_files(file:filename()) ->
    {ok, [{binary(), binary(), non_neg_integer()}]} | {error, term()}.
collect_release_files(ReleasePath) ->
    ocibuild_release:collect_release_files(ReleasePath).

-doc "Build an OCI image from release files. Delegates to ocibuild_release.".
-spec build_image(
    binary(),
    [{binary(), binary(), non_neg_integer()}],
    string() | binary(),
    binary(),
    map(),
    [non_neg_integer()],
    map()
) -> {ok, ocibuild:image()} | {error, term()}.
build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels) ->
    build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, ~"foreground").

-doc "Build an OCI image from release files with custom start command. Delegates to ocibuild_release.".
-spec build_image(
    binary(),
    [{binary(), binary(), non_neg_integer()}],
    string() | binary(),
    binary(),
    map(),
    [non_neg_integer()],
    map(),
    binary()
) -> {ok, ocibuild:image()} | {error, term()}.
build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd) ->
    %% Create progress callback for terminal display
    ProgressFn = make_progress_callback(),
    PullAuth = get_pull_auth(),
    Opts = #{auth => PullAuth, progress => ProgressFn},
    Result = ocibuild_release:build_image(
        BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd, Opts
    ),
    %% Clear progress line after build (only in TTY mode)
    clear_progress_line(),
    Result.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Main build logic
do_build(State, _Args, Config, Tag) ->
    rebar_api:info("Building OCI image: ~s", [Tag]),

    %% Find release using adapter callback
    case find_release(State, #{}) of
        {ok, ReleaseName, ReleasePath} ->
            rebar_api:info("Using release: ~s at ~s", [ReleaseName, ReleasePath]),

            %% Collect files from release
            case collect_release_files(ReleasePath) of
                {ok, Files} ->
                    rebar_api:info("Collected ~p files from release", [length(Files)]),
                    build_and_output(State, Config, Tag, ReleaseName, Files);
                {error, Reason} ->
                    {error, {?MODULE, Reason}}
            end;
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

%% @private Build image and output
build_and_output(State, _Config, Tag, ReleaseName, Files) ->
    %% Get normalized configuration from adapter
    AdapterConfig = get_config(State),
    #{
        base_image := BaseImage,
        workdir := Workdir,
        env := EnvMap,
        expose := ExposePorts,
        labels := Labels,
        description := Description
    } = AdapterConfig,

    rebar_api:info("Base image: ~s", [BaseImage]),

    %% Build the image
    case build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels) of
        {ok, Image0} ->
            %% Add description annotation if provided
            Image = ocibuild_release:add_description(Image0, Description),
            output_image(State, Tag, Image);
        {error, Reason} ->
            {error, {?MODULE, Reason}}
    end.

%% @private Get base image from args or config (used by get_config)
get_base_image(Args, Config) ->
    case proplists:get_value(base, Args) of
        undefined ->
            proplists:get_value(base_image, Config, ?DEFAULT_BASE_IMAGE);
        Base ->
            list_to_binary(Base)
    end.

%% @private Output the image (save and/or push)
output_image(State, Tag, Image) ->
    %% Get config from adapter
    AdapterConfig = get_config(State),
    #{output := OutputOpt, push := PushRegistry, chunk_size := ChunkSize} = AdapterConfig,

    %% Determine output path
    OutputPath =
        case OutputOpt of
            undefined ->
                %% Extract just the image name (last path segment) for the filename
                TagStr = binary_to_list(Tag),
                ImageName = lists:last(string:split(TagStr, "/", all)),
                SafeName = lists:map(
                    fun
                        ($:) -> $-;
                        (C) -> C
                    end,
                    ImageName
                ),
                SafeName ++ ".tar.gz";
            Path ->
                binary_to_list(Path)
        end,

    %% Save tarball
    rebar_api:info("Saving image to ~s", [OutputPath]),
    case ocibuild_release:save_image(Image, OutputPath, Tag) of
        ok ->
            rebar_api:info("Image saved successfully", []),

            %% Push if requested
            case PushRegistry of
                undefined ->
                    rebar_api:console("~nTo load the image:~n  podman load < ~s~n", [OutputPath]),
                    {ok, State};
                _ ->
                    push_to_registry(State, Tag, Image, PushRegistry, ChunkSize)
            end;
        {error, Reason} ->
            {error, {?MODULE, {save_failed, Reason}}}
    end.

%% @private Push image to registry
push_to_registry(State, Tag, Image, Registry, ChunkSize) ->
    %% Parse tag to get repository and tag parts
    {Repo, ImageTag} = ocibuild_release:parse_tag(Tag),

    %% Get auth from environment (for pushing)
    Auth = ocibuild_release:get_push_auth(),

    %% Build push options
    PushOpts =
        case ChunkSize of
            undefined -> #{};
            Size -> #{chunk_size => Size}
        end,

    rebar_api:info("Pushing to ~s/~s:~s", [Registry, Repo, ImageTag]),

    RepoTag = <<Repo/binary, ":", ImageTag/binary>>,
    case ocibuild_release:push_image(Image, Registry, RepoTag, Auth, PushOpts) of
        ok ->
            rebar_api:info("Push successful!", []),
            {ok, State};
        {error, Reason} ->
            {error, {?MODULE, {push_failed, Reason}}}
    end.

%%%===================================================================
%%% Legacy API (delegate to ocibuild_release for backward compatibility)
%%%===================================================================

-doc "Get authentication for pushing images. Delegates to ocibuild_release.".
-spec get_push_auth() -> map().
get_push_auth() -> ocibuild_release:get_push_auth().

-doc "Get authentication for pulling base images. Delegates to ocibuild_release.".
-spec get_pull_auth() -> map().
get_pull_auth() -> ocibuild_release:get_pull_auth().

-doc "Check if stdout is connected to a TTY. Delegates to ocibuild_release.".
-spec is_tty() -> boolean().
is_tty() -> ocibuild_release:is_tty().

-doc "Clear the progress line if in TTY mode. Delegates to ocibuild_release.".
-spec clear_progress_line() -> ok.
clear_progress_line() -> ocibuild_release:clear_progress_line().

-doc "Create a progress callback. Delegates to ocibuild_release.".
-spec make_progress_callback() -> ocibuild_registry:progress_callback().
make_progress_callback() -> ocibuild_release:make_progress_callback().

-doc "Format progress as a string with bar. Delegates to ocibuild_release.".
format_progress(Received, Total) -> ocibuild_release:format_progress(Received, Total).

-doc "Format bytes as human-readable string. Delegates to ocibuild_release.".
format_bytes(Bytes) -> ocibuild_release:format_bytes(Bytes).

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
        chunk_size => get_chunk_size(Args)
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

%% @private Get chunk size from args
get_chunk_size(Args) ->
    case proplists:get_value(chunk_size, Args) of
        undefined -> undefined;
        Size -> Size * 1024 * 1024
    end.

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
                    {error, {release_not_found, ResolvedName, ReleasePath}}
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
