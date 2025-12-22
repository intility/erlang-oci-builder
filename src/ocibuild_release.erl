%%%-------------------------------------------------------------------
-module(ocibuild_release).
-feature(maybe_expr, enable).
-moduledoc """
Shared release handling for OCI image building.

This module provides common functionality for collecting release files
and building OCI images, used by both rebar3 and Mix integrations.

Security features:
- Symlinks pointing outside the release directory are rejected
- Broken symlinks are skipped with a warning
- Path traversal via `..` components is prevented
""".

-include_lib("kernel/include/file.hrl").

%% Public API - High-level Orchestration
-export([run/3]).

%% Public API - File Collection
-export([
    collect_release_files/1,
    collect_release_files/2
]).

%% Public API - Image Building
-export([
    build_image/2,
    build_image/3
]).

%% Public API - Output Operations (save/push)
-export([
    save_image/3,
    push_image/5,
    parse_tag/1,
    add_description/2
]).

%% Public API - Authentication
-export([
    get_push_auth/0,
    get_pull_auth/0
]).

%% Public API - Progress Display
-export([
    make_progress_callback/0,
    format_progress/2,
    format_bytes/1,
    is_tty/0,
    clear_progress_line/0
]).

%% Public API - Cleanup
-export([
    stop_httpc/0
]).

%% Public API - Multi-platform Validation
-export([
    has_bundled_erts/1,
    check_for_native_code/1,
    validate_multiplatform/2
]).

%% Utility exports (used by build tools)
-export([
    to_binary/1,
    to_container_path/1,
    get_file_mode/1,
    make_relative_path/2,
    is_nil_or_undefined/1
]).

%% Exports for testing
-ifdef(TEST).
-export([
    strip_prefix/2,
    normalize_path/1,
    is_path_within/2,
    validate_symlink_target/2
]).
-endif.

-define(DEFAULT_WORKDIR, ~"/app").

%%%===================================================================
%%% High-level Orchestration
%%%===================================================================

-doc """
Run the complete OCI image build pipeline.

This function orchestrates the entire build process using the adapter module
for build-system-specific operations (configuration, release finding, logging).

The adapter module must implement the `ocibuild_adapter` behaviour:
- `get_config/1` - Extract configuration from build system state
- `find_release/2` - Locate the release directory
- `info/2`, `console/2`, `error/2` - Logging functions

Options:
- `cmd` - Start command override (default: from adapter config or "foreground")

Returns `{ok, State}` on success (State passed through from adapter),
or `{error, Reason}` on failure.
""".
-spec run(module(), term(), map()) -> {ok, term()} | {error, term()}.
run(AdapterModule, AdapterState, Opts) ->
    %% Get configuration from adapter
    Config = AdapterModule:get_config(AdapterState),
    #{
        base_image := BaseImage,
        workdir := Workdir,
        env := EnvMap,
        expose := ExposePorts,
        labels := Labels,
        cmd := DefaultCmd,
        description := Description,
        tag := Tag,
        output := OutputOpt,
        push := PushRegistry,
        chunk_size := ChunkSize
    } = Config,

    %% Get optional platform configuration
    PlatformOpt = maps:get(platform, Config, undefined),

    maybe
        %% Validate tag exists
        true ?= Tag =/= undefined,
        AdapterModule:info("Building OCI image: ~s", [Tag]),
        %% Find release
        {ok, ReleaseName, ReleasePath} ?= AdapterModule:find_release(AdapterState, Opts),
        AdapterModule:info("Using release: ~s at ~s", [ReleaseName, ReleasePath]),
        %% Parse and validate platforms
        {ok, Platforms} ?= parse_platform_option(PlatformOpt),
        ok ?= validate_platform_requirements(AdapterModule, ReleasePath, Platforms),
        %% Collect release files
        {ok, Files} ?= collect_release_files(ReleasePath),
        AdapterModule:info("Collected ~p files from release", [length(Files)]),
        %% Build image(s)
        AdapterModule:info("Base image: ~s", [BaseImage]),
        Cmd = maps:get(cmd, Opts, DefaultCmd),
        ProgressFn = make_progress_callback(),
        PullAuth = get_pull_auth(),
        BuildOpts = #{
            release_name => ReleaseName,
            workdir => Workdir,
            env => EnvMap,
            expose => ExposePorts,
            labels => Labels,
            cmd => Cmd,
            description => Description,
            auth => PullAuth,
            progress => ProgressFn
        },
        {ok, Images} ?= build_platform_images(BaseImage, Files, Platforms, BuildOpts),
        clear_progress_line(),
        %% Output the image(s)
        OutputOpts = #{
            tag => Tag,
            output => OutputOpt,
            push => PushRegistry,
            chunk_size => ChunkSize,
            platforms => Platforms,
            progress => ProgressFn
        },
        do_output(AdapterModule, AdapterState, Images, OutputOpts)
    else
        false ->
            {error, missing_tag};
        {error, {invalid_platform_value, _} = Reason} ->
            {error, {invalid_platform, Reason}};
        {error, {invalid_platform, _} = Reason} ->
            {error, Reason};
        {error, {bundled_erts, _} = Reason} ->
            {error, Reason};
        {error, Reason} when is_atom(Reason) ->
            {error, {release_not_found, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Parse platform option string into list of platforms
-spec parse_platform_option(binary() | undefined | nil) ->
    {ok, [ocibuild:platform()]} | {error, term()}.
parse_platform_option(undefined) ->
    {ok, []};
parse_platform_option(nil) ->
    {ok, []};
parse_platform_option(<<>>) ->
    {ok, []};
parse_platform_option(PlatformStr) when is_binary(PlatformStr) ->
    ocibuild:parse_platforms(PlatformStr);
parse_platform_option(Value) ->
    {error, {invalid_platform_value, Value}}.

%% @private Validate platform requirements (ERTS check, NIF warning)
-spec validate_platform_requirements(module(), file:filename(), [ocibuild:platform()]) ->
    ok | {error, term()}.
validate_platform_requirements(_AdapterModule, _ReleasePath, []) ->
    %% No platforms specified, skip validation
    ok;
validate_platform_requirements(_AdapterModule, _ReleasePath, [_SinglePlatform]) ->
    %% Single platform, no multi-platform validation needed
    ok;
validate_platform_requirements(_AdapterModule, ReleasePath, Platforms) when length(Platforms) > 1 ->
    %% Multi-platform: validate ERTS and warn about NIFs
    %% Note: validate_multiplatform handles NIF warnings internally
    validate_multiplatform(ReleasePath, Platforms).

%% @private Build image(s) for specified platforms
%%
%% Opts must contain: release_name, workdir, env, expose, labels, cmd
%% Optional: description, auth, progress
-spec build_platform_images(
    BaseImage :: binary(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    Platforms :: [ocibuild:platform()],
    Opts :: map()
) -> {ok, ocibuild:image() | [ocibuild:image()]} | {error, term()}.
build_platform_images(BaseImage, Files, [], Opts) ->
    %% No platforms specified - single platform build
    build_single_platform_image(BaseImage, Files, Opts);
build_platform_images(BaseImage, Files, [_SinglePlatform], Opts) ->
    %% Single platform specified - use single platform build path
    build_single_platform_image(BaseImage, Files, Opts);
build_platform_images(BaseImage, Files, Platforms, Opts) when length(Platforms) > 1 ->
    %% Multi-platform build - use ocibuild:from/3 with platforms option
    PullAuth = maps:get(auth, Opts, #{}),
    ProgressFn = maps:get(progress, Opts, fun(_) -> ok end),

    %% Pass platform maps via the platforms option in ocibuild:from/3
    case ocibuild:from(BaseImage, PullAuth, #{progress => ProgressFn, platforms => Platforms}) of
        {ok, BaseImages} when is_list(BaseImages) ->
            %% Apply release files and configuration to each platform image
            ConfiguredImages = [
                configure_release_image(BaseImg, Files, Opts)
             || BaseImg <- BaseImages
            ],
            {ok, ConfiguredImages};
        {ok, SingleImage} ->
            %% Fallback: got single image, configure it
            Image = configure_release_image(SingleImage, Files, Opts),
            {ok, [Image]};
        {error, _} = Error ->
            Error
    end.

%% @private Build a single platform image using build_image/3
-spec build_single_platform_image(
    BaseImage :: binary(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    Opts :: map()
) -> {ok, ocibuild:image()} | {error, term()}.
build_single_platform_image(BaseImage, Files, Opts) ->
    Description = maps:get(description, Opts, undefined),
    case build_image(BaseImage, Files, Opts) of
        {ok, Image0} ->
            Image = add_description(Image0, Description),
            {ok, Image};
        Error ->
            Error
    end.

%% @private Format platform map as string
-spec format_platform(ocibuild:platform()) -> binary().
format_platform(#{os := OS, architecture := Arch} = Platform) ->
    case maps:get(variant, Platform, undefined) of
        undefined -> <<OS/binary, "/", Arch/binary>>;
        Variant -> <<OS/binary, "/", Arch/binary, "/", Variant/binary>>
    end.

%% @private Configure a release image with files and settings
%%
%% Opts: release_name, workdir, env, expose, labels, cmd, description
-spec configure_release_image(
    Image :: ocibuild:image(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    Opts :: map()
) -> ocibuild:image().
configure_release_image(Image0, Files, Opts) ->
    ReleaseName = maps:get(release_name, Opts, <<"app">>),
    Workdir = maps:get(workdir, Opts, <<"/app">>),
    EnvMap = maps:get(env, Opts, #{}),
    ExposePorts = maps:get(expose, Opts, []),
    Labels = maps:get(labels, Opts, #{}),
    Cmd = maps:get(cmd, Opts, <<"foreground">>),
    Description = maps:get(description, Opts, undefined),

    %% Add release layer
    Image1 = ocibuild:add_layer(Image0, Files),

    %% Set working directory
    Image2 = ocibuild:workdir(Image1, Workdir),

    %% Set entrypoint
    ReleaseNameBin = to_binary(ReleaseName),
    EntrypointPath = <<Workdir/binary, "/bin/", ReleaseNameBin/binary>>,
    Image3 = ocibuild:entrypoint(Image2, [EntrypointPath, Cmd]),

    %% Set environment variables
    Image4 = ocibuild:env(Image3, EnvMap),

    %% Expose ports
    Image5 = lists:foldl(fun(Port, Img) -> ocibuild:expose(Img, Port) end, Image4, ExposePorts),

    %% Add labels
    Image6 = maps:fold(fun(K, V, Img) -> ocibuild:label(Img, K, V) end, Image5, Labels),

    %% Add description annotation
    add_description(Image6, Description).

%% @private Output the image (save and optionally push)
%% Handles both single image and list of images (multi-platform)
%%
%% Opts: tag, output, push, chunk_size, platforms
-spec do_output(
    AdapterModule :: module(),
    AdapterState :: term(),
    Images :: ocibuild:image() | [ocibuild:image()],
    Opts :: map()
) -> {ok, term()} | {error, term()}.
do_output(AdapterModule, AdapterState, Images, Opts) ->
    Tag = maps:get(tag, Opts),
    OutputOpt = maps:get(output, Opts, undefined),
    PushRegistry = maps:get(push, Opts, undefined),
    ChunkSize = maps:get(chunk_size, Opts, undefined),
    Platforms = maps:get(platforms, Opts, []),

    %% Determine output path (handle both Erlang undefined and Elixir nil)
    OutputPath =
        case is_nil_or_undefined(OutputOpt) of
            true -> default_output_path(Tag);
            false -> binary_to_list(OutputOpt)
        end,

    %% Determine if this is multi-platform
    IsMultiPlatform = is_list(Images) andalso length(Images) > 1,

    %% Save tarball
    case IsMultiPlatform of
        true ->
            AdapterModule:info("Saving multi-platform image to ~s", [OutputPath]),
            AdapterModule:info("Platforms: ~s", [format_platform_list(Platforms)]);
        false ->
            AdapterModule:info("Saving image to ~s", [OutputPath])
    end,

    %% For multi-platform, use ocibuild:save with list of images
    %% For single platform, use single image
    SaveOpts = #{tag => Tag, progress => maps:get(progress, Opts, undefined)},
    SaveResult =
        case Images of
            [SingleImage] ->
                save_image(SingleImage, OutputPath, SaveOpts);
            ImageList when is_list(ImageList) ->
                save_multi_image(ImageList, OutputPath, SaveOpts);
            SingleImage ->
                save_image(SingleImage, OutputPath, SaveOpts)
        end,

    case SaveResult of
        ok ->
            AdapterModule:info("Image saved successfully", []),

            %% Push if requested (handle both Erlang undefined and Elixir nil)
            case is_nil_or_undefined(PushRegistry) orelse PushRegistry =:= <<>> of
                true ->
                    AdapterModule:console("~nTo load the image:~n  podman load < ~s~n", [OutputPath]),
                    {ok, AdapterState};
                false ->
                    PushOpts = #{chunk_size => ChunkSize},
                    do_push(AdapterModule, AdapterState, Images, Tag, PushRegistry, PushOpts)
            end;
        {error, SaveError} ->
            {error, {save_failed, SaveError}}
    end.

%% @private Format platform list for display
-spec format_platform_list([ocibuild:platform()]) -> string().
format_platform_list(Platforms) ->
    PlatformStrs = [binary_to_list(format_platform(P)) || P <- Platforms],
    string:join(PlatformStrs, ", ").

%% @private Save multi-platform image
-spec save_multi_image([ocibuild:image()], string(), map()) -> ok | {error, term()}.
save_multi_image(Images, OutputPath, Opts) ->
    Tag = maps:get(tag, Opts, <<"latest">>),
    ProgressFn = maps:get(progress, Opts, undefined),
    SaveOpts = #{tag => Tag, progress => ProgressFn},
    ocibuild:save(Images, list_to_binary(OutputPath), SaveOpts).

%% @private Generate default output path from tag
default_output_path(Tag) ->
    TagStr = binary_to_list(Tag),
    ImageName = lists:last(string:split(TagStr, "/", all)),
    SafeName = lists:map(
        fun
            ($:) -> $-;
            (C) -> C
        end,
        ImageName
    ),
    SafeName ++ ".tar.gz".

%% @private Push image(s) to registry
%% Handles both single image and list of images (multi-platform)
%%
%% Opts: chunk_size
-spec do_push(
    AdapterModule :: module(),
    AdapterState :: term(),
    Images :: ocibuild:image() | [ocibuild:image()],
    Tag :: binary(),
    Registry :: binary(),
    Opts :: map()
) -> {ok, term()} | {error, term()}.
do_push(AdapterModule, AdapterState, Images, Tag, Registry, Opts) ->
    {Repo, ImageTag} = parse_tag(Tag),
    Auth = get_push_auth(),
    ChunkSize = maps:get(chunk_size, Opts, undefined),

    %% Build push options (ChunkSize is expected to be in bytes already or undefined/nil)
    PushOpts =
        case is_nil_or_undefined(ChunkSize) of
            true -> #{};
            false when is_integer(ChunkSize), ChunkSize > 0 -> #{chunk_size => ChunkSize};
            false -> #{}
        end,

    RepoTag = <<Repo/binary, ":", ImageTag/binary>>,

    %% Push single image or multi-platform index
    PushResult =
        case Images of
            ImageList when is_list(ImageList), length(ImageList) > 1 ->
                AdapterModule:info("Pushing multi-platform image to ~s/~s:~s", [
                    Registry, Repo, ImageTag
                ]),
                ocibuild:push_multi(ImageList, Registry, RepoTag, Auth, PushOpts);
            [SingleImage] ->
                AdapterModule:info("Pushing to ~s/~s:~s", [Registry, Repo, ImageTag]),
                push_image(SingleImage, Registry, RepoTag, Auth, PushOpts);
            SingleImage ->
                AdapterModule:info("Pushing to ~s/~s:~s", [Registry, Repo, ImageTag]),
                push_image(SingleImage, Registry, RepoTag, Auth, PushOpts)
        end,

    case PushResult of
        ok ->
            AdapterModule:info("Push successful!", []),
            {ok, AdapterState};
        {error, PushError} ->
            {error, {push_failed, PushError}}
    end.

%%%===================================================================
%%% Public API
%%%===================================================================

-doc """
Collect all files from a release directory for inclusion in an OCI image.

Returns a list of `{ContainerPath, Content, Mode}` tuples suitable for
passing to `ocibuild:add_layer/2`.

Security: Symlinks pointing outside the release directory are skipped
with a warning to prevent path traversal attacks.
""".
-spec collect_release_files(file:filename()) ->
    {ok, [{binary(), binary(), non_neg_integer()}]} | {error, term()}.
collect_release_files(ReleasePath) ->
    collect_release_files(ReleasePath, #{}).

-doc """
Collect release files with options.

Options:
- `workdir` - Container working directory (default: `/app`)
""".
-spec collect_release_files(file:filename(), map()) ->
    {ok, [{binary(), binary(), non_neg_integer()}]} | {error, term()}.
collect_release_files(ReleasePath, Opts) ->
    Workdir = maps:get(workdir, Opts, "/app"),
    try
        Files = collect_files_recursive(ReleasePath, ReleasePath, Workdir),
        {ok, Files}
    catch
        throw:{file_error, Path, Reason} ->
            {error, {file_read_error, Path, Reason}}
    end.

%%%===================================================================
%%% Image Building - Clean API
%%%===================================================================

-doc """
Build an OCI image from release files with default options.

Shorthand for `build_image(BaseImage, Files, #{})`.

Example:
```
Files = [{<<"/app/bin/myapp">>, Binary, 8#755}],
{ok, Image} = build_image(<<"scratch">>, Files).
```
""".
-spec build_image(BaseImage :: binary(), Files :: [{binary(), binary(), non_neg_integer()}]) ->
    {ok, ocibuild:image()} | {error, term()}.
build_image(BaseImage, Files) ->
    build_image(BaseImage, Files, #{}).

-doc """
Build an OCI image from release files with options.

Options:
- `release_name` - Release name (atom, string, or binary) - required for setting entrypoint
- `workdir` - Working directory in container (default: `<<"/app">>`)
- `env` - Environment variables map (default: `#{}`)
- `expose` - Ports to expose (default: `[]`)
- `labels` - Image labels map (default: `#{}`)
- `cmd` - Release start command (default: `<<"foreground">>`)
- `auth` - Authentication credentials for pulling base image
- `progress` - Progress callback function
- `annotations` - Map of manifest annotations

Example:
```
Files = [{<<"/app/bin/myapp">>, Binary, 8#755}],
{ok, Image} = build_image(<<"debian:stable-slim">>, Files, #{
    release_name => <<"myapp">>,
    workdir => <<"/app">>,
    env => #{<<"LANG">> => <<"C.UTF-8">>},
    expose => [8080],
    cmd => <<"foreground">>
}).
```
""".
-spec build_image(
    BaseImage :: binary(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    Opts :: map()
) -> {ok, ocibuild:image()} | {error, term()}.
build_image(BaseImage, Files, Opts) when is_map(Opts) ->
    %% Extract options with defaults
    ReleaseName = maps:get(release_name, Opts, <<"app">>),
    Workdir = maps:get(workdir, Opts, <<"/app">>),
    EnvMap = maps:get(env, Opts, #{}),
    ExposePorts = maps:get(expose, Opts, []),
    Labels = maps:get(labels, Opts, #{}),
    Cmd = maps:get(cmd, Opts, <<"foreground">>),
    do_build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd, Opts).

%%%===================================================================
%%% Image Building - Internal Implementation
%%%===================================================================

%% @private Internal implementation of image building
-spec do_build_image(
    binary(),
    [{binary(), binary(), non_neg_integer()}],
    string() | binary(),
    binary(),
    map(),
    [non_neg_integer()],
    map(),
    binary(),
    map()
) -> {ok, ocibuild:image()} | {error, term()}.
do_build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd, Opts) ->
    try
        %% Start from base image or scratch
        Image0 =
            case BaseImage of
                ~"scratch" ->
                    {ok, Img} = ocibuild:scratch(),
                    Img;
                _ ->
                    PullAuth = maps:get(auth, Opts, #{}),
                    ProgressFn = maps:get(progress, Opts, undefined),
                    PullOpts =
                        case ProgressFn of
                            undefined -> #{};
                            _ -> #{progress => ProgressFn}
                        end,
                    case ocibuild:from(BaseImage, PullAuth, PullOpts) of
                        {ok, Img} ->
                            Img;
                        {error, FromErr} ->
                            throw({base_image_failed, FromErr})
                    end
            end,

        %% Add release files as a layer
        Image1 = ocibuild:add_layer(Image0, Files),

        %% Set working directory
        Image2 = ocibuild:workdir(Image1, to_binary(Workdir)),

        %% Set entrypoint and clear inherited Cmd from base image
        ReleaseNameBin = to_binary(ReleaseName),
        CmdBin = to_binary(Cmd),
        Entrypoint = [<<"/app/bin/", ReleaseNameBin/binary>>, CmdBin],
        Image3a = ocibuild:entrypoint(Image2, Entrypoint),
        %% Clear Cmd to prevent base image's Cmd from being appended
        Image3 = ocibuild:cmd(Image3a, []),

        %% Set environment variables
        Image4 =
            case map_size(EnvMap) of
                0 -> Image3;
                _ -> ocibuild:env(Image3, EnvMap)
            end,

        %% Expose ports
        Image5 =
            lists:foldl(fun(Port, Img) -> ocibuild:expose(Img, Port) end, Image4, ExposePorts),

        %% Add labels
        Image6 =
            maps:fold(
                fun(Key, Value, Img) ->
                    ocibuild:label(Img, to_binary(Key), to_binary(Value))
                end,
                Image5,
                Labels
            ),

        %% Add annotations
        Annotations = maps:get(annotations, Opts, #{}),
        Image7 =
            maps:fold(
                fun(Key, Value, Img) ->
                    ocibuild:annotation(Img, to_binary(Key), to_binary(Value))
                end,
                Image6,
                Annotations
            ),

        {ok, Image7}
    catch
        throw:Reason ->
            {error, Reason};
        error:Reason:Stacktrace ->
            {error, {Reason, Stacktrace}};
        exit:Reason ->
            {error, {exit, Reason}}
    end.

%%%===================================================================
%%% File Collection (with symlink security)
%%%===================================================================

%% @private Recursively collect files from release directory
collect_files_recursive(BasePath, CurrentPath, Workdir) ->
    case file:list_dir(CurrentPath) of
        {ok, Entries} ->
            lists:flatmap(
                fun(Entry) ->
                    FullPath = filename:join(CurrentPath, Entry),
                    collect_entry(BasePath, FullPath, Workdir)
                end,
                Entries
            );
        {error, Reason} ->
            throw({file_error, CurrentPath, Reason})
    end.

%% @private Collect a single entry, handling symlinks securely
collect_entry(BasePath, FullPath, Workdir) ->
    case file:read_link_info(FullPath) of
        {ok, #file_info{type = symlink}} ->
            %% Symlink - validate target is within release directory
            case validate_symlink_target(BasePath, FullPath) of
                {ok, directory} ->
                    collect_files_recursive(BasePath, FullPath, Workdir);
                {ok, regular} ->
                    [collect_single_file(BasePath, FullPath, Workdir)];
                {error, outside_release} ->
                    io:format("  Warning: Skipping symlink ~s (target outside release)~n", [
                        FullPath
                    ]),
                    [];
                {error, _Reason} ->
                    %% Broken symlink or other error - skip
                    io:format("  Warning: Skipping broken symlink ~s~n", [FullPath]),
                    []
            end;
        {ok, #file_info{type = directory}} ->
            collect_files_recursive(BasePath, FullPath, Workdir);
        {ok, #file_info{type = regular}} ->
            [collect_single_file(BasePath, FullPath, Workdir)];
        {ok, #file_info{type = _Other}} ->
            %% Skip special files (devices, sockets, etc.)
            [];
        {error, Reason} ->
            throw({file_error, FullPath, Reason})
    end.

%% @private Validate that a symlink target is within the release directory
-spec validate_symlink_target(file:filename(), file:filename()) ->
    {ok, directory | regular} | {error, outside_release | term()}.
validate_symlink_target(BasePath, SymlinkPath) ->
    case file:read_link(SymlinkPath) of
        {ok, Target} ->
            %% Resolve relative symlinks relative to the symlink's directory
            AbsTarget =
                case filename:pathtype(Target) of
                    absolute ->
                        Target;
                    relative ->
                        filename:join(filename:dirname(SymlinkPath), Target)
                end,
            %% Normalize the path to resolve .. components without requiring file existence
            NormalizedTarget = normalize_path(AbsTarget),
            NormalizedBase = normalize_path(BasePath),
            %% Check if target is within the release directory
            case is_path_within(NormalizedBase, NormalizedTarget) of
                true ->
                    %% Target is safe - check what type it is (follows symlink)
                    case file:read_file_info(SymlinkPath) of
                        {ok, #file_info{type = directory}} -> {ok, directory};
                        {ok, #file_info{type = regular}} -> {ok, regular};
                        {ok, #file_info{type = _}} -> {error, unsupported_type};
                        {error, Reason} -> {error, Reason}
                    end;
                false ->
                    {error, outside_release}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Normalize a path by resolving . and .. components
%% Unlike filename:absname/1, this doesn't require the path to exist
-spec normalize_path(file:filename()) -> file:filename().
normalize_path(Path) ->
    %% First make it absolute if it isn't already
    AbsPath =
        case filename:pathtype(Path) of
            absolute -> Path;
            _ -> filename:join(element(2, file:get_cwd()), Path)
        end,
    %% Split and normalize
    Components = filename:split(AbsPath),
    NormalizedComponents = normalize_components(Components, []),
    case NormalizedComponents of
        [] -> "/";
        _ -> filename:join(NormalizedComponents)
    end.

%% @private Normalize path components by resolving . and ..
-spec normalize_components([string()], [string()]) -> [string()].
normalize_components([], Acc) ->
    lists:reverse(Acc);
normalize_components(["." | Rest], Acc) ->
    normalize_components(Rest, Acc);
normalize_components([".." | Rest], [_ | AccRest]) ->
    %% Go up one directory (but not past root)
    normalize_components(Rest, AccRest);
normalize_components([".." | Rest], []) ->
    %% Already at root, ignore the ..
    normalize_components(Rest, []);
normalize_components([Component | Rest], Acc) ->
    normalize_components(Rest, [Component | Acc]).

%% @private Check if a path is within a base directory
-spec is_path_within(file:filename(), file:filename()) -> boolean().
is_path_within(BasePath, TargetPath) ->
    %% Ensure both paths end without trailing separator for consistent comparison
    BaseComponents = filename:split(BasePath),
    TargetComponents = filename:split(TargetPath),
    lists:prefix(BaseComponents, TargetComponents).

%% @private Collect a single file with its container path and mode
collect_single_file(BasePath, FilePath, Workdir) ->
    %% Get relative path from release root (cross-platform)
    RelPath = make_relative_path(BasePath, FilePath),

    %% Convert to container path with forward slashes
    ContainerPath = to_container_path(RelPath, Workdir),

    %% Read file content
    case file:read_file(FilePath) of
        {ok, Content} ->
            %% Get file permissions
            Mode = get_file_mode(FilePath),
            {ContainerPath, Content, Mode};
        {error, Reason} ->
            throw({file_error, FilePath, Reason})
    end.

%%%===================================================================
%%% Utility Functions
%%%===================================================================

-doc """
Check if a value is nil (Elixir) or undefined (Erlang).

This helper provides cross-language compatibility when handling
optional values from Elixir code.
""".
-spec is_nil_or_undefined(term()) -> boolean().
is_nil_or_undefined(undefined) -> true;
is_nil_or_undefined(nil) -> true;
is_nil_or_undefined(_) -> false.

-doc "Make a path relative to a base path (cross-platform).".
-spec make_relative_path(file:filename(), file:filename()) -> file:filename().
make_relative_path(BasePath, FullPath) ->
    %% Normalize both paths to use consistent separators
    BaseNorm = filename:split(BasePath),
    FullNorm = filename:split(FullPath),
    %% Remove the base prefix from the full path
    strip_prefix(BaseNorm, FullNorm).

%% @private Strip common prefix from paths
strip_prefix([H | T1], [H | T2]) ->
    strip_prefix(T1, T2);
strip_prefix([], Remaining) ->
    filename:join(Remaining);
strip_prefix(_, FullPath) ->
    filename:join(FullPath).

-doc "Convert a local path to a container path (always forward slashes).".
-spec to_container_path(file:filename()) -> binary().
to_container_path(RelPath) ->
    to_container_path(RelPath, "/app").

-spec to_container_path(file:filename(), file:filename()) -> binary().
to_container_path(RelPath, Workdir) ->
    %% Split and rejoin with forward slashes for container
    Parts = filename:split(RelPath),
    UnixPath = string:join(Parts, "/"),
    WorkdirStr =
        case Workdir of
            W when is_binary(W) -> binary_to_list(W);
            W -> W
        end,
    list_to_binary(WorkdirStr ++ "/" ++ UnixPath).

-doc "Get file mode (permissions) for a file.".
-spec get_file_mode(file:filename()) -> non_neg_integer().
get_file_mode(FilePath) ->
    case file:read_file_info(FilePath) of
        {ok, FileInfo} ->
            %% Extract permission bits (rwxrwxrwx)
            element(8, FileInfo) band 8#777;
        {error, _} ->
            %% Default to readable file
            8#644
    end.

-doc "Convert various types to binary.".
-spec to_binary(term()) -> binary().
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value);
to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value).

%%%===================================================================
%%% Output Operations (save/push)
%%%===================================================================

-doc """
Save an image to a tarball file.

The image is saved in OCI layout format compatible with `podman load`.
""".
-spec save_image(ocibuild:image(), file:filename(), map()) -> ok | {error, term()}.
save_image(Image, OutputPath, Opts) ->
    Tag = maps:get(tag, Opts, <<"latest">>),
    ProgressFn = maps:get(progress, Opts, undefined),
    SaveOpts = #{tag => Tag, progress => ProgressFn},
    ocibuild:save(Image, OutputPath, SaveOpts).

-doc """
Push an image to a registry.

Handles authentication, progress display, and httpc cleanup.
""".
-spec push_image(ocibuild:image(), binary(), binary(), map(), map()) -> ok | {error, term()}.
push_image(Image, Registry, RepoTag, Auth, Opts) ->
    ProgressFn = make_progress_callback(),
    PushOpts = Opts#{progress => ProgressFn},
    Result = ocibuild:push(Image, Registry, RepoTag, Auth, PushOpts),
    clear_progress_line(),
    stop_httpc(),
    Result.

-doc """
Parse a tag into repository and tag parts.

Examples:
- `<<"myapp:1.0.0">>` -> `{<<"myapp">>, <<"1.0.0">>}`
- `<<"myapp">>` -> `{<<"myapp">>, <<"latest">>}`
- `<<"ghcr.io/org/app:v1">>` -> `{<<"ghcr.io/org/app">>, <<"v1">>}`
""".
-spec parse_tag(binary()) -> {Repo :: binary(), Tag :: binary()}.
parse_tag(Tag) ->
    %% Find the last colon that's not part of a port number
    %% Strategy: split on ":" and check if the last part looks like a tag
    case binary:split(Tag, ~":", [global]) of
        [Repo] ->
            {Repo, ~"latest"};
        Parts ->
            %% Check if the last part looks like a tag (no slashes)
            LastPart = lists:last(Parts),
            case binary:match(LastPart, ~"/") of
                nomatch ->
                    %% Last part is the tag
                    Repo = iolist_to_binary(lists:join(~":", lists:droplast(Parts))),
                    {Repo, LastPart};
                _ ->
                    %% Last part contains a slash, so no tag specified
                    {Tag, ~"latest"}
            end
    end.

-doc """
Add an OCI description annotation to an image.

If description is undefined or empty, returns the image unchanged.
""".
-spec add_description(ocibuild:image(), binary() | undefined) -> ocibuild:image().
add_description(Image, undefined) ->
    Image;
add_description(Image, <<>>) ->
    Image;
add_description(Image, Description) when is_binary(Description) ->
    ocibuild:annotation(Image, ~"org.opencontainers.image.description", Description).

%%%===================================================================
%%% Authentication
%%%===================================================================

-doc """
Get authentication credentials for pushing images.

Reads from environment variables:
- `OCIBUILD_PUSH_TOKEN` - Bearer token (takes priority)
- `OCIBUILD_PUSH_USERNAME` / `OCIBUILD_PUSH_PASSWORD` - Basic auth
""".
-spec get_push_auth() -> map().
get_push_auth() ->
    case os:getenv("OCIBUILD_PUSH_TOKEN") of
        false ->
            case {os:getenv("OCIBUILD_PUSH_USERNAME"), os:getenv("OCIBUILD_PUSH_PASSWORD")} of
                {false, _} ->
                    #{};
                {_, false} ->
                    #{};
                {User, Pass} ->
                    #{username => list_to_binary(User), password => list_to_binary(Pass)}
            end;
        Token ->
            #{token => list_to_binary(Token)}
    end.

-doc """
Get authentication credentials for pulling base images.

Reads from environment variables:
- `OCIBUILD_PULL_TOKEN` - Bearer token (takes priority)
- `OCIBUILD_PULL_USERNAME` / `OCIBUILD_PULL_PASSWORD` - Basic auth
""".
-spec get_pull_auth() -> map().
get_pull_auth() ->
    case os:getenv("OCIBUILD_PULL_TOKEN") of
        false ->
            case {os:getenv("OCIBUILD_PULL_USERNAME"), os:getenv("OCIBUILD_PULL_PASSWORD")} of
                {false, _} ->
                    #{};
                {_, false} ->
                    #{};
                {User, Pass} ->
                    #{username => list_to_binary(User), password => list_to_binary(Pass)}
            end;
        Token ->
            #{token => list_to_binary(Token)}
    end.

%%%===================================================================
%%% Progress Display
%%%===================================================================

-doc """
Check if stdout is connected to a TTY (terminal).

Returns true for interactive terminals, false for CI/pipes.
""".
-spec is_tty() -> boolean().
is_tty() ->
    case io:columns() of
        {ok, _} -> true;
        {error, _} -> false
    end.

-doc """
Clear the progress line if in TTY mode.

In CI mode (non-TTY), progress is printed with newlines so no clearing needed.
""".
-spec clear_progress_line() -> ok.
clear_progress_line() ->
    case is_tty() of
        true -> io:format("\r\e[K", []);
        false -> ok
    end.

-doc """
Create a progress callback for terminal display.

Handles both TTY (animated progress) and CI (final state only) modes.
""".
-spec make_progress_callback() -> ocibuild_registry:progress_callback().
make_progress_callback() ->
    IsTTY = is_tty(),
    fun(Info) ->
        #{phase := Phase, total_bytes := Total} = Info,
        Bytes = maps:get(bytes_sent, Info, maps:get(bytes_received, Info, 0)),
        LayerIndex = maps:get(layer_index, Info, 0),
        TotalLayers = maps:get(total_layers, Info, 1),
        HasProgress = is_integer(Total) andalso Total > 0 andalso Bytes > 0,
        IsComplete = Bytes =:= Total,
        %% Use Total (layer size) in key to distinguish between different layers
        %% across platforms that might have the same index
        AlreadyPrinted =
            case IsTTY of
                true ->
                    false;
                false when IsComplete ->
                    Key = {ocibuild_progress_done, Phase, LayerIndex, Total},
                    case get(Key) of
                        true ->
                            true;
                        _ ->
                            put(Key, true),
                            false
                    end;
                false ->
                    false
            end,
        ShouldPrint = HasProgress andalso (IsTTY orelse IsComplete) andalso not AlreadyPrinted,
        case ShouldPrint of
            true ->
                PhaseStr =
                    case Phase of
                        manifest -> "Fetching manifest";
                        config -> "Fetching config  ";
                        layer when TotalLayers > 1 ->
                            io_lib:format("Layer ~B/~B        ", [LayerIndex, TotalLayers]);
                        layer ->
                            "Downloading layer";
                        uploading when TotalLayers > 1 ->
                            io_lib:format("Layer ~B/~B        ", [LayerIndex, TotalLayers]);
                        uploading ->
                            "Uploading layer  "
                    end,
                ProgressStr = format_progress(Bytes, Total),
                case IsTTY of
                    true -> io:format("\r\e[K  ~s: ~s", [PhaseStr, ProgressStr]);
                    false -> io:format("  ~s: ~s~n", [PhaseStr, ProgressStr])
                end;
            false ->
                ok
        end
    end.

-doc "Format progress as a string with progress bar.".
-spec format_progress(non_neg_integer(), non_neg_integer() | unknown) -> iolist().
format_progress(Received, unknown) ->
    io_lib:format("~s", [format_bytes(Received)]);
format_progress(Received, Total) when is_integer(Total), Total > 0 ->
    Percent = min(100, (Received * 100) div Total),
    BarWidth = 30,
    Filled = (Percent * BarWidth) div 100,
    Empty = BarWidth - Filled,
    Bar = lists:duplicate(Filled, $=) ++ lists:duplicate(Empty, $\s),
    io_lib:format("[~s] ~3B% ~s/~s", [Bar, Percent, format_bytes(Received), format_bytes(Total)]);
format_progress(Received, _) ->
    io_lib:format("~s", [format_bytes(Received)]).

-doc "Format bytes as human-readable string.".
-spec format_bytes(non_neg_integer()) -> iolist().
format_bytes(Bytes) when Bytes < 1024 ->
    io_lib:format("~B B", [Bytes]);
format_bytes(Bytes) when Bytes < 1024 * 1024 ->
    io_lib:format("~.1f KB", [Bytes / 1024]);
format_bytes(Bytes) when Bytes < 1024 * 1024 * 1024 ->
    io_lib:format("~.1f MB", [Bytes / (1024 * 1024)]);
format_bytes(Bytes) ->
    io_lib:format("~.2f GB", [Bytes / (1024 * 1024 * 1024)]).

%%%===================================================================
%%% Cleanup
%%%===================================================================

-doc """
Stop the ocibuild httpc profile to allow clean VM exit.

This should be called after push operations to close HTTP connections
and allow the VM to exit cleanly.
""".
-spec stop_httpc() -> ok.
stop_httpc() ->
    ocibuild_registry:stop_httpc().

%%%===================================================================
%%% Multi-platform Validation
%%%===================================================================

-doc """
Check if a release has bundled ERTS.

Multi-platform builds require the ERTS to come from the base image,
not bundled in the release. This function checks for an `erts-*` directory
at the release root.

This function works for all BEAM languages (Erlang, Elixir, Gleam, LFE)
since they all produce standard OTP releases.

```
true = ocibuild_release:has_bundled_erts("/path/to/rel/myapp").
```
""".
-spec has_bundled_erts(file:filename()) -> boolean().
has_bundled_erts(ReleasePath) ->
    case file:list_dir(ReleasePath) of
        {ok, Entries} ->
            lists:any(
                fun(Entry) ->
                    case Entry of
                        "erts-" ++ _ -> true;
                        _ -> false
                    end
                end,
                Entries
            );
        {error, _} ->
            false
    end.

-doc """
Check for native code (NIFs) in a release.

Scans `lib/*/priv/` directories for native shared libraries:
- `.so` files (Linux/Unix/BSD)
- `.dll` files (Windows)
- `.dylib` files (macOS)

This function works for all BEAM languages since they all produce
standard OTP releases with the same directory structure.

Returns `{ok, []}` if no native code found, or
`{warning, [NifInfo]}` with details about each native file found.

```
{ok, []} = ocibuild_release:check_for_native_code("/path/to/release").
{warning, [#{app := <<"crypto">>, file := <<"crypto_nif.so">>}]} =
    ocibuild_release:check_for_native_code("/path/to/release_with_nifs").
```
""".
-spec check_for_native_code(file:filename()) ->
    {ok, []} | {warning, [#{app := binary(), file := binary(), extension := binary()}]}.
check_for_native_code(ReleasePath) ->
    LibPath = filename:join(ReleasePath, "lib"),
    case file:list_dir(LibPath) of
        {ok, AppDirs} ->
            NativeFiles = lists:flatmap(
                fun(AppDir) ->
                    find_native_files_in_app(LibPath, AppDir)
                end,
                AppDirs
            ),
            case NativeFiles of
                [] -> {ok, []};
                Files -> {warning, Files}
            end;
        {error, _} ->
            {ok, []}
    end.

-doc """
Validate that a release is suitable for multi-platform builds.

For multi-platform builds (more than one platform specified):
1. **Error** if bundled ERTS is detected - multi-platform requires ERTS from base image
2. **Warning** if native code (NIFs) detected - may not be portable

This function is universal for all BEAM languages.

```
ok = ocibuild_release:validate_multiplatform(ReleasePath, [Platform]).
{error, {bundled_erts, _Reason}} = ocibuild_release:validate_multiplatform(ReleasePath, [P1, P2]).
```
""".
-spec validate_multiplatform(file:filename(), [ocibuild:platform()]) ->
    ok | {error, {bundled_erts, binary()}}.
validate_multiplatform(_ReleasePath, Platforms) when length(Platforms) =< 1 ->
    %% Single platform builds don't need validation
    ok;
validate_multiplatform(ReleasePath, _Platforms) ->
    case has_bundled_erts(ReleasePath) of
        true ->
            {error, {bundled_erts, erts_error_message()}};
        false ->
            %% Check for NIFs (warning only, don't block)
            case check_for_native_code(ReleasePath) of
                {warning, NifFiles} ->
                    warn_about_nifs(NifFiles),
                    ok;
                {ok, []} ->
                    ok
            end
    end.

%% @private Find native files in an app's priv directory
-spec find_native_files_in_app(file:filename(), string()) ->
    [#{app := binary(), file := binary(), extension := binary()}].
find_native_files_in_app(LibPath, AppDir) ->
    PrivPath = filename:join([LibPath, AppDir, "priv"]),
    case file:list_dir(PrivPath) of
        {ok, Files} ->
            NativeFiles = lists:filtermap(
                fun(File) ->
                    case is_native_file(File) of
                        {true, Ext} ->
                            AppName = extract_app_name(AppDir),
                            {true, #{
                                app => list_to_binary(AppName),
                                file => list_to_binary(File),
                                extension => Ext
                            }};
                        false ->
                            false
                    end
                end,
                Files
            ),
            %% Also check subdirectories of priv
            SubDirs = [F || F <- Files, filelib:is_dir(filename:join(PrivPath, F))],
            NestedFiles = lists:flatmap(
                fun(SubDir) ->
                    find_native_files_in_subdir(LibPath, AppDir, ["priv", SubDir])
                end,
                SubDirs
            ),
            NativeFiles ++ NestedFiles;
        {error, _} ->
            []
    end.

%% @private Find native files in subdirectories of priv
-spec find_native_files_in_subdir(file:filename(), string(), [string()]) ->
    [#{app := binary(), file := binary(), extension := binary()}].
find_native_files_in_subdir(LibPath, AppDir, PathParts) ->
    FullPath = filename:join([LibPath, AppDir | PathParts]),
    case file:list_dir(FullPath) of
        {ok, Files} ->
            lists:filtermap(
                fun(File) ->
                    case is_native_file(File) of
                        {true, Ext} ->
                            AppName = extract_app_name(AppDir),
                            RelFile = filename:join(PathParts ++ [File]),
                            {true, #{
                                app => list_to_binary(AppName),
                                file => list_to_binary(RelFile),
                                extension => Ext
                            }};
                        false ->
                            false
                    end
                end,
                Files
            );
        {error, _} ->
            []
    end.

%% @private Check if a filename is a native shared library
-spec is_native_file(string()) -> {true, binary()} | false.
is_native_file(Filename) ->
    case filename:extension(Filename) of
        ".so" -> {true, ~".so"};
        ".dll" -> {true, ~".dll"};
        ".dylib" -> {true, ~".dylib"};
        _ -> false
    end.

%% @private Extract app name from versioned directory (e.g., "crypto-1.0.0" -> "crypto")
%%
%% OTP release directories use the format "appname-version" where version follows
%% semver-like patterns: "1.0.0", "2.10.0", "1.0.0-rc1", "5.2", etc.
%%
%% Examples:
%%   "cowboy-2.10.0" -> "cowboy"
%%   "my_app-1.0.0"  -> "my_app"
%%   "my-app-2"      -> "my-app-2" (not a valid version, no dot)
%%   "jsx-3.1.0"     -> "jsx"
-spec extract_app_name(string()) -> string().
extract_app_name(AppDir) ->
    case string:split(AppDir, "-", trailing) of
        [Name, Version] ->
            case is_version_string(Version) of
                true -> Name;
                false -> AppDir
            end;
        _ ->
            AppDir
    end.

%% @private Check if a string looks like a version number
%%
%% Valid versions must:
%% 1. Start with a digit
%% 2. Have format like "X.Y" or "X.Y.Z" (major.minor or major.minor.patch)
%% 3. May have pre-release suffix like "-rc1", "-beta", "-alpha.1"
%% 4. May have build metadata like "+build.123"
-spec is_version_string(string()) -> boolean().
is_version_string([]) ->
    false;
is_version_string([C | _] = Version) when C >= $0, C =< $9 ->
    %% Starts with digit, now check for valid version pattern
    %% Strip any pre-release (-rc1) or build metadata (+build) suffix first
    BaseVersion = strip_version_suffix(Version),
    %% Must contain at least one dot and have numeric segments
    case string:split(BaseVersion, ".", all) of
        [_Single] ->
            %% No dot found - not a valid version (e.g., "2" alone)
            false;
        Segments when length(Segments) >= 2 ->
            %% Check that at least the first two segments are numeric
            lists:all(fun is_numeric_segment/1, lists:sublist(Segments, 2));
        _ ->
            false
    end;
is_version_string(_) ->
    false.

%% @private Strip pre-release and build metadata suffixes from version
%% "1.0.0-rc1" -> "1.0.0", "1.0.0+build" -> "1.0.0"
-spec strip_version_suffix(string()) -> string().
strip_version_suffix(Version) ->
    %% Find first occurrence of - or + that's not at the start
    case find_suffix_start(Version, 1) of
        0 -> Version;
        Pos -> lists:sublist(Version, Pos - 1)
    end.

%% @private Find position of first - or + after initial version digits
-spec find_suffix_start(string(), pos_integer()) -> non_neg_integer().
find_suffix_start([], _Pos) ->
    0;
find_suffix_start([$- | _], Pos) ->
    %% Only treat as suffix if we've seen at least one dot
    Pos;
find_suffix_start([$+ | _], Pos) ->
    Pos;
find_suffix_start([_ | Rest], Pos) ->
    find_suffix_start(Rest, Pos + 1).

%% @private Check if a string segment is numeric (all digits)
-spec is_numeric_segment(string()) -> boolean().
is_numeric_segment([]) ->
    false;
is_numeric_segment(Segment) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Segment).

%% @private Generate error message for bundled ERTS
-spec erts_error_message() -> binary().
erts_error_message() ->
    <<
        "Multi-platform builds require 'include_erts' set to false.\n"
        "Found bundled ERTS in release directory.\n\n"
        "For Elixir, update mix.exs:\n"
        "  releases: [\n"
        "    myapp: [\n"
        "      include_erts: false,\n"
        "      include_src: false\n"
        "    ]\n"
        "  ]\n\n"
        "For Erlang, update rebar.config:\n"
        "  {relx, [\n"
        "    {include_erts, false}\n"
        "  ]}.\n\n"
        "Then use a base image with ERTS, e.g.:\n"
        "  base_image: \"elixir:1.17-slim\" or \"erlang:27-slim\""
    >>.

%% @private Warn about native code that may not be portable
-spec warn_about_nifs([#{app := binary(), file := binary(), extension := binary()}]) -> ok.
warn_about_nifs(NifFiles) ->
    io:format(
        standard_error,
        "~nWarning: Native code detected that may not be portable across platforms:~n",
        []
    ),
    lists:foreach(
        fun(#{app := App, file := File}) ->
            io:format(standard_error, "  - ~s: ~s~n", [App, File])
        end,
        NifFiles
    ),
    io:format(standard_error, "~nNIFs compiled for one architecture won't work on others.~n", []),
    io:format(
        standard_error,
        "Consider using cross-compilation or Rust-based NIFs with multi-target support.~n~n",
        []
    ),
    ok.
