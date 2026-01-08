%%%-------------------------------------------------------------------
-module(ocibuild_release).
-feature(maybe_expr, enable).

%% eqWalizer has limited support for maybe expressions
-eqwalizer({nowarn_function, push_tarball_impl/8}).
-eqwalizer({nowarn_function, push_sbom_referrer/6}).
-eqwalizer({nowarn_function, push_signature_referrer/7}).

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
    build_image/3,
    build_auto_annotations/3
]).

%% Public API - Output Operations (save/push)
-export([
    save_image/3,
    push_image/5,
    push_tarball/4,
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
    start_progress_coordinator/0,
    stop_progress_coordinator/0,
    format_progress/2,
    format_bytes/1,
    is_tty/0,
    clear_progress_line/0
]).

%% Public API - Cleanup
-export([
    stop_httpc/0
]).

%% Exported for testing
-ifdef(TEST).
-export([
    push_signature_referrer/7
]).
-endif.

%% Public API - Multi-platform Validation
-export([
    has_bundled_erts/1,
    check_for_native_code/1,
    validate_multiplatform/2
]).

%% Public API - Smart Layer Partitioning
-export([
    partition_files_by_layer/5,
    classify_file_layer/5,
    build_release_layers/5
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
    validate_symlink_target/2,
    tag_additional/6
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
        tags := Tags,
        output := OutputOpt,
        push := PushRegistry,
        chunk_size := ChunkSize
    } = Config,

    %% Get optional platform configuration
    PlatformOpt = maps:get(platform, Config, undefined),

    %% Get optional uid configuration (default applied in configure_release_image)
    Uid = maps:get(uid, Config, undefined),

    maybe
        %% Validate at least one tag exists
        true ?= Tags =/= [],
        [FirstTag | _] = Tags,
        AdapterModule:info("Building OCI image: ~s", [FirstTag]),
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
        %% Start progress manager for parallel multi-line display
        _ = ocibuild_progress:start_manager(),
        PullAuth = get_pull_auth(),
        %% Get auto-annotation config (vcs_annotations defaults to true)
        VcsAnnotations = maps:get(vcs_annotations, Config, true),
        AppVersion = maps:get(app_version, Config, undefined),
        %% Get app_name for layer classification (may differ from release_name)
        AppName = maps:get(app_name, Config, undefined),
        %% Get dependencies for smart layer classification
        Dependencies = ocibuild_adapter:get_dependencies(AdapterModule, AdapterState),
        BuildOpts = #{
            release_name => ReleaseName,
            app_name => AppName,
            release_path => ReleasePath,
            workdir => Workdir,
            env => EnvMap,
            expose => ExposePorts,
            labels => Labels,
            cmd => Cmd,
            description => Description,
            auth => PullAuth,
            uid => Uid,
            vcs_annotations => VcsAnnotations,
            app_version => AppVersion,
            dependencies => Dependencies
        },
        {ok, Images} ?= build_platform_images(BaseImage, Files, Platforms, BuildOpts),
        %% Output the image(s) - this is where layer downloads happen for save
        SbomPath = maps:get(sbom, Config, undefined),
        SignKey = maps:get(sign_key, Config, undefined),
        OutputOpts = #{
            tags => Tags,
            output => OutputOpt,
            push => PushRegistry,
            chunk_size => ChunkSize,
            platforms => Platforms,
            sbom => SbomPath,
            sign_key => SignKey
        },
        Result = do_output(AdapterModule, AdapterState, Images, OutputOpts),
        ocibuild_progress:stop_manager(),
        Result
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
            %% Clean up httpc in case build_platform_images started it before failing
            stop_httpc(),
            {error, Reason}
    end.

%% @private Parse platform option string into list of platforms
%% If no platform specified, auto-detects the current system platform
-spec parse_platform_option(binary() | undefined | nil) ->
    {ok, [ocibuild:platform()]} | {error, term()}.
parse_platform_option(undefined) ->
    {ok, [detect_current_platform()]};
parse_platform_option(nil) ->
    {ok, [detect_current_platform()]};
parse_platform_option(<<>>) ->
    {ok, [detect_current_platform()]};
parse_platform_option(PlatformStr) when is_binary(PlatformStr) ->
    ocibuild:parse_platforms(PlatformStr);
parse_platform_option(Value) ->
    {error, {invalid_platform_value, Value}}.

%% @private Detect current system platform
-spec detect_current_platform() -> ocibuild:platform().
detect_current_platform() ->
    {Os, Arch} = ocibuild_registry:get_target_platform(),
    #{os => Os, architecture => Arch}.

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
%% Always treats platforms as a list. Returns a list of images.
%% The output format (single vs multi-platform index) is determined later
%% based on whether the original request was for multiple platforms.
%%
%% Opts must contain: release_name, workdir, env, expose, labels, cmd
%% Optional: description, auth, progress
-spec build_platform_images(
    BaseImage :: binary(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    Platforms :: [ocibuild:platform()],
    Opts :: map()
) -> {ok, [ocibuild:image()]} | {error, term()}.
build_platform_images(~"scratch", Files, Platforms, Opts) ->
    %% Scratch base image - create empty images for each platform
    Description = maps:get(description, Opts, undefined),
    ReleasePath = maps:get(release_path, Opts, ~"."),
    Images = [
        begin
            {ok, BaseImg} = ocibuild:scratch(),
            ImgWithPlatform = BaseImg#{platform => Platform},
            %% Build auto-annotations for this image
            AutoAnnotations = build_auto_annotations(ImgWithPlatform, ReleasePath, Opts),
            %% Merge with user annotations (user takes precedence)
            UserAnnotations = maps:get(annotations, Opts, #{}),
            MergedAnnotations = maps:merge(AutoAnnotations, UserAnnotations),
            OptsWithAnnotations = Opts#{annotations => MergedAnnotations},
            add_description(
                configure_release_image(ImgWithPlatform, Files, OptsWithAnnotations), Description
            )
        end
     || Platform <- Platforms
    ],
    {ok, Images};
build_platform_images(BaseImage, Files, Platforms, Opts) ->
    PullAuth = maps:get(auth, Opts, #{}),
    Description = maps:get(description, Opts, undefined),
    ReleasePath = maps:get(release_path, Opts, ~"."),

    %% Always use the platforms list - ocibuild:from/3 handles both single and multi
    case ocibuild:from(BaseImage, PullAuth, #{platforms => Platforms}) of
        {ok, BaseImages} when is_list(BaseImages) ->
            %% Apply release files and configuration to each platform image
            ConfiguredImages = [
                begin
                    %% Build auto-annotations for this image (includes base image info)
                    AutoAnnotations = build_auto_annotations(BaseImg, ReleasePath, Opts),
                    %% Merge with user annotations (user takes precedence)
                    UserAnnotations = maps:get(annotations, Opts, #{}),
                    MergedAnnotations = maps:merge(AutoAnnotations, UserAnnotations),
                    OptsWithAnnotations = Opts#{annotations => MergedAnnotations},
                    add_description(
                        configure_release_image(BaseImg, Files, OptsWithAnnotations), Description
                    )
                end
             || BaseImg <- BaseImages
            ],
            {ok, ConfiguredImages};
        {ok, SingleImage} ->
            %% Fallback: got single image, configure it
            AutoAnnotations = build_auto_annotations(SingleImage, ReleasePath, Opts),
            %% Merge with user annotations (user takes precedence)
            UserAnnotations = maps:get(annotations, Opts, #{}),
            MergedAnnotations = maps:merge(AutoAnnotations, UserAnnotations),
            OptsWithAnnotations = Opts#{annotations => MergedAnnotations},
            Image = add_description(
                configure_release_image(SingleImage, Files, OptsWithAnnotations), Description
            ),
            {ok, [Image]};
        {error, _} = Error ->
            Error
    end.

%% @private Format platform map as string
-spec format_platform(ocibuild:platform()) -> binary().
format_platform(#{os := OS, architecture := Arch} = Platform) ->
    case maps:get(variant, Platform, undefined) of
        undefined -> <<OS/binary, "/", Arch/binary>>;
        Variant -> <<OS/binary, "/", Arch/binary, "/", Variant/binary>>
    end.

%%%===================================================================
%%% Auto Annotations
%%%===================================================================

-doc """
Build automatic OCI annotations from VCS and build context.

This function creates annotations based on:
- VCS information (source URL, revision) if vcs_annotations is true
- Application version from the adapter
- Build timestamp (respects SOURCE_DATE_EPOCH for reproducible builds)
- Base image information (name and digest)

The annotations are merged with any user-provided annotations, with user
annotations taking precedence over auto-generated ones.
""".
-spec build_auto_annotations(ocibuild:image(), file:filename(), map()) -> map().
build_auto_annotations(Image, ReleasePath, Config) ->
    %% Start with empty annotations
    Annotations0 = #{},

    %% Add VCS annotations if enabled (default: true)
    VcsEnabled = maps:get(vcs_annotations, Config, true),
    Annotations1 =
        case VcsEnabled of
            true -> add_vcs_annotations(ReleasePath, Annotations0);
            false -> Annotations0
        end,

    %% Add version annotation from app_version
    Annotations2 =
        case maps:get(app_version, Config, undefined) of
            undefined ->
                Annotations1;
            Version when is_binary(Version), byte_size(Version) > 0 ->
                Annotations1#{~"org.opencontainers.image.version" => Version};
            _ ->
                Annotations1
        end,

    %% Add created timestamp (respects SOURCE_DATE_EPOCH for reproducible builds)
    Annotations3 = Annotations2#{
        ~"org.opencontainers.image.created" => ocibuild_time:get_iso8601()
    },

    %% Add base image annotations
    add_base_image_annotations(Image, Annotations3).

%% @private Add VCS annotations (source URL, revision) from detected VCS
%% Wrapped in try-catch to prevent VCS failures from breaking the build
-spec add_vcs_annotations(file:filename(), map()) -> map().
add_vcs_annotations(ReleasePath, Annotations) ->
    try
        case ocibuild_vcs:detect(ReleasePath) of
            {ok, VcsModule} ->
                VcsAnnotations = ocibuild_vcs:get_annotations(VcsModule, ReleasePath),
                maps:merge(Annotations, VcsAnnotations);
            not_found ->
                Annotations
        end
    catch
        _Class:_Reason ->
            %% VCS detection/annotation failed - silently continue without VCS annotations
            %% This prevents issues like git binary not found or permissions errors
            %% from breaking the image build
            Annotations
    end.

%% @private Add base image annotations (name and digest)
-spec add_base_image_annotations(ocibuild:image(), map()) -> map().
add_base_image_annotations(Image, Annotations) ->
    case maps:get(base, Image, none) of
        none ->
            %% Scratch image, no base annotations
            Annotations;
        {Registry, Repo, Tag} ->
            %% Build base image reference
            BaseRef = <<Registry/binary, "/", Repo/binary, ":", Tag/binary>>,
            A1 = Annotations#{~"org.opencontainers.image.base.name" => BaseRef},
            %% Compute digest from base manifest JSON
            case maps:get(base_manifest, Image, undefined) of
                undefined ->
                    A1;
                Manifest when is_map(Manifest) ->
                    ManifestJson = ocibuild_json:encode(Manifest),
                    Digest = ocibuild_digest:sha256(ManifestJson),
                    A1#{~"org.opencontainers.image.base.digest" => Digest}
            end
    end.

%%%===================================================================
%%% Image Configuration
%%%===================================================================

%% @private Configure a release image with files and settings
%%
%% This is the single source of truth for image configuration, used by both
%% the programmatic API (build_image/3) and CLI adapters (run/3).
%%
%% Opts:
%%   - release_name: Release name for entrypoint (default: ~"app")
%%   - workdir: Working directory in container (default: ~"/app")
%%   - env: Environment variables map (default: #{})
%%   - expose: Ports to expose (default: [])
%%   - labels: Image labels map (default: #{})
%%   - cmd: Release start command (default: ~"foreground")
%%   - uid: User ID to run as (default: 65534 for nobody)
%%   - annotations: Manifest annotations map (default: #{})
%%   - description: Image description annotation (default: undefined)
-spec configure_release_image(
    Image :: ocibuild:image(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    Opts :: map()
) -> ocibuild:image().
configure_release_image(Image0, Files, Opts) ->
    ReleaseName = maps:get(release_name, Opts, ~"app"),
    Workdir = to_binary(maps:get(workdir, Opts, ~"/app")),
    EnvMap = maps:get(env, Opts, #{}),
    ExposePorts = maps:get(expose, Opts, []),
    Labels = maps:get(labels, Opts, #{}),
    Cmd = to_binary(maps:get(cmd, Opts, ~"foreground")),
    Description = maps:get(description, Opts, undefined),
    Uid = maps:get(uid, Opts, undefined),
    Annotations = maps:get(annotations, Opts, #{}),

    %% Smart layer building: uses dependency info to split into 2-3 layers
    %% Falls back to single layer if no dependencies provided
    ReleasePath = maps:get(release_path, Opts, undefined),
    Dependencies = maps:get(dependencies, Opts, []),
    Image1 = build_release_layers(Image0, Files, ReleasePath, Dependencies, Opts),

    %% Generate SBOM and add as layer
    Image1a = add_sbom_layer(Image1, ReleaseName, Dependencies, ReleasePath, Opts),

    %% Set working directory
    Image2 = ocibuild:workdir(Image1a, Workdir),

    %% Set entrypoint and clear inherited Cmd from base image
    ReleaseNameBin = to_binary(ReleaseName),
    EntrypointPath = <<Workdir/binary, "/bin/", ReleaseNameBin/binary>>,
    Image3a = ocibuild:entrypoint(Image2, [EntrypointPath, Cmd]),
    Image3 = ocibuild:cmd(Image3a, []),

    %% Set environment variables
    Image4 =
        case map_size(EnvMap) of
            0 -> Image3;
            _ -> ocibuild:env(Image3, EnvMap)
        end,

    %% Expose ports
    Image5 = lists:foldl(fun(Port, Img) -> ocibuild:expose(Img, Port) end, Image4, ExposePorts),

    %% Add labels
    Image6 =
        case map_size(Labels) of
            0 ->
                Image5;
            _ ->
                maps:fold(
                    fun(K, V, Img) -> ocibuild:label(Img, to_binary(K), to_binary(V)) end,
                    Image5,
                    Labels
                )
        end,

    %% Add annotations
    Image7 =
        case map_size(Annotations) of
            0 ->
                Image6;
            _ ->
                maps:fold(
                    fun(K, V, Img) -> ocibuild:annotation(Img, to_binary(K), to_binary(V)) end,
                    Image6,
                    Annotations
                )
        end,

    %% Set user; when no UID is configured, default to 65534 (nobody) for non-root security
    %% Note: Elixir passes `nil` instead of `undefined` when not configured
    Image8 =
        case Uid of
            undefined ->
                ocibuild:user(Image7, ~"65534");
            nil ->
                ocibuild:user(Image7, ~"65534");
            U when is_integer(U), U >= 0 ->
                ocibuild:user(Image7, integer_to_binary(U));
            U when is_integer(U), U < 0 ->
                erlang:error({invalid_uid, U, "UID must be non-negative"});
            Other ->
                erlang:error({invalid_uid_type, Other, "UID must be an integer"})
        end,

    %% Add description annotation (convenience for OCI description)
    add_description(Image8, Description).

%% @private Output the image (save and optionally push)
%% Handles both single image and list of images (multi-platform)
%%
%% Opts: tags, output, push, chunk_size, platforms
-spec do_output(
    AdapterModule :: module(),
    AdapterState :: term(),
    Images :: ocibuild:image() | [ocibuild:image()],
    Opts :: map()
) -> {ok, term()} | {error, term()}.
do_output(AdapterModule, AdapterState, Images, Opts) ->
    Tags = maps:get(tags, Opts),
    [FirstTag | _] = Tags,
    OutputOpt = maps:get(output, Opts, undefined),
    PushRegistry = maps:get(push, Opts, undefined),
    ChunkSize = maps:get(chunk_size, Opts, undefined),
    Platforms = maps:get(platforms, Opts, []),

    %% Determine output path (handle both Erlang undefined and Elixir nil)
    %% Use first tag for output filename
    OutputPath =
        case is_nil_or_undefined(OutputOpt) of
            true -> default_output_path(FirstTag);
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
    %% Use first tag for the saved tarball
    SaveOpts = #{tag => FirstTag, progress => maps:get(progress, Opts, undefined)},
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

            %% Export SBOM to file if path specified
            SbomPath = maps:get(sbom, Opts, undefined),
            export_sbom_file(AdapterModule, Images, SbomPath),

            %% Push if requested (handle both Erlang undefined and Elixir nil)
            case is_nil_or_undefined(PushRegistry) orelse PushRegistry =:= <<>> of
                true ->
                    %% No push - clean up httpc started during base image pull
                    stop_httpc(),
                    AdapterModule:console("~nTo load the image:~n  podman load < ~s~n", [OutputPath]),
                    {ok, AdapterState};
                false ->
                    %% Clear progress bars before push to start fresh
                    ocibuild_progress:clear(),
                    SignKey = maps:get(sign_key, Opts, undefined),
                    PushOpts = #{chunk_size => ChunkSize, sign_key => SignKey},
                    do_push(AdapterModule, AdapterState, Images, Tags, PushRegistry, PushOpts)
            end;
        {error, SaveError} ->
            stop_httpc(),
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
    Tag = maps:get(tag, Opts, ~"latest"),
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

%% @private Push image(s) to registry with multiple tags
%% Handles both single image and list of images (multi-platform)
%% First tag does full push, additional tags just add tag references
%%
%% Opts: chunk_size
-spec do_push(
    AdapterModule :: module(),
    AdapterState :: term(),
    Images :: ocibuild:image() | [ocibuild:image()],
    Tags :: [binary()],
    Registry :: binary(),
    Opts :: map()
) -> {ok, term()} | {error, term()}.
do_push(AdapterModule, AdapterState, Images, Tags, PushDest, Opts) ->
    [FirstTag | AdditionalTags] = Tags,
    {ImageName, FirstImageTag} = parse_tag(FirstTag),
    Auth = get_push_auth(),
    ChunkSize = maps:get(chunk_size, Opts, undefined),

    %% Parse push destination: "ghcr.io/org" -> {Registry, Namespace}
    %% The registry is the first component, namespace is the rest
    {Registry, Namespace} = parse_push_destination(PushDest),

    %% Combine namespace with image name to form full repo path
    Repo =
        case Namespace of
            <<>> -> ImageName;
            _ -> <<Namespace/binary, "/", ImageName/binary>>
        end,

    %% Build push options (ChunkSize is expected to be in bytes already or undefined/nil)
    PushOpts =
        case is_nil_or_undefined(ChunkSize) of
            true -> #{};
            false when is_integer(ChunkSize), ChunkSize > 0 -> #{chunk_size => ChunkSize};
            false -> #{}
        end,

    RepoTag = <<Repo/binary, ":", FirstImageTag/binary>>,

    %% Push single image or multi-platform index with first tag
    PushResult =
        case Images of
            ImageList when is_list(ImageList), length(ImageList) > 1 ->
                AdapterModule:info("Pushing multi-platform image to ~s/~s:~s", [
                    Registry, Repo, FirstImageTag
                ]),
                ocibuild:push_multi(ImageList, Registry, RepoTag, Auth, PushOpts);
            [SingleImage] ->
                AdapterModule:info("Pushing to ~s/~s:~s", [Registry, Repo, FirstImageTag]),
                push_image(SingleImage, Registry, RepoTag, Auth, PushOpts);
            SingleImage ->
                AdapterModule:info("Pushing to ~s/~s:~s", [Registry, Repo, FirstImageTag]),
                push_image(SingleImage, Registry, RepoTag, Auth, PushOpts)
        end,

    case PushResult of
        {ok, Digest} ->
            %% Tag additional tags (efficient - just PUT manifest to new tag)
            TagResults = tag_additional(AdapterModule, Registry, Repo, Digest, AdditionalTags, Auth),

            %% Push SBOM as referrer artifact (if available)
            push_sbom_referrer(AdapterModule, Images, Registry, Repo, Auth, PushOpts),
            %% Push signature as referrer artifact (if sign_key configured)
            push_signature_referrer(AdapterModule, Images, Registry, Repo, FirstImageTag, Auth, Opts),
            %% Clean up httpc after all pushes complete
            stop_httpc(),

            %% Print digests for all tags (same digest for all)
            FullImageRef = <<Registry/binary, "/", Repo/binary, ":", FirstImageTag/binary>>,
            AdapterModule:console("Pushed: ~s@~s~n", [FullImageRef, Digest]),
            lists:foreach(
                fun({Tag, ok}) ->
                    {_, TagPart} = parse_tag(Tag),
                    Ref = <<Registry/binary, "/", Repo/binary, ":", TagPart/binary>>,
                    AdapterModule:console("Pushed: ~s@~s~n", [Ref, Digest]);
                   ({Tag, {error, Reason}}) ->
                    AdapterModule:error("Failed to tag ~s: ~p", [Tag, Reason])
                end,
                TagResults
            ),
            {ok, AdapterState};
        {error, PushError} ->
            stop_httpc(),
            {error, {push_failed, PushError}}
    end.

%% @private Tag additional tags (just PUT manifest to new tag URL)
-spec tag_additional(module(), binary(), binary(), binary(), [binary()], map()) ->
    [{binary(), ok | {error, term()}}].
tag_additional(_AdapterModule, _Registry, _Repo, _Digest, [], _Auth) ->
    [];
tag_additional(AdapterModule, Registry, Repo, Digest, Tags, Auth) ->
    lists:map(
        fun(Tag) ->
            {_, TagPart} = parse_tag(Tag),
            AdapterModule:info("Tagging ~s/~s:~s", [Registry, Repo, TagPart]),
            Result = ocibuild_registry:tag_from_digest(Registry, Repo, Digest, TagPart, Auth),
            case Result of
                {ok, _} -> {Tag, ok};
                {error, _} = Err -> {Tag, Err}
            end
        end,
        Tags
    ).

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
        %% Note: Files are sorted in ocibuild_tar:create/2 for reproducibility
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
Files = [{~"/app/bin/myapp", Binary, 8#755}],
{ok, Image} = build_image(~"scratch", Files).
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
- `workdir` - Working directory in container (default: `~"/app"`)
- `env` - Environment variables map (default: `#{}`)
- `expose` - Ports to expose (default: `[]`)
- `labels` - Image labels map (default: `#{}`)
- `cmd` - Release start command (default: `~"foreground"`)
- `uid` - User ID to run as (default: 65534 for nobody; use 0 for root)
- `auth` - Authentication credentials for pulling base image
- `progress` - Progress callback function
- `annotations` - Map of manifest annotations

Example:
```
Files = [{~"/app/bin/myapp", Binary, 8#755}],
{ok, Image} = build_image(~"debian:stable-slim", Files, #{
    release_name => ~"myapp",
    workdir => ~"/app",
    env => #{~"LANG" => ~"C.UTF-8"},
    expose => [8080],
    cmd => ~"foreground"
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
    ReleaseName = maps:get(release_name, Opts, ~"app"),
    Workdir = maps:get(workdir, Opts, ~"/app"),
    EnvMap = maps:get(env, Opts, #{}),
    ExposePorts = maps:get(expose, Opts, []),
    Labels = maps:get(labels, Opts, #{}),
    Cmd = maps:get(cmd, Opts, ~"foreground"),
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
                    PlatformOpt = maps:get(platform, Opts, undefined),
                    PullOpts0 =
                        case ProgressFn of
                            undefined -> #{};
                            _ -> #{progress => ProgressFn}
                        end,
                    PullOpts =
                        case PlatformOpt of
                            undefined -> PullOpts0;
                            _ -> PullOpts0#{platform => PlatformOpt}
                        end,
                    case ocibuild:from(BaseImage, PullAuth, PullOpts) of
                        {ok, Img} ->
                            Img;
                        {error, FromErr} ->
                            throw({base_image_failed, FromErr})
                    end
            end,

        %% Configure the image using the shared configuration function
        ConfigOpts = Opts#{
            release_name => ReleaseName,
            workdir => Workdir,
            env => EnvMap,
            expose => ExposePorts,
            labels => Labels,
            cmd => Cmd
        },
        Image1 = configure_release_image(Image0, Files, ConfigOpts),

        {ok, Image1}
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
    atom_to_binary(Value, utf8);
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
    Tag = maps:get(tag, Opts, ~"latest"),
    ProgressFn = maps:get(progress, Opts, undefined),
    SaveOpts = #{tag => Tag, progress => ProgressFn},
    ocibuild:save(Image, OutputPath, SaveOpts).

-doc """
Push an image to a registry.

Handles authentication, progress display, and httpc cleanup.
Returns `{ok, Digest}` where Digest is the sha256 digest of the pushed manifest.
""".
-spec push_image(ocibuild:image(), binary(), binary(), map(), map()) ->
    {ok, Digest :: binary()} | {error, term()}.
push_image(Image, Registry, RepoTag, Auth, Opts) ->
    ProgressFn = make_progress_callback(),
    PushOpts = Opts#{progress => ProgressFn},
    Result = ocibuild:push(Image, Registry, RepoTag, Auth, PushOpts),
    clear_progress_line(),
    %% Note: Don't stop httpc here - it may be needed for SBOM referrer push
    Result.

-doc """
Push a pre-built OCI tarball to a registry.

This function loads an existing OCI image tarball and pushes it to a registry
without rebuilding. Useful for CI/CD pipelines where build and push are
separate steps.

Options:
- `registry` - Target registry (e.g., `<<"ghcr.io/myorg">>`)
- `tags` - Optional tag overrides (uses embedded tag from tarball if not specified)
- `chunk_size` - Chunk size for uploads in bytes

Supports multiple tags: first tag does full upload, additional tags just
add tag references to the same manifest (efficient).

Returns `{ok, AdapterState}` on success with the pushed digest printed.
""".
-spec push_tarball(module(), term(), file:filename(), map()) ->
    {ok, term()} | {error, term()}.
push_tarball(AdapterModule, AdapterState, TarballPath, Opts) ->
    Registry = maps:get(registry, Opts),
    TagsOverride = maps:get(tags, Opts, []),
    ChunkSize = maps:get(chunk_size, Opts, undefined),

    AdapterModule:info("Loading tarball: ~s", [TarballPath]),

    case ocibuild_layout:load_tarball_for_push(TarballPath) of
        {ok, #{images := Images, tag := EmbeddedTag, is_multi_platform := IsMulti, cleanup := Cleanup}} ->
            %% Wrap in try-after to ensure cleanup is always called
            try
                push_tarball_impl(AdapterModule, AdapterState, Registry, TagsOverride,
                                  ChunkSize, Images, EmbeddedTag, IsMulti)
            after
                Cleanup()
            end;
        {error, _} = Err ->
            Err
    end.

%% Internal implementation of push_tarball, separated to ensure cleanup in caller
-spec push_tarball_impl(module(), term(), binary(), [binary()],
                        non_neg_integer() | undefined, [map()], binary() | undefined, boolean()) ->
    {ok, term()} | {error, term()}.
push_tarball_impl(AdapterModule, AdapterState, Registry, TagsOverride,
                  ChunkSize, Images, EmbeddedTag, IsMulti) ->
    maybe
        %% Determine tags: override takes precedence, then embedded, else error
        Tags = case TagsOverride of
            [] when EmbeddedTag =/= undefined -> [EmbeddedTag];
            [] -> [];
            _ -> TagsOverride
        end,

        %% Validate at least one tag is specified
        ok ?= case Tags of
            [] ->
                {error, {no_tag_specified, "Use --tag to specify image tag"}};
            _ ->
                ok
        end,

        %% Use first tag for push, additional tags will be added after
        [FirstTag | AdditionalTags] = Tags,

        %% Parse tag and registry
        {ImageName, FirstImageTag} = parse_tag(FirstTag),
        {RegistryHost, Namespace} = parse_push_destination(Registry),
        Repo = case Namespace of
            <<>> -> ImageName;
            _ -> <<Namespace/binary, "/", ImageName/binary>>
        end,

        AdapterModule:info("Pushing to ~s/~s:~s", [RegistryHost, Repo, FirstImageTag]),

        %% Get auth and progress
        Auth = get_push_auth(),
        ProgressFn = make_progress_callback(),
        PushOpts = #{progress => ProgressFn, chunk_size => ChunkSize},

        %% Convert loaded images to push_blobs_input format
        BlobsList = [convert_loaded_image_to_blobs(Img) || Img <- Images],

        %% Push with first tag
        Result = case {IsMulti, BlobsList} of
            {_, []} ->
                %% Defensive: should be caught earlier by validate_index_schema
                {error, {no_images_in_tarball}};
            {true, _} ->
                ocibuild_registry:push_blobs_multi(
                    RegistryHost, Repo, FirstImageTag, BlobsList, Auth, PushOpts
                );
            {false, [Blobs]} ->
                %% Check if tag override requires manifest update
                FinalBlobs = maybe_update_manifest_tag(Blobs, FirstTag, EmbeddedTag),
                ocibuild_registry:push_blobs(
                    RegistryHost, Repo, FirstImageTag, FinalBlobs, Auth, PushOpts
                );
            {false, _MultipleBlobsUnexpected} ->
                %% Inconsistent state: single-platform flag but multiple images
                {error, {invalid_tarball_layout,
                         ~"is_multi_platform is false but tarball contains multiple images"}}
        end,

        clear_progress_line(),

        {ok, Digest} ?= Result,

        %% Tag additional tags (efficient - just PUT manifest to new tag)
        TagResults = tag_additional(AdapterModule, RegistryHost, Repo, Digest, AdditionalTags, Auth),

        stop_httpc(),

        %% Print digests for all tags
        FullRef = <<RegistryHost/binary, "/", Repo/binary, ":", FirstImageTag/binary>>,
        AdapterModule:console("Pushed: ~s@~s~n", [FullRef, Digest]),
        lists:foreach(
            fun({Tag, ok}) ->
                {_, TagPart} = parse_tag(Tag),
                Ref = <<RegistryHost/binary, "/", Repo/binary, ":", TagPart/binary>>,
                AdapterModule:console("Pushed: ~s@~s~n", [Ref, Digest]);
               ({Tag, {error, Reason}}) ->
                AdapterModule:error("Failed to tag ~s: ~p", [Tag, Reason])
            end,
            TagResults
        ),
        {ok, AdapterState}
    else
        {error, _} = Err -> Err
    end.

%% Convert loaded_image to push_blobs_input format
-spec convert_loaded_image_to_blobs(map()) -> map().
convert_loaded_image_to_blobs(#{manifest := Manifest, config := Config, layers := Layers}) ->
    #{
        manifest => Manifest,
        config => Config,
        layers => Layers
    }.

%% Update manifest annotation if tag was overridden
-spec maybe_update_manifest_tag(map(), binary(), binary() | undefined) -> map().
maybe_update_manifest_tag(Blobs, NewTag, OldTag) when NewTag =:= OldTag ->
    %% Tags match, no change needed
    Blobs;
maybe_update_manifest_tag(#{manifest := ManifestJson} = Blobs, NewTag, _OldTag) ->
    %% Update the annotation in the manifest
    Manifest = ocibuild_json:decode(ManifestJson),
    OldAnnotations = maps:get(~"annotations", Manifest, #{}),
    NewAnnotations = OldAnnotations#{~"org.opencontainers.image.ref.name" => NewTag},
    UpdatedManifest = Manifest#{~"annotations" => NewAnnotations},
    UpdatedManifestJson = ocibuild_json:encode(UpdatedManifest),
    Blobs#{manifest => UpdatedManifestJson}.

-doc """
Parse a tag into repository and tag parts.

Examples:
- `~"myapp:1.0.0"` -> `{~"myapp", ~"1.0.0"}`
- `~"myapp"` -> `{~"myapp", ~"latest"}`
- `~"ghcr.io/org/app:v1"` -> `{~"ghcr.io/org/app", ~"v1"}`
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
Parse a push destination into registry host and namespace.

Examples:
- `~"ghcr.io/intility"` -> `{~"ghcr.io", ~"intility"}`
- `~"ghcr.io"` -> `{~"ghcr.io", <<>>}`
- `~"docker.io/myorg"` -> `{~"docker.io", ~"myorg"}`
- `~"localhost:5000/myorg"` -> `{~"localhost:5000", ~"myorg"}`
""".
-spec parse_push_destination(binary()) -> {Registry :: binary(), Namespace :: binary()}.
parse_push_destination(Dest) ->
    case binary:split(Dest, ~"/") of
        [Registry] ->
            %% No slash - just a registry host
            {Registry, <<>>};
        [FirstPart | Rest] ->
            %% Check if first part looks like a registry (contains "." or ":")
            case binary:match(FirstPart, [~".", ~":"]) of
                nomatch ->
                    %% No dot or colon - treat entire thing as namespace on docker.io
                    {~"docker.io", Dest};
                _ ->
                    %% First part is the registry, rest is namespace
                    Namespace = iolist_to_binary(lists:join(~"/", Rest)),
                    {FirstPart, Namespace}
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
Start the progress coordinator for multi-line parallel progress display.

Call this before starting parallel downloads/uploads. The coordinator
manages separate terminal lines for each concurrent operation.
""".
-spec start_progress_coordinator() -> pid() | undefined.
start_progress_coordinator() ->
    case is_tty() of
        true ->
            case whereis(ocibuild_progress_coord) of
                undefined ->
                    Pid = spawn_link(fun() ->
                        progress_coord_loop(#{slots => #{}, next_slot => 1})
                    end),
                    register(ocibuild_progress_coord, Pid),
                    Pid;
                Pid ->
                    Pid
            end;
        false ->
            undefined
    end.

-doc """
Stop the progress coordinator and clean up terminal state.
""".
-spec stop_progress_coordinator() -> ok.
stop_progress_coordinator() ->
    case whereis(ocibuild_progress_coord) of
        undefined ->
            ok;
        Pid ->
            Pid ! {stop, self()},
            receive
                {stopped, Pid} -> ok
            after 1000 ->
                ok
            end
    end.

%% Progress coordinator loop - manages multi-line display
progress_coord_loop(State) ->
    receive
        {update, SlotKey, Info} ->
            Slots = maps:get(slots, State),
            NextSlot = maps:get(next_slot, State),
            {Slot, NewSlots} =
                case maps:find(SlotKey, Slots) of
                    {ok, ExistingSlot} ->
                        {ExistingSlot, Slots};
                    error ->
                        %% New slot - print newline to reserve space
                        io:format("~n"),
                        {NextSlot, maps:put(SlotKey, NextSlot, Slots)}
                end,
            NewNextSlot =
                case maps:is_key(SlotKey, Slots) of
                    true -> NextSlot;
                    false -> NextSlot + 1
                end,
            render_progress_line(Slot, NewNextSlot - 1, Info),
            progress_coord_loop(State#{slots => NewSlots, next_slot => NewNextSlot});
        {stop, From} ->
            %% Move cursor to bottom and clean up
            Slots = maps:get(slots, State),
            TotalSlots = maps:size(Slots),
            case TotalSlots > 0 of
                true ->
                    %% Move to bottom of progress area
                    io:format("~n");
                false ->
                    ok
            end,
            From ! {stopped, self()},
            ok
    end.

%% Render a progress line at the given slot position
render_progress_line(Slot, TotalSlots, Info) ->
    #{phase := Phase, total_bytes := Total} = Info,
    Bytes = maps:get(bytes_sent, Info, maps:get(bytes_received, Info, 0)),
    LayerIndex = maps:get(layer_index, Info, 0),
    TotalLayers = maps:get(total_layers, Info, 1),
    Platform = maps:get(platform, Info, undefined),

    PhaseStr =
        case Phase of
            manifest ->
                "Fetching manifest";
            config ->
                "Fetching config  ";
            layer when TotalLayers > 1 ->
                io_lib:format("Layer ~B/~B        ", [LayerIndex, TotalLayers]);
            layer ->
                "Downloading layer";
            uploading when TotalLayers > 1 ->
                io_lib:format("Layer ~B/~B        ", [LayerIndex, TotalLayers]);
            uploading ->
                "Uploading layer  "
        end,

    PlatformStr =
        case Platform of
            undefined -> "";
            #{architecture := Arch} -> io_lib:format(" (~s)", [Arch])
        end,

    ProgressStr = format_progress(Bytes, Total),

    %% Calculate how many lines to move up from current position
    %% After printing \n for a slot, cursor is on that slot's line
    %% So we only need to move up by (TotalSlots - Slot), not +1
    LinesToMove = TotalSlots - Slot,

    %% Move up (if needed), clear line, print, move back down
    case LinesToMove of
        0 ->
            %% Already on the right line, just print
            io:format("\r\e[K  ~s~s: ~s", [PhaseStr, PlatformStr, ProgressStr]);
        N when N > 0 ->
            %% Move up N lines, print, move back down
            io:format(
                "\e[~BA\r\e[K  ~s~s: ~s\e[~BB",
                [N, PhaseStr, PlatformStr, ProgressStr, N]
            )
    end.

-doc """
Create a progress callback for terminal display.

Handles both TTY (animated multi-line progress via coordinator) and
CI (final state only) modes.
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

        case IsTTY of
            true when HasProgress ->
                %% TTY mode: use coordinator for multi-line display
                %% Create unique slot key from layer size (distinguishes layers)
                SlotKey = {Phase, Total},
                case whereis(ocibuild_progress_coord) of
                    undefined ->
                        %% No coordinator, fall back to single-line
                        PhaseStr = format_phase(Phase, LayerIndex, TotalLayers),
                        ProgressStr = format_progress(Bytes, Total),
                        io:format("\r\e[K  ~s: ~s", [PhaseStr, ProgressStr]);
                    Pid ->
                        Pid ! {update, SlotKey, Info}
                end;
            false when HasProgress andalso IsComplete ->
                %% CI mode: only print completion, deduplicate
                Key = {ocibuild_progress_done, Phase, LayerIndex, Total},
                case get(Key) of
                    true ->
                        ok;
                    _ ->
                        put(Key, true),
                        PhaseStr = format_phase(Phase, LayerIndex, TotalLayers),
                        ProgressStr = format_progress(Bytes, Total),
                        io:format("  ~s: ~s~n", [PhaseStr, ProgressStr])
                end;
            _ ->
                ok
        end
    end.

%% Format phase string for progress display
format_phase(manifest, _, _) ->
    "Fetching manifest";
format_phase(config, _, _) ->
    "Fetching config  ";
format_phase(layer, LayerIndex, TotalLayers) when TotalLayers > 1 ->
    io_lib:format("Layer ~B/~B        ", [LayerIndex, TotalLayers]);
format_phase(layer, _, _) ->
    "Downloading layer";
format_phase(uploading, LayerIndex, TotalLayers) when TotalLayers > 1 ->
    io_lib:format("Layer ~B/~B        ", [LayerIndex, TotalLayers]);
format_phase(uploading, _, _) ->
    "Uploading layer  ".

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
{warning, [#{app := ~"crypto", file := ~"crypto_nif.so"}]} =
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

%%%===================================================================
%%% Smart Layer Partitioning
%%%===================================================================

-doc """
Partition collected files into ERTS, dependency, and application layers.

Files are classified based on their container paths and the lock file:
- **App layer**: `lib/<app_name>-*`, `bin/`, `releases/`
- **Deps layer**: `lib/<name>-*` where `name` is in the lock file
- **ERTS layer** (if bundled): `erts-*` and `lib/<name>-*` NOT in lock file (OTP libs)

If ERTS is not bundled, OTP libs go to the deps layer instead.

The lock file is the source of truth for dependencies - anything in `lib/`
that's not in the lock file and not the app itself must be an OTP library.

Returns `{ErtsFiles, DepFiles, AppFiles}` where each is a list of
`{Path, Content, Mode}` tuples.
""".
-spec partition_files_by_layer(
    Files :: [{binary(), binary(), non_neg_integer()}],
    Deps :: [#{name := binary(), version := binary(), source := binary()}],
    AppName :: binary(),
    Workdir :: binary(),
    HasErts :: boolean()
) ->
    {
        ErtsFiles :: [{binary(), binary(), non_neg_integer()}],
        DepFiles :: [{binary(), binary(), non_neg_integer()}],
        AppFiles :: [{binary(), binary(), non_neg_integer()}]
    }.
partition_files_by_layer(Files, Deps, AppName, Workdir, HasErts) ->
    DepNames = sets:from_list([maps:get(name, D) || D <- Deps]),

    lists:foldl(
        fun({Path, _Content, _Mode} = File, {Erts, DepAcc, App}) ->
            case classify_file_layer(Path, DepNames, AppName, Workdir, HasErts) of
                erts -> {[File | Erts], DepAcc, App};
                dep -> {Erts, [File | DepAcc], App};
                app -> {Erts, DepAcc, [File | App]}
            end
        end,
        {[], [], []},
        Files
    ).

-doc """
Classify a single file path into erts, dep, or app layer.

Classification rules (lock file is source of truth):
1. `lib/<app_name>-*` -> app layer (your application)
2. `lib/<name>-*` where name is in lock file -> dep layer
3. `lib/<name>-*` where name is NOT in lock file -> OTP lib:
   - If ERTS bundled: erts layer
   - If ERTS not bundled: dep layer (OTP libs are stable like deps)
4. `erts-*` directory -> erts layer
5. `bin/`, `releases/`, etc. -> app layer
""".
-spec classify_file_layer(
    Path :: binary(),
    DepNames :: sets:set(binary()),
    AppName :: binary(),
    Workdir :: binary(),
    HasErts :: boolean()
) -> erts | dep | app.
classify_file_layer(Path, DepNames, AppName, Workdir, HasErts) ->
    %% Remove workdir prefix to get relative path
    RelPath = strip_workdir_prefix(Path, Workdir),

    case extract_path_component(RelPath) of
        {erts, _ErtsVersion} ->
            %% erts-* directory always goes to erts layer
            erts;
        {lib, LibName} ->
            %% Check in order: app name, then deps, then OTP (everything else)
            case LibName =:= AppName of
                true ->
                    app;
                false ->
                    case sets:is_element(LibName, DepNames) of
                        true ->
                            dep;
                        false ->
                            %% Not app, not dep -> must be OTP lib
                            %% With ERTS: group with ERTS layer
                            %% Without ERTS: treat like deps (stable, cached)
                            case HasErts of
                                true -> erts;
                                false -> dep
                            end
                    end
            end;
        _Other ->
            %% bin/, releases/, etc. go to app layer
            app
    end.

-doc """
Build release layers with smart dependency layering when available.

If dependency information is provided, creates 2-3 layers:
- **With ERTS** (bundled): ERTS layer, Deps layer, App layer
- **Without ERTS** (multi-platform): Deps layer, App layer

Falls back to single layer if no dependencies provided (backward compatible).
""".
-spec build_release_layers(
    Image :: ocibuild:image(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    ReleasePath :: file:filename() | undefined,
    Deps :: [#{name := binary(), version := binary(), source := binary()}],
    Opts :: map()
) -> ocibuild:image().
build_release_layers(Image0, Files, _ReleasePath, [], _Opts) ->
    %% No dependency info - single layer fallback
    ocibuild:add_layer(Image0, Files);
build_release_layers(Image0, Files, _ReleasePath, _Deps, #{app_name := undefined}) ->
    %% No app_name - can't do smart layering, single layer fallback
    ocibuild:add_layer(Image0, Files);
build_release_layers(Image0, Files, ReleasePath, Deps, #{app_name := AppName} = Opts) ->
    %% Smart layering with known app_name
    Workdir = to_binary(maps:get(workdir, Opts, ?DEFAULT_WORKDIR)),
    HasErts = has_bundled_erts(ReleasePath),

    {ErtsFiles, DepFiles, AppFiles} =
        partition_files_by_layer(Files, Deps, to_binary(AppName), Workdir, HasErts),

    case HasErts of
        true ->
            %% 3 layers: ERTS + OTP libs -> Deps -> App
            I1 = add_layer_if_nonempty(Image0, ErtsFiles, erts),
            I2 = add_layer_if_nonempty(I1, DepFiles, deps),
            add_layer_if_nonempty(I2, AppFiles, app);
        false ->
            %% 2 layers: Deps + OTP libs -> App
            I1 = add_layer_if_nonempty(Image0, DepFiles, deps),
            add_layer_if_nonempty(I1, AppFiles, app)
    end;
build_release_layers(Image0, Files, _ReleasePath, _Deps, _Opts) ->
    %% No app_name in opts - single layer fallback
    ocibuild:add_layer(Image0, Files).

%% @private Add layer only if file list is non-empty
-spec add_layer_if_nonempty(ocibuild:image(), [{binary(), binary(), non_neg_integer()}], atom()) ->
    ocibuild:image().
add_layer_if_nonempty(Image, [], _Type) ->
    Image;
add_layer_if_nonempty(Image, Files, Type) ->
    ocibuild:add_layer(Image, Files, #{layer_type => Type}).

%% @private Strip workdir prefix from path
-spec strip_workdir_prefix(binary(), binary()) -> binary().
strip_workdir_prefix(Path, Workdir) ->
    WorkdirWithSlash = <<Workdir/binary, "/">>,
    WorkdirLen = byte_size(WorkdirWithSlash),
    case Path of
        <<WorkdirWithSlash:WorkdirLen/binary, Rest/binary>> ->
            Rest;
        _ ->
            %% Try without trailing slash
            WorkdirLen2 = byte_size(Workdir),
            case Path of
                <<Workdir:WorkdirLen2/binary, "/", Rest/binary>> -> Rest;
                _ -> Path
            end
    end.

%% @private Extract component type and name from relative path
%% Returns {erts, Version}, {lib, AppName}, or {other, FirstComponent}
-spec extract_path_component(binary()) ->
    {erts, binary()} | {lib, binary()} | {other, binary()}.
extract_path_component(Path) ->
    case binary:split(Path, ~"/") of
        [First | _] ->
            case First of
                <<"erts-", Version/binary>> ->
                    {erts, Version};
                ~"lib" ->
                    %% Extract app name from lib/appname-version/...
                    case binary:split(Path, ~"/", [global]) of
                        [~"lib", AppDir | _] ->
                            AppName = extract_app_name_from_dir(AppDir),
                            {lib, AppName};
                        _ ->
                            {other, First}
                    end;
                _ ->
                    {other, First}
            end;
        _ ->
            {other, Path}
    end.

%% @private Extract app name from versioned directory name
%% "cowboy-2.10.0" -> "cowboy"
%% "my_app-1.0.0" -> "my_app"
-spec extract_app_name_from_dir(binary()) -> binary().
extract_app_name_from_dir(DirName) ->
    %% Find the last hyphen followed by a version-like string
    case binary:split(DirName, ~"-", [global]) of
        [Name] ->
            Name;
        Parts ->
            %% Check if last part looks like a version (starts with digit)
            LastPart = lists:last(Parts),
            case looks_like_version(LastPart) of
                true ->
                    %% Join all but last part
                    iolist_to_binary(lists:join(~"-", lists:droplast(Parts)));
                false ->
                    %% Non-standard version (e.g., "myapp-main", "cowboy-latest")
                    %% Return full dir name as we can't reliably extract app name
                    DirName
            end
    end.

%% @private Check if a binary looks like a version number (starts with digit)
-spec looks_like_version(binary()) -> boolean().
looks_like_version(<<C, _/binary>>) when C >= $0, C =< $9 -> true;
looks_like_version(_) -> false.

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
%%   "my-app-2"      -> "my-app-2" ("2" lacks a dot, so not a version)
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

%% @private Find position of first `-` or `+` character in a version string.
%% Returns position (1-based) of the suffix marker, or 0 if not found.
%% Used to strip pre-release (-rc1) or build metadata (+build) from versions.
-spec find_suffix_start(string(), pos_integer()) -> non_neg_integer().
find_suffix_start([], _Pos) ->
    0;
find_suffix_start([$- | _], Pos) ->
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
    NifList = [io_lib:format("  - ~s: ~s", [App, File]) || #{app := App, file := File} <- NifFiles],
    NifListStr = lists:join("\n", NifList),
    logger:warning(
        "Native code detected that may not be portable across platforms:~n~s~n~n"
        "NIFs compiled for one architecture won't work on others.~n"
        "Consider using cross-compilation or Rust-based NIFs with multi-target support.",
        [NifListStr]
    ),
    ok.

%%%===================================================================
%%% SBOM Generation
%%%===================================================================

%% @private Push SBOM as OCI referrer artifact (silently skips if unsupported)
%% For single-platform images, pushes SBOM as referrer to the image manifest.
%% For multi-platform images, referrer attachment is not yet supported.
-spec push_sbom_referrer(
    module(),
    ocibuild:image() | [ocibuild:image()],
    binary(),
    binary(),
    map(),
    map()
) -> ok.
push_sbom_referrer(AdapterModule, [Image], Registry, Repo, Auth, Opts) when is_map(Image) ->
    %% Single image in a list - unwrap and process
    push_sbom_referrer(AdapterModule, Image, Registry, Repo, Auth, Opts);
push_sbom_referrer(_AdapterModule, Images, _Registry, _Repo, _Auth, _Opts) when is_list(Images) ->
    %% Multi-platform images - referrer attachment not yet supported
    %% (would need to attach SBOM to each platform manifest)
    ok;
push_sbom_referrer(AdapterModule, Image, Registry, Repo, Auth, Opts) when is_map(Image) ->
    SbomJson = maps:get(sbom, Image, undefined),
    maybe
        %% Only proceed if SBOM exists
        true ?= is_binary(SbomJson),
        %% Calculate manifest digest and size from image
        {ok, ManifestDigest, ManifestSize} ?= calculate_manifest_info(Image),
        %% Push SBOM as referrer
        PushOpts = maps:with([chunk_size], Opts),
        PushResult = ocibuild_registry:push_referrer(
            SbomJson,
            ocibuild_sbom:media_type(),
            Registry,
            Repo,
            ManifestDigest,
            ManifestSize,
            Auth,
            PushOpts
        ),
        case PushResult of
            ok ->
                AdapterModule:info("SBOM attached as artifact", []);
            {error, {referrer_not_supported, _}} ->
                %% Registry doesn't support referrers - silent skip
                ok;
            {error, Reason} ->
                AdapterModule:info("Warning: SBOM attachment failed: ~p", [Reason])
        end
    else
        false ->
            %% No SBOM in image - skip
            ok;
        {error, _Reason} ->
            %% Could not calculate manifest - skip referrer push
            ok
    end.

%%%===================================================================
%%% Image Signing
%%%===================================================================

%% @private Push cosign-compatible signature as OCI referrer artifact
%% For single-platform images, signs and pushes signature as referrer to the image manifest.
%% For multi-platform images, signature attachment is not yet supported.
-spec push_signature_referrer(
    module(),
    ocibuild:image() | [ocibuild:image()],
    binary(),
    binary(),
    binary(),
    map(),
    map()
) -> ok.
push_signature_referrer(AdapterModule, [Image], Registry, Repo, Tag, Auth, Opts) when is_map(Image) ->
    %% Single image in a list - unwrap and process
    push_signature_referrer(AdapterModule, Image, Registry, Repo, Tag, Auth, Opts);
push_signature_referrer(_AdapterModule, Images, _Registry, _Repo, _Tag, _Auth, _Opts) when is_list(Images) ->
    %% Multi-platform images - signature attachment not yet supported
    ok;
push_signature_referrer(AdapterModule, Image, Registry, Repo, Tag, Auth, Opts) when is_map(Image) ->
    SignKeyPath = maps:get(sign_key, Opts, undefined),
    maybe
        %% Only proceed if sign_key is a binary path
        true ?= is_binary(SignKeyPath),
        %% Load the signing key
        {ok, PrivateKey} ?= ocibuild_sign:load_key(binary_to_list(SignKeyPath)),
        %% Calculate manifest digest and size
        {ok, ManifestDigest, ManifestSize} ?= calculate_manifest_info(Image),
        %% Build docker reference for payload
        DockerRef = <<Registry/binary, "/", Repo/binary, ":", Tag/binary>>,
        %% Sign the manifest
        {ok, PayloadJson, Signature} ?= ocibuild_sign:sign(ManifestDigest, DockerRef, PrivateKey),
        %% Push signature as referrer
        PushOpts = maps:with([chunk_size], Opts),
        PushResult = ocibuild_registry:push_signature(
            PayloadJson,
            Signature,
            Registry,
            Repo,
            ManifestDigest,
            ManifestSize,
            Auth,
            PushOpts
        ),
        case PushResult of
            ok ->
                SignatureTag = ocibuild_registry:digest_to_signature_tag(ManifestDigest),
                AdapterModule:info("Image signed with ~s", [SignatureTag]);
            {error, PushError} ->
                AdapterModule:info("Warning: Signing failed: ~p", [PushError])
        end
    else
        false ->
            %% No sign key configured (SignKeyPath is undefined/nil/not binary) - skip
            ok;
        {error, {key_read_failed, KeyPath, _} = KeyError} ->
            %% File read error - path is in the error tuple
            AdapterModule:info("Warning: Failed to load signing key ~s: ~p", [KeyPath, KeyError]);
        {error, {invalid_pem, _} = KeyError} ->
            AdapterModule:info("Warning: Invalid signing key ~s: ~p", [SignKeyPath, KeyError]);
        {error, {pem_decode_failed, _} = KeyError} ->
            AdapterModule:info("Warning: Invalid signing key ~s: ~p", [SignKeyPath, KeyError]);
        {error, {key_decode_failed, _} = KeyError} ->
            AdapterModule:info("Warning: Invalid signing key ~s: ~p", [SignKeyPath, KeyError]);
        {error, {unsupported_curve, _, _} = KeyError} ->
            AdapterModule:info("Warning: Invalid signing key ~s: ~p", [SignKeyPath, KeyError]);
        {error, {unsupported_curve_format, _} = KeyError} ->
            AdapterModule:info("Warning: Invalid signing key ~s: ~p", [SignKeyPath, KeyError]);
        {error, Reason} ->
            %% Could not calculate manifest, signing failed, or other error
            AdapterModule:info("Warning: Signing failed: ~p", [Reason])
    end.

%% @private Calculate manifest digest and size from image
%% Reuses manifest building logic from ocibuild_layout
-spec calculate_manifest_info(ocibuild:image()) ->
    {ok, binary(), non_neg_integer()} | {error, term()}.
calculate_manifest_info(Image) ->
    try
        %% Reuse config blob building from ocibuild_layout
        {ConfigJson, ConfigDigest} = ocibuild_layout:build_config_blob(Image),
        ConfigDescriptor = #{
            ~"mediaType" => ~"application/vnd.oci.image.config.v1+json",
            ~"digest" => ConfigDigest,
            ~"size" => byte_size(ConfigJson)
        },

        %% Reuse layer descriptor building from ocibuild_layout
        LayerDescriptors = ocibuild_layout:build_layer_descriptors(Image),

        %% Get annotations
        Annotations = maps:get(annotations, Image, #{}),

        %% Build manifest
        {ManifestJson, ManifestDigest} = ocibuild_manifest:build(
            ConfigDescriptor, LayerDescriptors, Annotations
        ),

        {ok, ManifestDigest, byte_size(ManifestJson)}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% @private Export SBOM to file if path is specified
%% Extracts SBOM from first image in list (multi-platform) or single image
-spec export_sbom_file(module(), ocibuild:image() | [ocibuild:image()], binary() | undefined) -> ok.
export_sbom_file(_AdapterModule, _Images, undefined) ->
    ok;
export_sbom_file(_AdapterModule, _Images, nil) ->
    ok;
export_sbom_file(AdapterModule, Images, SbomPath) when is_binary(SbomPath) ->
    %% Get SBOM from first image (for multi-platform, first image's SBOM is representative)
    Image =
        case Images of
            [FirstImage | _] -> FirstImage;
            SingleImage when is_map(SingleImage) -> SingleImage
        end,
    case maps:get(sbom, Image, undefined) of
        undefined ->
            AdapterModule:info("Warning: SBOM not available for export", []);
        SbomJson when is_binary(SbomJson) ->
            Path = binary_to_list(SbomPath),
            case file:write_file(Path, SbomJson) of
                ok ->
                    AdapterModule:info("SBOM exported to ~s", [Path]);
                {error, Reason} ->
                    AdapterModule:info("Warning: Failed to export SBOM to ~s: ~p", [Path, Reason])
            end
    end,
    ok.

%% @private Add SBOM layer to image
%% Generates SPDX 2.2 SBOM and adds as a layer at /sbom.spdx.json
%% Also stores the SBOM JSON in the image map for later referrer push
-spec add_sbom_layer(
    ocibuild:image(), binary() | atom(), [map()], file:filename() | undefined, map()
) ->
    ocibuild:image().
add_sbom_layer(Image, ReleaseName, Dependencies, ReleasePath, Opts) ->
    try
        %% Build SBOM options from available data
        %% app_name may differ from release_name (e.g., app: :indicator_sync, release: :server)
        ReleaseNameBin = to_binary(ReleaseName),
        AppName =
            case maps:get(app_name, Opts, undefined) of
                undefined -> ReleaseNameBin;
                nil -> ReleaseNameBin;
                Name -> to_binary(Name)
            end,
        AppVersion = maps:get(app_version, Opts, undefined),
        SourceUrl = get_source_url_from_opts(Opts),
        ErtsVersion = detect_erts_version(ReleasePath),
        OtpVersion = get_otp_version(),
        {BaseImage, BaseDigest} = get_base_image_info(Image),

        SbomOpts = #{
            app_name => AppName,
            release_name => ReleaseNameBin,
            app_version => AppVersion,
            source_url => SourceUrl,
            base_image => BaseImage,
            base_digest => BaseDigest,
            erts_version => ErtsVersion,
            otp_version => OtpVersion
        },

        %% Generate SBOM
        {ok, SbomJson} = ocibuild_sbom:generate(Dependencies, SbomOpts),

        %% Add SBOM as layer using ocibuild:add_layer to properly update config
        %% (updates both layers list and config diff_ids/history)
        SbomFiles = [{~"/sbom.spdx.json", SbomJson, 8#644}],
        Image1 = ocibuild:add_layer(Image, SbomFiles, #{layer_type => sbom}),

        %% Store SBOM JSON for later referrer push
        Image1#{sbom => SbomJson}
    catch
        Class:Reason:Stacktrace ->
            %% SBOM generation failed - log warning and continue without SBOM
            io:format(
                standard_error,
                "ocibuild: warning: SBOM generation failed (~p:~p), continuing without SBOM~n"
                "  Stacktrace: ~p~n",
                [Class, Reason, Stacktrace]
            ),
            Image
    end.

%% @private Get source URL from opts (from VCS annotations if available)
-spec get_source_url_from_opts(map()) -> binary() | undefined.
get_source_url_from_opts(Opts) ->
    Annotations = maps:get(annotations, Opts, #{}),
    maps:get(~"org.opencontainers.image.source", Annotations, undefined).

%% @private Detect ERTS version from release path
%% Looks for erts-X.Y.Z directory in the release
-spec detect_erts_version(file:filename() | undefined) -> binary() | undefined.
detect_erts_version(undefined) ->
    undefined;
detect_erts_version(ReleasePath) ->
    case file:list_dir(ReleasePath) of
        {ok, Entries} ->
            case
                lists:filtermap(
                    fun(Entry) ->
                        case Entry of
                            "erts-" ++ Version -> {true, list_to_binary(Version)};
                            _ -> false
                        end
                    end,
                    Entries
                )
            of
                [Version | _] -> Version;
                [] -> undefined
            end;
        {error, _} ->
            undefined
    end.

%% @private Get OTP version from runtime
-spec get_otp_version() -> binary().
get_otp_version() ->
    list_to_binary(erlang:system_info(otp_release)).

%% @private Get base image info (reference and digest) from image
-spec get_base_image_info(ocibuild:image()) ->
    {{binary(), binary(), binary()}, binary() | undefined} | {none, undefined}.
get_base_image_info(Image) ->
    case maps:get(base, Image, none) of
        none ->
            {none, undefined};
        {_Registry, _Repo, _Tag} = BaseRef ->
            %% Calculate digest from base manifest if available
            Digest =
                case maps:get(base_manifest, Image, undefined) of
                    undefined ->
                        undefined;
                    Manifest when is_map(Manifest) ->
                        ManifestJson = ocibuild_json:encode(Manifest),
                        ocibuild_digest:sha256(ManifestJson)
                end,
            {BaseRef, Digest}
    end.
