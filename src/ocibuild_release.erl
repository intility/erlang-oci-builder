%%%-------------------------------------------------------------------
-module(ocibuild_release).
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
    build_image/7,
    build_image/8,
    build_image/9
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

%% Utility exports (used by build tools)
-export([
    to_binary/1,
    to_container_path/1,
    get_file_mode/1,
    make_relative_path/2
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
    try
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
            tag := TagOpt,
            output := OutputOpt,
            push := PushRegistry,
            chunk_size := ChunkSize
        } = Config,

        %% Get tag (required)
        Tag =
            case TagOpt of
                undefined -> throw({error, missing_tag});
                T -> T
            end,

        AdapterModule:info("Building OCI image: ~s", [Tag]),

        %% Find release using adapter
        case AdapterModule:find_release(AdapterState, Opts) of
            {ok, ReleaseName, ReleasePath} ->
                AdapterModule:info("Using release: ~s at ~s", [ReleaseName, ReleasePath]),

                %% Collect files from release
                case collect_release_files(ReleasePath) of
                    {ok, Files} ->
                        AdapterModule:info("Collected ~p files from release", [length(Files)]),

                        %% Build the image
                        AdapterModule:info("Base image: ~s", [BaseImage]),
                        Cmd = maps:get(cmd, Opts, DefaultCmd),

                        ProgressFn = make_progress_callback(),
                        PullAuth = get_pull_auth(),
                        BuildOpts = #{auth => PullAuth, progress => ProgressFn},

                        Result = build_image(
                            BaseImage,
                            Files,
                            ReleaseName,
                            Workdir,
                            EnvMap,
                            ExposePorts,
                            Labels,
                            Cmd,
                            BuildOpts
                        ),
                        clear_progress_line(),

                        case Result of
                            {ok, Image0} ->
                                %% Add description annotation
                                Image = add_description(Image0, Description),

                                %% Output the image (save and optionally push)
                                do_output(
                                    AdapterModule,
                                    AdapterState,
                                    Image,
                                    Tag,
                                    OutputOpt,
                                    PushRegistry,
                                    ChunkSize
                                );
                            {error, BuildError} ->
                                {error, {build_failed, BuildError}}
                        end;
                    {error, CollectError} ->
                        {error, {collect_failed, CollectError}}
                end;
            {error, FindError} ->
                {error, {release_not_found, FindError}}
        end
    catch
        throw:{error, Reason} ->
            {error, Reason};
        throw:Reason ->
            {error, Reason}
    end.

%% @private Output the image (save and optionally push)
-spec do_output(
    module(),
    term(),
    ocibuild:image(),
    binary(),
    binary() | undefined,
    binary() | undefined,
    pos_integer() | undefined
) -> {ok, term()} | {error, term()}.
do_output(AdapterModule, AdapterState, Image, Tag, OutputOpt, PushRegistry, ChunkSize) ->
    %% Determine output path
    %% Handle both undefined (Erlang) and nil (Elixir) for missing values
    OutputPath =
        case OutputOpt of
            undefined ->
                default_output_path(Tag);
            nil ->
                default_output_path(Tag);
            Path ->
                binary_to_list(Path)
        end,

    %% Save tarball
    AdapterModule:info("Saving image to ~s", [OutputPath]),
    case save_image(Image, OutputPath, Tag) of
        ok ->
            AdapterModule:info("Image saved successfully", []),

            %% Push if requested
            %% Handle both undefined (Erlang) and nil (Elixir), and empty binary
            case PushRegistry of
                undefined ->
                    AdapterModule:console("~nTo load the image:~n  podman load < ~s~n", [OutputPath]),
                    {ok, AdapterState};
                nil ->
                    AdapterModule:console("~nTo load the image:~n  podman load < ~s~n", [OutputPath]),
                    {ok, AdapterState};
                <<>> ->
                    AdapterModule:console("~nTo load the image:~n  podman load < ~s~n", [OutputPath]),
                    {ok, AdapterState};
                _ ->
                    do_push(AdapterModule, AdapterState, Image, Tag, PushRegistry, ChunkSize)
            end;
        {error, SaveError} ->
            {error, {save_failed, SaveError}}
    end.

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

%% @private Push image to registry
-spec do_push(
    module(),
    term(),
    ocibuild:image(),
    binary(),
    binary(),
    pos_integer() | undefined
) -> {ok, term()} | {error, term()}.
do_push(AdapterModule, AdapterState, Image, Tag, Registry, ChunkSize) ->
    {Repo, ImageTag} = parse_tag(Tag),
    Auth = get_push_auth(),

    %% Build push options
    %% ChunkSize is expected to be in bytes already (or undefined/nil)
    PushOpts =
        case ChunkSize of
            undefined -> #{};
            nil -> #{};
            Size when is_integer(Size), Size > 0 -> #{chunk_size => Size};
            _ -> #{}
        end,

    AdapterModule:info("Pushing to ~s/~s:~s", [Registry, Repo, ImageTag]),

    RepoTag = <<Repo/binary, ":", ImageTag/binary>>,
    case push_image(Image, Registry, RepoTag, Auth, PushOpts) of
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

-doc """
Build an OCI image from release files.

Uses "foreground" as the default start command (appropriate for Erlang releases).
""".
-spec build_image(
    BaseImage :: binary(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    ReleaseName :: string() | binary(),
    Workdir :: binary(),
    EnvMap :: map(),
    ExposePorts :: [non_neg_integer()],
    Labels :: map()
) -> {ok, ocibuild:image()} | {error, term()}.
build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels) ->
    build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, ~"foreground").

-doc """
Build an OCI image from release files with custom start command.

The Cmd parameter specifies the release start command:
- `"foreground"` - for Erlang releases (default)
- `"start"` - for Elixir releases
- `"daemon"` - for background mode
""".
-spec build_image(
    BaseImage :: binary(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    ReleaseName :: string() | binary(),
    Workdir :: binary(),
    EnvMap :: map(),
    ExposePorts :: [non_neg_integer()],
    Labels :: map(),
    Cmd :: binary()
) -> {ok, ocibuild:image()} | {error, term()}.
build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd) ->
    build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd, #{}).

-doc """
Build an OCI image from release files with custom start command and options.

Options:
- `auth` - Authentication credentials for pulling base image
- `progress` - Progress callback function
- `annotations` - Map of manifest annotations (e.g., `#{<<"org.opencontainers.image.description">> => <<"...">>}`)
""".
-spec build_image(
    BaseImage :: binary(),
    Files :: [{binary(), binary(), non_neg_integer()}],
    ReleaseName :: string() | binary(),
    Workdir :: binary(),
    EnvMap :: map(),
    ExposePorts :: [non_neg_integer()],
    Labels :: map(),
    Cmd :: binary(),
    Opts :: map()
) -> {ok, ocibuild:image()} | {error, term()}.
build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd, Opts) ->
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
-spec save_image(ocibuild:image(), file:filename(), binary()) -> ok | {error, term()}.
save_image(Image, OutputPath, Tag) ->
    SaveOpts = #{tag => Tag},
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
        HasProgress = is_integer(Total) andalso Total > 0 andalso Bytes > 0,
        IsComplete = Bytes =:= Total,
        AlreadyPrinted =
            case IsTTY of
                true ->
                    false;
                false when IsComplete ->
                    Key = {ocibuild_progress_done, Phase, LayerIndex},
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
                        layer -> "Downloading layer";
                        uploading -> "Uploading layer  "
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
