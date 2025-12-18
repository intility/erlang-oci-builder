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

%% Public API
-export([
    collect_release_files/1,
    collect_release_files/2,
    build_image/7,
    build_image/8,
    build_image/9
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
