%%%-------------------------------------------------------------------
-module(ocibuild_tar).
-moduledoc """
In-memory TAR archive builder.

This module builds POSIX ustar format TAR archives entirely in memory,
without writing to disk. This is essential for building OCI layers
efficiently.

The TAR format consists of 512-byte blocks:
- Each file has a 512-byte header followed by content padded to 512 bytes
- Archive ends with two 512-byte blocks of zeros
""".

-export([create/1, create/2, create_compressed/1, create_compressed/2]).

-define(BLOCK_SIZE, 512).
%% TAR header field offsets and sizes (POSIX ustar format)
-define(NAME_OFFSET, 0).
-define(NAME_SIZE, 100).
-define(MODE_OFFSET, 100).
-define(MODE_SIZE, 8).
-define(UID_OFFSET, 108).
-define(UID_SIZE, 8).
-define(GID_OFFSET, 116).
-define(GID_SIZE, 8).
-define(SIZE_OFFSET, 124).
-define(SIZE_SIZE, 12).
-define(MTIME_OFFSET, 136).
-define(MTIME_SIZE, 12).
-define(CHECKSUM_OFFSET, 148).
-define(CHECKSUM_SIZE, 8).
-define(TYPEFLAG_OFFSET, 156).
-define(LINKNAME_OFFSET, 157).
-define(LINKNAME_SIZE, 100).
-define(MAGIC_OFFSET, 257).
-define(VERSION_OFFSET, 263).
-define(UNAME_OFFSET, 265).
-define(UNAME_SIZE, 32).
-define(GNAME_OFFSET, 297).
-define(GNAME_SIZE, 32).
-define(DEVMAJOR_OFFSET, 329).
-define(DEVMAJOR_SIZE, 8).
-define(DEVMINOR_OFFSET, 337).
-define(DEVMINOR_SIZE, 8).
-define(PREFIX_OFFSET, 345).
-define(PREFIX_SIZE, 155).
%% Type flags

%% Regular file
-define(FILETYPE, $0).
%% Directory
-define(DIRTYPE, $5).

%% Common file permission modes

% rwxr-xr-x (executable files, directories)
-define(MODE_EXEC, 8#755).
% rw-r--r-- (regular files)
-define(MODE_FILE, 8#644).

-doc """
Create a TAR archive in memory.

Files are specified as `{Path, Content, Mode}` tuples.
Directories are created automatically for paths containing `/`.

```
TarData = ocibuild_tar:create([
    {~"/app/myapp", AppBinary, 8#755},
    {~"/app/config.json", ConfigJson, 8#644}
]).
```
""".
-spec create([{Path :: binary(), Content :: binary(), Mode :: integer()}]) -> binary().
create(Files) ->
    create(Files, #{}).

-doc """
Create a TAR archive in memory with options.

Options:
- `mtime`: Unix timestamp for file modification times (for reproducible builds)

Files are sorted alphabetically for reproducibility.

```
%% With fixed mtime for reproducible builds
TarData = ocibuild_tar:create(Files, #{mtime => 1700000000}).
```
""".
-spec create(Files, Opts) -> binary() when
    Files :: [{Path :: binary(), Content :: binary(), Mode :: integer()}],
    Opts :: #{mtime => non_neg_integer()}.
create(Files, Opts) ->
    %% Sort files alphabetically for reproducibility
    SortedFiles = lists:sort(fun({PathA, _, _}, {PathB, _, _}) -> PathA =< PathB end, Files),

    %% Get mtime (use provided value or current time)
    MTime = maps:get(mtime, Opts, erlang:system_time(second)),

    %% Collect all directories that need to be created
    Dirs = collect_directories(SortedFiles),

    %% Build directory entries first, then file entries
    DirEntries = [build_dir_entry(Dir, MTime) || Dir <- lists:sort(Dirs)],
    FileEntries = [
        build_file_entry(Path, Content, Mode, MTime)
     || {Path, Content, Mode} <- SortedFiles
    ],

    %% End of archive marker: two 512-byte zero blocks
    EndMarker = <<0:(?BLOCK_SIZE * 2 * 8)>>,

    iolist_to_binary([DirEntries, FileEntries, EndMarker]).

-doc "Create a gzip-compressed TAR archive in memory.".
-spec create_compressed([{Path :: binary(), Content :: binary(), Mode :: integer()}]) ->
    binary().
create_compressed(Files) ->
    create_compressed(Files, #{}).

-doc "Create a gzip-compressed TAR archive in memory with options.".
-spec create_compressed(Files, Opts) -> binary() when
    Files :: [{Path :: binary(), Content :: binary(), Mode :: integer()}],
    Opts :: #{mtime => non_neg_integer()}.
create_compressed(Files, Opts) ->
    Tar = create(Files, Opts),
    zlib:gzip(Tar).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Collect unique parent directories from file paths
-spec collect_directories([{binary(), binary(), integer()}]) -> [binary()].
collect_directories(Files) ->
    AllDirs =
        lists:foldl(
            fun({Path, _, _}, Acc) ->
                %% Normalize path and get all parent directories
                NormPath = normalize_path(Path),
                parent_dirs(NormPath, Acc)
            end,
            sets:new([{version, 2}]),
            Files
        ),
    sets:to_list(AllDirs).

%% Get all parent directories for a path
-spec parent_dirs(binary(), sets:set(binary())) -> sets:set(binary()).
parent_dirs(Path, Acc) ->
    case filename:dirname(Path) of
        ~"." ->
            Acc;
        ~"/" ->
            Acc;
        Parent ->
            Acc1 = sets:add_element(Parent, Acc),
            parent_dirs(Parent, Acc1)
    end.

%% Validate path doesn't contain traversal sequences
-spec validate_path(binary()) -> ok | {error, path_traversal}.
validate_path(Path) ->
    %% Split path and check each component for ".."
    Components = binary:split(Path, ~"/", [global]),
    case lists:member(~"..", Components) of
        true ->
            {error, path_traversal};
        false ->
            ok
    end.

%% Normalize path: ensure it starts with ./ for tar compatibility
%% Rejects paths containing ".." traversal sequences (security)
-spec normalize_path(binary()) -> binary().
normalize_path(Path) ->
    case validate_path(Path) of
        ok ->
            normalize_path_internal(Path);
        {error, path_traversal} ->
            error({path_traversal, Path})
    end.

-spec normalize_path_internal(binary()) -> binary().
normalize_path_internal(<<"/", Rest/binary>>) ->
    %% Keep as binary interpolation
    <<"./", Rest/binary>>;
normalize_path_internal(<<"./", _/binary>> = Path) ->
    Path;
normalize_path_internal(Path) ->
    %% Keep as binary interpolation
    <<"./", Path/binary>>.

%% Build a directory entry
-spec build_dir_entry(binary(), non_neg_integer()) -> iolist().
build_dir_entry(Path, MTime) ->
    %% Directories have trailing slash in tar
    DirPath =
        case binary:last(Path) of
            $/ ->
                Path;
            _ ->
                <<Path/binary, "/">>
        end,
    NormPath = normalize_path(DirPath),
    Header = build_header(NormPath, 0, ?MODE_EXEC, ?DIRTYPE, MTime),
    [Header].

%% Build a file entry (header + content + padding)
-spec build_file_entry(binary(), binary(), integer(), non_neg_integer()) -> iolist().
build_file_entry(Path, Content, Mode, MTime) ->
    NormPath = normalize_path(Path),
    Size = byte_size(Content),
    Header = build_header(NormPath, Size, Mode, ?FILETYPE, MTime),
    Padding = padding(Size),
    [Header, Content, Padding].

%% Build a TAR header
-spec build_header(binary(), non_neg_integer(), integer(), byte(), non_neg_integer()) -> binary().
build_header(Name, Size, Mode, TypeFlag, MTime) ->
    %% Handle long names using prefix field if needed
    {Prefix, ShortName} = split_name(Name),

    %% Build header with placeholder checksum (spaces)

    % name
    H0 = <<
        (pad_right(ShortName, ?NAME_SIZE))/binary,
        % mode
        (octal(Mode, ?MODE_SIZE))/binary,
        % uid
        (octal(0, ?UID_SIZE))/binary,
        % gid
        (octal(0, ?GID_SIZE))/binary,
        % size
        (octal(Size, ?SIZE_SIZE))/binary,
        % mtime
        (octal(MTime, ?MTIME_SIZE))/binary,
        % checksum placeholder (8 spaces)
        "        ",
        % typeflag
        TypeFlag,
        % linkname
        (pad_right(<<>>, ?LINKNAME_SIZE))/binary,
        % magic
        "ustar",
        % null after magic
        0,
        % version
        "00",
        % uname
        (pad_right(~"root", ?UNAME_SIZE))/binary,
        % gname
        (pad_right(~"root", ?GNAME_SIZE))/binary,
        % devmajor
        (octal(0, ?DEVMAJOR_SIZE))/binary,
        % devminor
        (octal(0, ?DEVMINOR_SIZE))/binary,
        % prefix
        (pad_right(Prefix, ?PREFIX_SIZE))/binary
    >>,

    %% Pad to full block size
    H1 = pad_right(H0, ?BLOCK_SIZE),

    %% Calculate and insert checksum
    Checksum = compute_checksum(H1),
    ChecksumStr = octal_checksum(Checksum),

    %% Replace placeholder with actual checksum
    <<Before:(?CHECKSUM_OFFSET)/binary, _:(?CHECKSUM_SIZE)/binary, After/binary>> = H1,
    <<Before/binary, ChecksumStr/binary, After/binary>>.

%% Split name into prefix and name if too long
-spec split_name(binary()) -> {binary(), binary()}.
split_name(Name) when byte_size(Name) =< ?NAME_SIZE ->
    {<<>>, Name};
split_name(Name) ->
    %% Try to find a good split point (at a /)
    case find_split_point(Name) of
        {ok, Prefix, ShortName} when
            byte_size(ShortName) =< ?NAME_SIZE, byte_size(Prefix) =< ?PREFIX_SIZE
        ->
            {Prefix, ShortName};
        _ ->
            %% Truncate if we can't split properly
            {<<>>, binary:part(Name, 0, min(byte_size(Name), ?NAME_SIZE))}
    end.

%% Find a split point at a directory separator
-spec find_split_point(binary()) -> {ok, binary(), binary()} | error.
find_split_point(Name) ->
    %% Find last / that gives us a valid split
    case binary:matches(Name, ~"/") of
        [] ->
            error;
        Matches ->
            find_valid_split(Name, lists:reverse(Matches))
    end.

find_valid_split(_Name, []) ->
    error;
find_valid_split(Name, [{Pos, _} | Rest]) ->
    Prefix = binary:part(Name, 0, Pos),
    ShortName = binary:part(Name, Pos + 1, byte_size(Name) - Pos - 1),
    if
        byte_size(ShortName) =< ?NAME_SIZE, byte_size(Prefix) =< ?PREFIX_SIZE ->
            {ok, Prefix, ShortName};
        true ->
            find_valid_split(Name, Rest)
    end.

%% Compute checksum (sum of all bytes, treating checksum field as spaces)
-spec compute_checksum(binary()) -> integer().
compute_checksum(Header) ->
    lists:sum(binary_to_list(Header)).

%% Format number as octal string for tar header
%% TAR octal fields are null-terminated, with optional space before null
-spec octal(integer(), integer()) -> binary().
octal(N, Width) ->
    S = integer_to_list(N, 8),
    %% Field format: left-pad with zeros, leave room for trailing null (and optional space)
    %% Standard format: digits + space + null, or just digits + null

    % Leave room for trailing null
    MaxDigits = Width - 1,
    case length(S) > MaxDigits of
        true ->
            %% Truncate if too long (shouldn't happen with valid data)
            Truncated = lists:sublist(S, MaxDigits),
            list_to_binary(Truncated ++ [0]);
        false ->
            PadLen = MaxDigits - length(S),
            Padded = lists:duplicate(PadLen, $0) ++ S ++ [0],
            list_to_binary(Padded)
    end.

%% Format checksum (6 octal digits + null + space)
-spec octal_checksum(integer()) -> binary().
octal_checksum(N) ->
    S = integer_to_list(N, 8),
    Padded = lists:duplicate(6 - length(S), $0) ++ S,
    list_to_binary(Padded ++ [0, $\s]).

%% Pad binary to length with null bytes
-spec pad_right(binary(), non_neg_integer()) -> binary().
pad_right(Bin, Len) when byte_size(Bin) >= Len ->
    binary:part(Bin, 0, Len);
pad_right(Bin, Len) ->
    PadSize = Len - byte_size(Bin),
    <<Bin/binary, 0:PadSize/unit:8>>.

%% Calculate padding needed to align to block size
-spec padding(non_neg_integer()) -> binary().
padding(Size) ->
    case Size rem ?BLOCK_SIZE of
        0 ->
            <<>>;
        R ->
            <<0:((?BLOCK_SIZE - R) * 8)>>
    end.
