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

%% TAR header field sizes (POSIX ustar format)
-define(NAME_SIZE, 100).
-define(MODE_SIZE, 8).
-define(UID_SIZE, 8).
-define(GID_SIZE, 8).
-define(SIZE_SIZE, 12).
-define(MTIME_SIZE, 12).
-define(CHECKSUM_OFFSET, 148).
-define(CHECKSUM_SIZE, 8).
-define(CHECKSUM_DIGITS, 6).
-define(LINKNAME_SIZE, 100).
-define(UNAME_SIZE, 32).
-define(GNAME_SIZE, 32).
-define(DEVMAJOR_SIZE, 8).
-define(DEVMINOR_SIZE, 8).
-define(PREFIX_SIZE, 155).

%% Type flags

%% Maximum valid file mode (rwxrwxrwx + setuid + setgid + sticky = 7777 octal)
-define(MAX_MODE, 8#7777).

%% Regular file
-define(FILETYPE, $0).
%% Directory
-define(DIRTYPE, $5).
%% PAX extended header (for long paths)
-define(PAXTYPE, $x).

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
    %% Normalize all paths first (validates and adds ./ prefix)
    NormalizedFiles = [{normalize_path(Path), Content, Mode} || {Path, Content, Mode} <- Files],

    %% Check for duplicate paths after normalization
    %% (e.g., "/app/file" and "app/file" both become "./app/file")
    ok = check_duplicates(NormalizedFiles),

    %% Sort files alphabetically for reproducibility
    SortedFiles = lists:sort(
        fun({PathA, _, _}, {PathB, _, _}) -> PathA =< PathB end, NormalizedFiles
    ),

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

%% Check for duplicate paths in the file list
-spec check_duplicates([{binary(), binary(), integer()}]) -> ok.
check_duplicates(Files) ->
    Paths = [Path || {Path, _, _} <- Files],
    case length(Paths) =:= sets:size(sets:from_list(Paths)) of
        true -> ok;
        false -> error({duplicate_paths, find_duplicates(Paths)})
    end.

-spec find_duplicates([binary()]) -> [binary()].
find_duplicates(Paths) ->
    {_, Dups} = lists:foldl(
        fun(Path, {Seen, Dups}) ->
            case sets:is_element(Path, Seen) of
                true -> {Seen, [Path | Dups]};
                false -> {sets:add_element(Path, Seen), Dups}
            end
        end,
        {sets:new(), []},
        Paths
    ),
    lists:usort(Dups).

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
                %% Path is already normalized, get all parent directories
                parent_dirs(Path, Acc)
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

%% Validate path for security issues:
%% - No ".." traversal sequences
%% - No null bytes (could truncate paths in C-based tools)
%% - No empty paths
-spec validate_path(binary()) -> ok | {error, path_traversal | null_byte | empty_path}.
validate_path(<<>>) ->
    {error, empty_path};
validate_path(Path) ->
    case binary:match(Path, <<0>>) of
        nomatch ->
            Components = binary:split(Path, ~"/", [global]),
            case lists:member(~"..", Components) of
                true -> {error, path_traversal};
                false -> ok
            end;
        _ ->
            {error, null_byte}
    end.

%% Normalize path: ensure it starts with ./ for tar compatibility
%% Raises error for security violations (traversal, null bytes, empty paths)
-spec normalize_path(binary()) -> binary().
normalize_path(Path) ->
    case validate_path(Path) of
        ok ->
            normalize_path_internal(Path);
        {error, Reason} ->
            error({Reason, Path})
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

%% Build a directory entry (Path is already normalized)
%% Uses PAX extended headers for paths that don't fit in ustar format
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
    case path_encoding(DirPath) of
        {ustar, Prefix, Name} ->
            Header = build_header_with_parts(Prefix, Name, 0, ?MODE_EXEC, ?DIRTYPE, MTime),
            [Header];
        pax ->
            PaxData = build_pax_record(DirPath),
            PaxHeader = build_pax_header(PaxData, MTime),
            PaxPadding = padding(byte_size(PaxData)),
            TruncatedName = binary:part(DirPath, 0, min(byte_size(DirPath), ?NAME_SIZE)),
            UstarHeader = build_header_with_parts(
                <<>>, TruncatedName, 0, ?MODE_EXEC, ?DIRTYPE, MTime
            ),
            [PaxHeader, PaxData, PaxPadding, UstarHeader]
    end.

%% Validate file mode is within valid range
-spec validate_mode(integer()) -> ok.
validate_mode(Mode) when Mode >= 0, Mode =< ?MAX_MODE ->
    ok;
validate_mode(Mode) ->
    error({invalid_mode, Mode}).

%% Build a file entry (header + content + padding)
%% Path is already normalized. Uses PAX extended headers for paths that don't fit in ustar format.
-spec build_file_entry(binary(), binary(), integer(), non_neg_integer()) -> iolist().
build_file_entry(Path, Content, Mode, MTime) ->
    ok = validate_mode(Mode),
    Size = byte_size(Content),
    Padding = padding(Size),
    case path_encoding(Path) of
        {ustar, Prefix, Name} ->
            Header = build_header_with_parts(Prefix, Name, Size, Mode, ?FILETYPE, MTime),
            [Header, Content, Padding];
        pax ->
            PaxData = build_pax_record(Path),
            PaxHeader = build_pax_header(PaxData, MTime),
            PaxPadding = padding(byte_size(PaxData)),
            %% Truncate name for the ustar header (readers will use PAX path)
            TruncatedName = binary:part(Path, 0, min(byte_size(Path), ?NAME_SIZE)),
            UstarHeader = build_header_with_parts(
                <<>>, TruncatedName, Size, Mode, ?FILETYPE, MTime
            ),
            [PaxHeader, PaxData, PaxPadding, UstarHeader, Content, Padding]
    end.

%% Build a TAR header with explicit prefix and name
-spec build_header_with_parts(
    binary(), binary(), non_neg_integer(), integer(), byte(), non_neg_integer()
) -> binary().
build_header_with_parts(Prefix, Name, Size, Mode, TypeFlag, MTime) ->
    %% Build header with placeholder checksum (spaces)

    % name
    H0 = <<
        (pad_right(Name, ?NAME_SIZE))/binary,
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

%% Determine how to encode a path: ustar (with optional prefix) or PAX extended header
-spec path_encoding(binary()) -> {ustar, Prefix :: binary(), Name :: binary()} | pax.
path_encoding(Path) when byte_size(Path) =< ?NAME_SIZE ->
    {ustar, <<>>, Path};
path_encoding(Path) ->
    case try_ustar_split(Path) of
        {ok, Prefix, Name} -> {ustar, Prefix, Name};
        error -> pax
    end.

%% Try to split path at a `/` so prefix <= 155 bytes and name <= 100 bytes
-spec try_ustar_split(binary()) -> {ok, binary(), binary()} | error.
try_ustar_split(Path) ->
    case binary:matches(Path, ~"/") of
        [] ->
            error;
        Matches ->
            %% Try splits from rightmost `/` first (maximizes prefix usage)
            try_splits(Path, lists:reverse(Matches))
    end.

-spec try_splits(binary(), [{non_neg_integer(), non_neg_integer()}]) ->
    {ok, binary(), binary()} | error.
try_splits(_Path, []) ->
    error;
try_splits(Path, [{Pos, _} | Rest]) ->
    Prefix = binary:part(Path, 0, Pos),
    Name = binary:part(Path, Pos + 1, byte_size(Path) - Pos - 1),
    case byte_size(Name) =< ?NAME_SIZE andalso byte_size(Prefix) =< ?PREFIX_SIZE of
        true -> {ok, Prefix, Name};
        false -> try_splits(Path, Rest)
    end.

%% Build a PAX extended header record for the path attribute
%% Format: "<length> path=<value>\n" where length includes itself
-spec build_pax_record(binary()) -> binary().
build_pax_record(Path) ->
    %% The tricky part: length field includes itself, so we iterate to find it
    Value = <<"path=", Path/binary, $\n>>,
    %% Start with estimate: " path=<path>\n" needs length prefix
    find_pax_length(Value, byte_size(Value) + 1, 0).

%% Maximum iterations for PAX length calculation (should converge in 1-2)
-define(MAX_PAX_ITERATIONS, 10).

-spec find_pax_length(binary(), non_neg_integer(), non_neg_integer()) -> binary().
find_pax_length(_Value, _EstimatedLen, Iterations) when Iterations >= ?MAX_PAX_ITERATIONS ->
    error(pax_length_convergence_failed);
find_pax_length(Value, EstimatedLen, Iterations) ->
    LenStr = integer_to_binary(EstimatedLen),
    ActualLen = byte_size(LenStr) + 1 + byte_size(Value),
    case ActualLen of
        EstimatedLen ->
            <<LenStr/binary, " ", Value/binary>>;
        _ ->
            find_pax_length(Value, ActualLen, Iterations + 1)
    end.

%% Build a PAX extended header entry (typeflag 'x')
-spec build_pax_header(binary(), non_neg_integer()) -> binary().
build_pax_header(PaxData, MTime) ->
    Size = byte_size(PaxData),
    %% PAX header uses a placeholder name
    PaxName = ~"./PaxHeaders/entry",
    build_header_with_parts(<<>>, PaxName, Size, ?MODE_FILE, ?PAXTYPE, MTime).

%% Compute checksum (sum of all bytes, treating checksum field as spaces)
-spec compute_checksum(binary()) -> integer().
compute_checksum(Header) ->
    lists:sum(binary_to_list(Header)).

%% Format number as octal string for tar header
%% TAR octal fields are null-terminated, with optional space before null
-spec octal(non_neg_integer(), pos_integer()) -> binary().
octal(N, Width) when N >= 0 ->
    S = integer_to_list(N, 8),
    MaxDigits = Width - 1,
    case length(S) > MaxDigits of
        true ->
            error({octal_overflow, N, Width});
        false ->
            PadLen = MaxDigits - length(S),
            Padded = lists:duplicate(PadLen, $0) ++ S ++ [0],
            list_to_binary(Padded)
    end;
octal(N, _Width) ->
    error({negative_value, N}).

%% Format checksum (6 octal digits + null + space)
-spec octal_checksum(non_neg_integer()) -> binary().
octal_checksum(N) ->
    S = integer_to_list(N, 8),
    Padded = lists:duplicate(?CHECKSUM_DIGITS - length(S), $0) ++ S,
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
