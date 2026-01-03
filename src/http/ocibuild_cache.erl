%%%-------------------------------------------------------------------
-module(ocibuild_cache).
-moduledoc """
Layer caching for OCI image builds.

Caches downloaded base image layers locally to avoid re-downloading on every build.
Designed for CI/CD workflows where caching the `_build/ocibuild_cache` directory
provides significant build time improvements.

Cache location resolution (in order):
1. `OCIBUILD_CACHE_DIR` environment variable (explicit override)
2. Project root detection → `<project>/_build/ocibuild_cache/`
3. Fall back to `./_build/ocibuild_cache/` (current directory)

Project root is detected by walking up the directory tree looking for
`rebar.config`, `mix.exs`, or `gleam.toml`.

CI Integration Example:
```yaml
# GitHub Actions
- uses: actions/cache@v4
  with:
    path: _build/ocibuild_cache
    key: ocibuild-${{ runner.os }}-${{ hashFiles('rebar.lock') }}

# GitLab CI
cache:
  paths:
    - _build/ocibuild_cache/
```
""".

-export([
    get/1,
    put/2,
    cache_dir/0,
    clear/0
]).

%% Exports for testing
-ifdef(TEST).
-export([find_project_root/1, project_markers/0]).
-endif.

%% Project marker files that indicate a project root
-define(PROJECT_MARKERS, [~"rebar.config", ~"mix.exs", ~"gleam.toml"]).

%%%===================================================================
%%% API
%%%===================================================================

-doc """
Get a cached blob by digest.

Returns `{ok, Data}` if the blob is found and its digest matches.
Returns `{error, not_found}` if the blob is not in the cache.
Returns `{error, corrupted}` if the cached blob doesn't match its digest.
""".
-spec get(binary()) -> {ok, binary()} | {error, not_found | corrupted}.
get(Digest) ->
    Path = blob_path(Digest),
    case file:read_file(Path) of
        {ok, Data} ->
            %% Verify digest to protect against cache corruption
            ActualDigest = ocibuild_digest:sha256(Data),
            case ActualDigest =:= Digest of
                true ->
                    {ok, Data};
                false ->
                    %% Cache is corrupted, delete the bad file
                    _ = file:delete(Path),
                    {error, corrupted}
            end;
        {error, enoent} ->
            {error, not_found};
        {error, _} ->
            {error, not_found}
    end.

-doc """
Store a blob in the cache.

The blob is stored under its digest, so it can be retrieved later with `get/1`.
The digest is computed from the data, not provided as a parameter, to ensure
that only valid data is cached.
""".
-spec put(binary(), binary()) -> ok | {error, term()}.
put(Digest, Data) ->
    Path = blob_path(Digest),
    %% Ensure directory exists
    ok = filelib:ensure_dir(Path),
    %% Write atomically using a temp file + rename to prevent partial writes
    TmpPath = Path ++ ".tmp." ++ integer_to_list(erlang:unique_integer([positive])),
    case file:write_file(TmpPath, Data) of
        ok ->
            case file:rename(TmpPath, Path) of
                ok ->
                    ok;
                {error, Reason} ->
                    _ = file:delete(TmpPath),
                    {error, Reason}
            end;
        {error, Reason} ->
            _ = file:delete(TmpPath),
            {error, Reason}
    end.

-doc """
Get the cache directory path.

Resolution order:
1. `OCIBUILD_CACHE_DIR` environment variable
2. Project root + `_build/ocibuild_cache/`
3. `./_build/ocibuild_cache/` (current directory fallback)
""".
-spec cache_dir() -> file:filename().
cache_dir() ->
    case os:getenv("OCIBUILD_CACHE_DIR") of
        false ->
            %% Try to find project root
            {ok, Cwd} = file:get_cwd(),
            case find_project_root(Cwd) of
                {ok, ProjectRoot} ->
                    filename:join([ProjectRoot, "_build", "ocibuild_cache"]);
                not_found ->
                    filename:join(["_build", "ocibuild_cache"])
            end;
        EnvDir ->
            EnvDir
    end.

-doc """
Clear the entire cache.

Removes all cached blobs. Use with caution as this will force re-download
of all layers on the next build.
""".
-spec clear() -> ok | {error, term()}.
clear() ->
    CacheDir = cache_dir(),
    case filelib:is_dir(CacheDir) of
        true ->
            delete_dir_recursive(CacheDir);
        false ->
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Compute path for a blob in the cache
-spec blob_path(binary()) -> file:filename().
blob_path(Digest) ->
    CacheDir = cache_dir(),
    Encoded = ocibuild_digest:encoded(Digest),
    filename:join([CacheDir, "blobs", "sha256", binary_to_list(Encoded)]).

%% Find project root by walking up directory tree
-spec find_project_root(file:filename()) -> {ok, file:filename()} | not_found.
find_project_root(Dir) ->
    case has_project_marker(Dir) of
        true ->
            {ok, Dir};
        false ->
            Parent = filename:dirname(Dir),
            case Parent =:= Dir of
                %% Reached filesystem root
                true ->
                    not_found;
                false ->
                    find_project_root(Parent)
            end
    end.

%% Check if directory contains any project marker file
-spec has_project_marker(file:filename()) -> boolean().
has_project_marker(Dir) ->
    lists:any(
        fun(Marker) ->
            Path = filename:join(Dir, Marker),
            filelib:is_regular(Path)
        end,
        project_markers()
    ).

%% Project marker files (exposed for testing)
-spec project_markers() -> [binary()].
project_markers() ->
    ?PROJECT_MARKERS.

%% Recursively delete a directory
-spec delete_dir_recursive(file:filename()) -> ok | {error, term()}.
delete_dir_recursive(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            Results = lists:map(
                fun(File) ->
                    Path = filename:join(Dir, File),
                    case filelib:is_dir(Path) of
                        true ->
                            delete_dir_recursive(Path);
                        false ->
                            file:delete(Path)
                    end
                end,
                Files
            ),
            case lists:all(fun(R) -> R =:= ok end, Results) of
                true ->
                    file:del_dir(Dir);
                false ->
                    {error, could_not_delete_contents}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
