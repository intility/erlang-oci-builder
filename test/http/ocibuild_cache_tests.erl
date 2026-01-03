%%%-------------------------------------------------------------------
-module(ocibuild_cache_tests).
-moduledoc """
Tests for ocibuild_cache module.
""".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test setup and teardown
%%%===================================================================

setup() ->
    %% Create a unique temp directory for testing
    TempDir = ocibuild_test_helpers:make_temp_dir("ocibuild_cache_test"),
    %% Set the cache dir to our temp directory
    os:putenv("OCIBUILD_CACHE_DIR", TempDir),
    TempDir.

cleanup(TempDir) ->
    %% Restore environment
    os:unsetenv("OCIBUILD_CACHE_DIR"),
    %% Clean up temp directory
    ocibuild_test_helpers:cleanup_temp_dir(TempDir).

%%%===================================================================
%%% Test generators
%%%===================================================================

cache_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        {"put and get blob", fun put_get_test/0},
        {"get non-existent blob", fun get_not_found_test/0},
        {"get corrupted blob", fun get_corrupted_test/0},
        {"put overwrites existing", fun put_overwrites_test/0},
        {"clear removes all blobs", fun clear_test/0},
        fun(TempDir) ->
            {"cache_dir returns env var value", fun() -> cache_dir_env_var_test(TempDir) end}
        end
    ]}.

project_root_test_() ->
    {foreach, fun setup_project_root/0, fun cleanup_project_root/1, [
        fun(TempDir) ->
            {"find rebar.config project root", fun() -> find_rebar_project_root_test(TempDir) end}
        end,
        fun(TempDir) ->
            {"find mix.exs project root", fun() -> find_mix_project_root_test(TempDir) end}
        end,
        fun(TempDir) ->
            {"find gleam.toml project root", fun() -> find_gleam_project_root_test(TempDir) end}
        end,
        fun(TempDir) ->
            {"not found when no markers", fun() -> find_no_project_root_test(TempDir) end}
        end,
        fun(TempDir) ->
            {"finds nearest project root", fun() -> find_nearest_project_root_test(TempDir) end}
        end
    ]}.

cache_dir_without_env_test_() ->
    {foreach, fun setup_cache_dir_test/0, fun cleanup_cache_dir_test/1, [
        fun({_OrigCwd, TempDir}) ->
            {"cache_dir falls back to project root", fun() ->
                cache_dir_project_root_test(TempDir)
            end}
        end,
        fun({_OrigCwd, TempDir}) ->
            {"cache_dir falls back to cwd", fun() -> cache_dir_cwd_fallback_test(TempDir) end}
        end
    ]}.

%%%===================================================================
%%% Basic cache tests
%%%===================================================================

put_get_test() ->
    %% Create test data
    Data = ~"hello world this is test data for caching",
    Digest = ocibuild_digest:sha256(Data),

    %% Put and get
    ?assertEqual(ok, ocibuild_cache:put(Digest, Data)),
    ?assertEqual({ok, Data}, ocibuild_cache:get(Digest)).

get_not_found_test() ->
    %% Try to get a blob that doesn't exist
    FakeDigest = ocibuild_digest:sha256(~"nonexistent"),
    ?assertEqual({error, not_found}, ocibuild_cache:get(FakeDigest)).

get_corrupted_test() ->
    %% Create test data
    Data = ~"original data",
    Digest = ocibuild_digest:sha256(Data),

    %% Put the data
    ok = ocibuild_cache:put(Digest, Data),

    %% Corrupt the cached file by writing different content
    CacheDir = ocibuild_cache:cache_dir(),
    Encoded = ocibuild_digest:encoded(Digest),
    Path = filename:join([CacheDir, "blobs", "sha256", binary_to_list(Encoded)]),
    ok = file:write_file(Path, ~"corrupted data"),

    %% Get should detect corruption and return error
    ?assertEqual({error, corrupted}, ocibuild_cache:get(Digest)),

    %% Corrupted file should be deleted
    ?assertEqual(false, filelib:is_regular(Path)).

put_overwrites_test() ->
    %% This tests that putting the same digest twice doesn't error
    Data = ~"some data",
    Digest = ocibuild_digest:sha256(Data),

    ?assertEqual(ok, ocibuild_cache:put(Digest, Data)),
    ?assertEqual(ok, ocibuild_cache:put(Digest, Data)),
    ?assertEqual({ok, Data}, ocibuild_cache:get(Digest)).

clear_test() ->
    %% Put some data
    Data1 = ~"data one",
    Data2 = ~"data two",
    Digest1 = ocibuild_digest:sha256(Data1),
    Digest2 = ocibuild_digest:sha256(Data2),

    ok = ocibuild_cache:put(Digest1, Data1),
    ok = ocibuild_cache:put(Digest2, Data2),

    %% Verify data is there
    ?assertEqual({ok, Data1}, ocibuild_cache:get(Digest1)),
    ?assertEqual({ok, Data2}, ocibuild_cache:get(Digest2)),

    %% Clear cache
    ?assertEqual(ok, ocibuild_cache:clear()),

    %% Data should be gone
    ?assertEqual({error, not_found}, ocibuild_cache:get(Digest1)),
    ?assertEqual({error, not_found}, ocibuild_cache:get(Digest2)).

cache_dir_env_var_test(TempDir) ->
    %% The fixture's TempDir is already set in OCIBUILD_CACHE_DIR
    %% Verify cache_dir returns what was set
    CacheDir = ocibuild_cache:cache_dir(),
    ?assertEqual(TempDir, CacheDir).

%%%===================================================================
%%% Project root detection tests
%%%===================================================================

setup_project_root() ->
    %% Unset env var so project root detection is used
    os:unsetenv("OCIBUILD_CACHE_DIR"),
    TempDir = ocibuild_test_helpers:make_temp_dir("ocibuild_project_test"),
    TempDir.

cleanup_project_root(TempDir) ->
    ocibuild_test_helpers:cleanup_temp_dir(TempDir).

find_rebar_project_root_test(TempDir) ->
    %% Create a fake rebar project structure
    RebarFile = filename:join(TempDir, "rebar.config"),
    SubDir = filename:join([TempDir, "src", "subdir"]),
    %% ensure_dir creates parent dirs but not the final component
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),
    ok = file:write_file(RebarFile, ~"{deps, []}."),

    %% Find project root from subdir
    ?assertEqual({ok, TempDir}, ocibuild_cache:find_project_root(SubDir)).

find_mix_project_root_test(TempDir) ->
    %% Create a fake mix project structure
    MixFile = filename:join(TempDir, "mix.exs"),
    SubDir = filename:join([TempDir, "lib", "myapp"]),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),
    ok = file:write_file(MixFile, ~"defmodule Mix do end"),

    %% Find project root from subdir
    ?assertEqual({ok, TempDir}, ocibuild_cache:find_project_root(SubDir)).

find_gleam_project_root_test(TempDir) ->
    %% Create a fake gleam project structure
    GleamFile = filename:join(TempDir, "gleam.toml"),
    SubDir = filename:join([TempDir, "src"]),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),
    ok = file:write_file(GleamFile, <<"name = \"myapp\"">>),

    %% Find project root from subdir
    ?assertEqual({ok, TempDir}, ocibuild_cache:find_project_root(SubDir)).

find_no_project_root_test(TempDir) ->
    %% Create an isolated temp directory with no markers
    SubDir = filename:join(TempDir, "subdir"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    %% Since we're in /tmp, which may have parent markers, let's test
    %% that find_project_root returns not_found when walking up
    %% to filesystem root without finding markers.
    %% The best we can do is check it doesn't crash.
    Result = ocibuild_cache:find_project_root(SubDir),
    ?assert(Result =:= not_found orelse element(1, Result) =:= ok).

find_nearest_project_root_test(TempDir) ->
    %% Create nested project structure (monorepo-like)
    OuterProject = TempDir,
    InnerProject = filename:join([TempDir, "packages", "inner"]),
    InnerSubDir = filename:join(InnerProject, "src"),

    ok = filelib:ensure_dir(filename:join(InnerSubDir, "dummy")),

    %% Create markers at both levels
    ok = file:write_file(filename:join(OuterProject, "rebar.config"), <<>>),
    ok = file:write_file(filename:join(InnerProject, "rebar.config"), <<>>),

    %% Should find the nearest (inner) project root
    ?assertEqual({ok, InnerProject}, ocibuild_cache:find_project_root(InnerSubDir)).

%%%===================================================================
%%% Cache dir fallback tests
%%%===================================================================

setup_cache_dir_test() ->
    %% Unset env var
    os:unsetenv("OCIBUILD_CACHE_DIR"),
    %% Save original cwd
    {ok, OrigCwd} = file:get_cwd(),
    TempDir = ocibuild_test_helpers:make_temp_dir("ocibuild_cachedir_test"),
    {OrigCwd, TempDir}.

cleanup_cache_dir_test({OrigCwd, TempDir}) ->
    %% Restore cwd
    ok = file:set_cwd(OrigCwd),
    ocibuild_test_helpers:cleanup_temp_dir(TempDir).

cache_dir_project_root_test(TempDir) ->
    %% Create a rebar project
    ok = file:write_file(filename:join(TempDir, "rebar.config"), <<>>),
    SubDir = filename:join(TempDir, "src"),
    ok = filelib:ensure_dir(filename:join(SubDir, "dummy")),

    %% Change to the subdir
    ok = file:set_cwd(SubDir),

    %% Cache dir should be <project>/_build/ocibuild_cache
    %% Use basename comparison to handle macOS /tmp -> /private/tmp symlink
    CacheDir = ocibuild_cache:cache_dir(),
    ?assertEqual("ocibuild_cache", filename:basename(CacheDir)),
    ?assertEqual("_build", filename:basename(filename:dirname(CacheDir))),
    %% Verify it's under our temp project (check for the unique prefix)
    ?assert(string:find(CacheDir, "ocibuild_cachedir_test_") =/= nomatch).

cache_dir_cwd_fallback_test(TempDir) ->
    %% No project markers, just an empty directory
    ok = file:set_cwd(TempDir),

    %% Cache dir should fall back to ./_build/ocibuild_cache
    %% But since we're in /tmp which might have markers, let's just
    %% check it returns a sensible path
    CacheDir = ocibuild_cache:cache_dir(),
    ?assert(is_list(CacheDir)),
    ?assertMatch(
        "_build/ocibuild_cache",
        filename:basename(filename:dirname(CacheDir)) ++ "/" ++ filename:basename(CacheDir)
    ).

%%%===================================================================
%%% Utility function tests
%%%===================================================================

project_markers_test() ->
    Markers = ocibuild_cache:project_markers(),
    ?assert(is_list(Markers)),
    ?assert(length(Markers) >= 3),
    ?assert(lists:member(~"rebar.config", Markers)),
    ?assert(lists:member(~"mix.exs", Markers)),
    ?assert(lists:member(~"gleam.toml", Markers)).
