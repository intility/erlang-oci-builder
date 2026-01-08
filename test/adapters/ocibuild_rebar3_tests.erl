%%%-------------------------------------------------------------------
-module(ocibuild_rebar3_tests).
-moduledoc "Tests for the rebar3 provider (ocibuild_rebar3)".

-include_lib("eunit/include/eunit.hrl").

-import(ocibuild_test_helpers, [make_temp_dir/1, cleanup_temp_dir/1]).

%%%===================================================================
%%% format_error tests
%%%===================================================================

format_error_missing_tag_test() ->
    Result = ocibuild_rebar3:format_error(missing_tag),
    ?assert(is_list(Result)),
    ?assert(string:find(Result, "--tag") =/= nomatch).

format_error_release_not_found_test() ->
    Result = ocibuild_rebar3:format_error({release_not_found, "myapp", "/path/to/rel"}),
    ?assert(is_list(Result)),
    ?assert(string:find(Result, "myapp") =/= nomatch).

format_error_no_release_test() ->
    Result = ocibuild_rebar3:format_error({no_release_configured, []}),
    ?assert(is_list(Result)),
    ?assert(string:find(Result, "No release") =/= nomatch).

format_error_file_read_error_test() ->
    Result = ocibuild_rebar3:format_error({file_read_error, "/path/file", enoent}),
    ?assert(is_list(Result)),
    ?assert(string:find(Result, "/path/file") =/= nomatch).

format_error_save_failed_test() ->
    Result = ocibuild_rebar3:format_error({save_failed, some_reason}),
    ?assert(is_list(Result)),
    ?assert(string:find(Result, "save") =/= nomatch).

format_error_push_failed_test() ->
    Result = ocibuild_rebar3:format_error({push_failed, auth_error}),
    ?assert(is_list(Result)),
    ?assert(string:find(Result, "push") =/= nomatch).

format_error_base_image_failed_test() ->
    Result = ocibuild_rebar3:format_error({build_failed, {base_image_failed, not_found}}),
    ?assert(is_list(Result)),
    ?assert(string:find(Result, "base image") =/= nomatch).

format_error_generic_test() ->
    Result = ocibuild_rebar3:format_error({some, random, error}),
    ?assert(is_list(Result)).

%%%===================================================================
%%% find_relx_release tests
%%%===================================================================

find_relx_release_simple_test() ->
    Config = [{release, {myapp, "1.0.0"}, [kernel, stdlib]}],
    ?assertEqual({ok, "myapp"}, ocibuild_rebar3:find_relx_release(Config)).

find_relx_release_with_opts_test() ->
    Config = [{release, {myapp, "1.0.0"}, [kernel], [{dev_mode, true}]}],
    ?assertEqual({ok, "myapp"}, ocibuild_rebar3:find_relx_release(Config)).

find_relx_release_empty_test() ->
    ?assertEqual(error, ocibuild_rebar3:find_relx_release([])).

find_relx_release_no_release_test() ->
    Config = [{profiles, [{prod, []}]}, {deps, []}],
    ?assertEqual(error, ocibuild_rebar3:find_relx_release(Config)).

find_relx_release_multiple_test() ->
    %% Should return the first release found
    Config = [
        {release, {app1, "1.0"}, [kernel]},
        {release, {app2, "2.0"}, [stdlib]}
    ],
    ?assertEqual({ok, "app1"}, ocibuild_rebar3:find_relx_release(Config)).

%%%===================================================================
%%% get_base_image tests
%%%===================================================================

get_base_image_from_args_test() ->
    Args = [{base, "alpine:3.19"}],
    Config = [{base_image, ~"debian:stable-slim"}],
    ?assertEqual(~"alpine:3.19", ocibuild_rebar3:get_base_image(Args, Config)).

get_base_image_from_config_test() ->
    Args = [],
    Config = [{base_image, ~"ubuntu:22.04"}],
    ?assertEqual(~"ubuntu:22.04", ocibuild_rebar3:get_base_image(Args, Config)).

get_base_image_default_test() ->
    Args = [],
    Config = [],
    ?assertEqual(~"debian:stable-slim", ocibuild_rebar3:get_base_image(Args, Config)).

%%%===================================================================
%%% rebar.lock parsing tests
%%%===================================================================

parse_rebar_lock_new_format_test() ->
    %% New format with version tuple: {"1.2.0", [{...}]}
    LockContent = <<
        "{\"1.2.0\",\n"
        "[{<<\"cowboy\">>,{pkg,<<\"cowboy\">>,<<\"2.10.0\">>},0},\n"
        " {<<\"cowlib\">>,{pkg,<<\"cowlib\">>,<<\"2.12.1\">>},1}]}.\n"
    >>,
    TmpFile = make_temp_lock_file("rebar_new", LockContent),
    {ok, Deps} = ocibuild_rebar3:parse_rebar_lock(TmpFile),
    file:delete(TmpFile),
    cleanup_temp_dir(filename:dirname(TmpFile)),
    ?assertEqual(2, length(Deps)),
    [Cowboy, Cowlib] = lists:sort(
        fun(A, B) ->
            maps:get(name, A) < maps:get(name, B)
        end,
        Deps
    ),
    ?assertEqual(~"cowboy", maps:get(name, Cowboy)),
    ?assertEqual(~"2.10.0", maps:get(version, Cowboy)),
    ?assertEqual(~"hex", maps:get(source, Cowboy)),
    ?assertEqual(~"cowlib", maps:get(name, Cowlib)),
    ?assertEqual(~"2.12.1", maps:get(version, Cowlib)).

parse_rebar_lock_old_format_test() ->
    %% Old format: just a list without version tuple
    LockContent = <<
        "[{<<\"cowboy\">>,{pkg,<<\"cowboy\">>,<<\"2.10.0\">>},0}].\n"
    >>,
    TmpFile = make_temp_lock_file("rebar_old", LockContent),
    {ok, Deps} = ocibuild_rebar3:parse_rebar_lock(TmpFile),
    file:delete(TmpFile),
    cleanup_temp_dir(filename:dirname(TmpFile)),
    ?assertEqual(1, length(Deps)),
    [Cowboy] = Deps,
    ?assertEqual(~"cowboy", maps:get(name, Cowboy)),
    ?assertEqual(~"2.10.0", maps:get(version, Cowboy)),
    ?assertEqual(~"hex", maps:get(source, Cowboy)).

parse_rebar_lock_git_dep_test() ->
    LockContent = <<
        "[{<<\"mylib\">>,{git,\"https://github.com/org/mylib.git\",{ref,\"abc123\"}},0}].\n"
    >>,
    TmpFile = make_temp_lock_file("rebar_git", LockContent),
    {ok, Deps} = ocibuild_rebar3:parse_rebar_lock(TmpFile),
    file:delete(TmpFile),
    cleanup_temp_dir(filename:dirname(TmpFile)),
    ?assertEqual(1, length(Deps)),
    [Mylib] = Deps,
    ?assertEqual(~"mylib", maps:get(name, Mylib)),
    ?assertEqual(~"abc123", maps:get(version, Mylib)),
    ?assertEqual(~"https://github.com/org/mylib.git", maps:get(source, Mylib)).

parse_rebar_lock_empty_test() ->
    LockContent = ~"[].\n",
    TmpFile = make_temp_lock_file("rebar_empty", LockContent),
    {ok, Deps} = ocibuild_rebar3:parse_rebar_lock(TmpFile),
    file:delete(TmpFile),
    cleanup_temp_dir(filename:dirname(TmpFile)),
    ?assertEqual([], Deps).

parse_rebar_lock_missing_test() ->
    {ok, Deps} = ocibuild_rebar3:parse_rebar_lock("/nonexistent/path/rebar.lock"),
    ?assertEqual([], Deps).

%%%===================================================================
%%% Tag normalization tests
%%%===================================================================

%% normalize_tag/1 tests

normalize_tag_binary_test() ->
    ?assertEqual(~"myapp:1.0.0", ocibuild_rebar3:normalize_tag(~"myapp:1.0.0")).

normalize_tag_string_test() ->
    ?assertEqual(~"myapp:1.0.0", ocibuild_rebar3:normalize_tag("myapp:1.0.0")).

normalize_tag_atom_test() ->
    ?assertEqual(~"myapp", ocibuild_rebar3:normalize_tag(myapp)).

%% normalize_tags/1 tests

normalize_tags_empty_list_test() ->
    ?assertEqual([], ocibuild_rebar3:normalize_tags([])).

normalize_tags_single_binary_test() ->
    ?assertEqual([~"myapp:1.0.0"], ocibuild_rebar3:normalize_tags(~"myapp:1.0.0")).

normalize_tags_single_string_test() ->
    %% Single string (charlist) should be treated as one tag
    ?assertEqual([~"myapp:1.0.0"], ocibuild_rebar3:normalize_tags("myapp:1.0.0")).

normalize_tags_list_of_binaries_test() ->
    Tags = [~"myapp:1.0.0", ~"myapp:latest"],
    ?assertEqual([~"myapp:1.0.0", ~"myapp:latest"], ocibuild_rebar3:normalize_tags(Tags)).

normalize_tags_list_of_strings_test() ->
    Tags = ["myapp:1.0.0", "myapp:latest"],
    ?assertEqual([~"myapp:1.0.0", ~"myapp:latest"], ocibuild_rebar3:normalize_tags(Tags)).

normalize_tags_mixed_types_test() ->
    Tags = [~"myapp:1.0.0", "myapp:latest", myapp],
    ?assertEqual([~"myapp:1.0.0", ~"myapp:latest", ~"myapp"], ocibuild_rebar3:normalize_tags(Tags)).

normalize_tags_invalid_type_test() ->
    %% Non-list, non-binary types return empty list
    ?assertEqual([], ocibuild_rebar3:normalize_tags(123)),
    ?assertEqual([], ocibuild_rebar3:normalize_tags(undefined)).

%% get_tags/2 tests

get_tags_from_cli_single_test() ->
    Args = [{tag, "myapp:1.0.0"}],
    Config = [],
    ?assertEqual([~"myapp:1.0.0"], ocibuild_rebar3:get_tags(Args, Config)).

get_tags_from_cli_multiple_test() ->
    Args = [{tag, "myapp:1.0.0"}, {tag, "myapp:latest"}],
    Config = [],
    ?assertEqual([~"myapp:1.0.0", ~"myapp:latest"], ocibuild_rebar3:get_tags(Args, Config)).

get_tags_from_config_single_string_test() ->
    Args = [],
    Config = [{tag, "myapp:1.0.0"}],
    ?assertEqual([~"myapp:1.0.0"], ocibuild_rebar3:get_tags(Args, Config)).

get_tags_from_config_single_binary_test() ->
    Args = [],
    Config = [{tag, ~"myapp:1.0.0"}],
    ?assertEqual([~"myapp:1.0.0"], ocibuild_rebar3:get_tags(Args, Config)).

get_tags_from_config_list_test() ->
    Args = [],
    Config = [{tag, ["myapp:1.0.0", "myapp:latest"]}],
    ?assertEqual([~"myapp:1.0.0", ~"myapp:latest"], ocibuild_rebar3:get_tags(Args, Config)).

get_tags_cli_overrides_config_test() ->
    Args = [{tag, "cli:tag"}],
    Config = [{tag, "config:tag"}],
    ?assertEqual([~"cli:tag"], ocibuild_rebar3:get_tags(Args, Config)).

get_tags_empty_when_none_test() ->
    Args = [],
    Config = [],
    ?assertEqual([], ocibuild_rebar3:get_tags(Args, Config)).

%%%===================================================================
%%% Helper functions
%%%===================================================================

make_temp_lock_file(Prefix, Content) ->
    TmpDir = make_temp_dir("lock_test"),
    LockFile = filename:join(TmpDir, Prefix ++ ".lock"),
    ok = file:write_file(LockFile, Content),
    LockFile.
