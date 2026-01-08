%%%-------------------------------------------------------------------
-module(ocibuild_release_tests).
-moduledoc "Tests for the shared release handling API (ocibuild_release)".

-include_lib("eunit/include/eunit.hrl").

-import(ocibuild_test_helpers, [make_temp_dir/1, cleanup_temp_dir/1]).

%% Fake adapter callbacks for testing tag_additional/6
-export([info/2, console/2, error/2]).

%%%===================================================================
%%% Test fixtures - setup/cleanup functions
%%%===================================================================

%% Setup for tests needing a mock release directory
setup_mock_release() ->
    create_mock_release().

cleanup_mock_release(TmpDir) ->
    cleanup_temp_dir(TmpDir).

%% Setup for tests needing a simple temp directory
setup_temp_dir() ->
    make_temp_dir("ocibuild_test").

cleanup_temp_dir_wrapper(TmpDir) ->
    cleanup_temp_dir(TmpDir).

%% Setup for push auth tests
setup_push_auth() ->
    os:unsetenv("OCIBUILD_PUSH_TOKEN"),
    os:unsetenv("OCIBUILD_PUSH_USERNAME"),
    os:unsetenv("OCIBUILD_PUSH_PASSWORD"),
    ok.

cleanup_push_auth(_) ->
    os:unsetenv("OCIBUILD_PUSH_TOKEN"),
    os:unsetenv("OCIBUILD_PUSH_USERNAME"),
    os:unsetenv("OCIBUILD_PUSH_PASSWORD").

%% Setup for pull auth tests
setup_pull_auth() ->
    os:unsetenv("OCIBUILD_PULL_TOKEN"),
    os:unsetenv("OCIBUILD_PULL_USERNAME"),
    os:unsetenv("OCIBUILD_PULL_PASSWORD"),
    ok.

cleanup_pull_auth(_) ->
    os:unsetenv("OCIBUILD_PULL_TOKEN"),
    os:unsetenv("OCIBUILD_PULL_USERNAME"),
    os:unsetenv("OCIBUILD_PULL_PASSWORD").

%%%===================================================================
%%% Test generators
%%%===================================================================

file_collection_test_() ->
    {foreach, fun setup_mock_release/0, fun cleanup_mock_release/1, [
        fun(TmpDir) ->
            {"collect release files", fun() -> collect_release_files_test(TmpDir) end}
        end
    ]}.

empty_dir_collection_test_() ->
    {foreach, fun setup_temp_dir/0, fun cleanup_temp_dir_wrapper/1, [
        fun(TmpDir) ->
            {"collect empty dir", fun() -> collect_empty_dir_test(TmpDir) end}
        end
    ]}.

file_mode_test_() ->
    {foreach, fun setup_temp_dir/0, fun cleanup_temp_dir_wrapper/1, [
        fun(TmpDir) ->
            {"get file mode", fun() -> get_file_mode_test(TmpDir) end}
        end
    ]}.

symlink_security_test_() ->
    {foreach, fun setup_temp_dir/0, fun cleanup_temp_dir_wrapper/1, [
        fun(TmpDir) ->
            {"symlink inside release", fun() -> collect_symlink_inside_release_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"symlink outside release", fun() -> collect_symlink_outside_release_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"symlink relative escape", fun() -> collect_symlink_relative_escape_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"broken symlink", fun() -> collect_broken_symlink_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"symlink to dir inside", fun() -> collect_symlink_to_dir_inside_test(TmpDir) end}
        end
    ]}.

multiplatform_validation_test_() ->
    {foreach, fun setup_mock_release/0, fun cleanup_mock_release/1, [
        fun(TmpDir) ->
            {"has bundled ERTS true", fun() -> has_bundled_erts_true_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"has bundled ERTS false", fun() -> has_bundled_erts_false_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"native code found .so", fun() -> check_for_native_code_found_so_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"native code found .dll", fun() -> check_for_native_code_found_dll_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"native code found .dylib", fun() ->
                check_for_native_code_found_dylib_test(TmpDir)
            end}
        end,
        fun(TmpDir) ->
            {"native code none", fun() -> check_for_native_code_none_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"native code nested priv", fun() -> check_for_native_code_nested_priv_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"validate multiplatform ERTS error", fun() ->
                validate_multiplatform_erts_error_test(TmpDir)
            end}
        end,
        fun(TmpDir) ->
            {"validate multiplatform ok", fun() -> validate_multiplatform_ok_test(TmpDir) end}
        end
    ]}.

cli_integration_test_() ->
    {foreach, fun setup_mock_release/0, fun cleanup_mock_release/1, [
        fun(TmpDir) ->
            {"run single platform", fun() -> run_single_platform_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"run multiplatform ERTS error", fun() -> run_multiplatform_erts_error_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"run multiplatform no ERTS", fun() -> run_multiplatform_no_erts_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"run no platform", fun() -> run_no_platform_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"run empty platform", fun() -> run_empty_platform_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"run nil platform", fun() -> run_nil_platform_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"run multiplatform NIF warning", fun() ->
                run_multiplatform_nif_warning_test(TmpDir)
            end}
        end
    ]}.

layer_building_test_() ->
    {foreach, fun setup_temp_dir/0, fun cleanup_temp_dir_wrapper/1, [
        fun(TmpDir) ->
            {"build layers with deps and ERTS", fun() ->
                build_layers_with_deps_and_erts_test(TmpDir)
            end}
        end,
        fun(TmpDir) ->
            {"build layers with deps no ERTS", fun() ->
                build_layers_with_deps_no_erts_test(TmpDir)
            end}
        end,
        fun(TmpDir) ->
            {"build layers fallback no deps", fun() ->
                build_layers_fallback_no_deps_test(TmpDir)
            end}
        end,
        fun(TmpDir) ->
            {"build layers fallback no app name", fun() ->
                build_layers_fallback_no_app_name_test(TmpDir)
            end}
        end
    ]}.

push_auth_test_() ->
    {foreach, fun setup_push_auth/0, fun cleanup_push_auth/1, [
        {"push auth empty", fun get_push_auth_empty_test/0},
        {"push auth token", fun get_push_auth_token_test/0},
        {"push auth username password", fun get_push_auth_username_password_test/0},
        {"push auth username only", fun get_push_auth_username_only_test/0},
        {"push auth password only", fun get_push_auth_password_only_test/0}
    ]}.

pull_auth_test_() ->
    {foreach, fun setup_pull_auth/0, fun cleanup_pull_auth/1, [
        {"pull auth empty", fun get_pull_auth_empty_test/0},
        {"pull auth username password", fun get_pull_auth_username_password_test/0}
    ]}.

%%%===================================================================
%%% File collection tests
%%%===================================================================

collect_release_files_test(TmpDir) ->
    {ok, Files} = ocibuild_release:collect_release_files(TmpDir),

    %% Should have collected the files we created
    ?assert(length(Files) >= 3),

    %% Check that bin/myapp has executable permissions
    BinFile = lists:keyfind(~"/app/bin/myapp", 1, Files),
    ?assertNotEqual(false, BinFile),
    {_, _, BinMode} = BinFile,
    ?assertEqual(8#755, BinMode band 8#777),

    %% Check that lib file has regular permissions
    LibFile = lists:keyfind(~"/app/lib/myapp-1.0.0/ebin/myapp.beam", 1, Files),
    ?assertNotEqual(false, LibFile),
    {_, _, LibMode} = LibFile,
    ?assertEqual(8#644, LibMode band 8#777).

collect_empty_dir_test(TmpDir) ->
    {ok, Files} = ocibuild_release:collect_release_files(TmpDir),
    ?assertEqual([], Files).

%%%===================================================================
%%% Build image tests (using exported function)
%%%===================================================================

build_scratch_image_test() ->
    Files =
        [
            {~"/app/bin/myapp", ~"#!/bin/sh\necho hello", 8#755},
            {~"/app/lib/myapp.beam", ~"beam_data", 8#644}
        ],

    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => "myapp",
        workdir => ~"/app"
    }),

    %% Verify image structure (1 app layer + 1 SBOM layer = 2)
    ?assert(is_map(Image)),
    ?assertEqual(2, length(maps:get(layers, Image))),

    %% Verify config
    Config = maps:get(config, Image),
    InnerConfig = maps:get(~"config", Config),
    ?assertEqual(
        [~"/app/bin/myapp", ~"foreground"],
        maps:get(~"Entrypoint", InnerConfig)
    ),
    ?assertEqual(~"/app", maps:get(~"WorkingDir", InnerConfig)).

build_with_env_test() ->
    Files = [{~"/app/test", ~"data", 8#644}],
    EnvMap = #{~"LANG" => ~"C.UTF-8", ~"PORT" => ~"8080"},

    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => "myapp",
        workdir => ~"/app",
        env => EnvMap
    }),

    Config = maps:get(config, Image),
    InnerConfig = maps:get(~"config", Config),
    EnvList = maps:get(~"Env", InnerConfig),

    %% Should contain both env vars
    ?assert(lists:any(fun(E) -> binary:match(E, ~"LANG=") =/= nomatch end, EnvList)),
    ?assert(lists:any(fun(E) -> binary:match(E, ~"PORT=") =/= nomatch end, EnvList)).

build_with_exposed_ports_test() ->
    Files = [{~"/app/test", ~"data", 8#644}],

    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => "myapp",
        workdir => ~"/app",
        expose => [8080, 443]
    }),

    Config = maps:get(config, Image),
    InnerConfig = maps:get(~"config", Config),
    ExposedPorts = maps:get(~"ExposedPorts", InnerConfig),

    ?assert(maps:is_key(~"8080/tcp", ExposedPorts)),
    ?assert(maps:is_key(~"443/tcp", ExposedPorts)).

build_with_labels_test() ->
    Files = [{~"/app/test", ~"data", 8#644}],
    Labels = #{~"org.opencontainers.image.version" => ~"1.0.0"},

    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => "myapp",
        workdir => ~"/app",
        labels => Labels
    }),

    Config = maps:get(config, Image),
    InnerConfig = maps:get(~"config", Config),
    ImageLabels = maps:get(~"Labels", InnerConfig),

    ?assertEqual(~"1.0.0", maps:get(~"org.opencontainers.image.version", ImageLabels)).

%%%===================================================================
%%% Auth tests
%%%===================================================================

get_push_auth_empty_test() ->
    ?assertEqual(#{}, ocibuild_release:get_push_auth()).

get_push_auth_token_test() ->
    os:putenv("OCIBUILD_PUSH_TOKEN", "mytoken123"),
    Auth = ocibuild_release:get_push_auth(),
    ?assertEqual(#{token => ~"mytoken123"}, Auth).

get_push_auth_username_password_test() ->
    %% Ensure token is not set (takes precedence over username/password)
    os:unsetenv("OCIBUILD_PUSH_TOKEN"),
    os:putenv("OCIBUILD_PUSH_USERNAME", "myuser"),
    os:putenv("OCIBUILD_PUSH_PASSWORD", "mypass"),
    Auth = ocibuild_release:get_push_auth(),
    ?assertEqual(#{username => ~"myuser", password => ~"mypass"}, Auth).

get_push_auth_username_only_test() ->
    %% Ensure token and password are not set
    os:unsetenv("OCIBUILD_PUSH_TOKEN"),
    os:unsetenv("OCIBUILD_PUSH_PASSWORD"),
    os:putenv("OCIBUILD_PUSH_USERNAME", "myuser"),
    %% Missing password should return empty
    ?assertEqual(#{}, ocibuild_release:get_push_auth()).

get_push_auth_password_only_test() ->
    %% Ensure token and username are not set
    os:unsetenv("OCIBUILD_PUSH_TOKEN"),
    os:unsetenv("OCIBUILD_PUSH_USERNAME"),
    os:putenv("OCIBUILD_PUSH_PASSWORD", "mypass"),
    %% Missing username should return empty
    ?assertEqual(#{}, ocibuild_release:get_push_auth()).

get_pull_auth_empty_test() ->
    ?assertEqual(#{}, ocibuild_release:get_pull_auth()).

get_pull_auth_username_password_test() ->
    os:putenv("OCIBUILD_PULL_USERNAME", "pulluser"),
    os:putenv("OCIBUILD_PULL_PASSWORD", "pullpass"),
    Auth = ocibuild_release:get_pull_auth(),
    ?assertEqual(#{username => ~"pulluser", password => ~"pullpass"}, Auth).

%%%===================================================================
%%% Format bytes tests
%%%===================================================================

format_bytes_test() ->
    ?assertEqual("100 B", lists:flatten(ocibuild_release:format_bytes(100))),
    ?assertEqual("0 B", lists:flatten(ocibuild_release:format_bytes(0))),
    ?assertEqual("1.0 KB", lists:flatten(ocibuild_release:format_bytes(1024))),
    ?assertEqual("1.0 MB", lists:flatten(ocibuild_release:format_bytes(1024 * 1024))),
    ?assertEqual("1.00 GB", lists:flatten(ocibuild_release:format_bytes(1024 * 1024 * 1024))).

%%%===================================================================
%%% Format progress tests
%%%===================================================================

format_progress_unknown_total_test() ->
    Result = lists:flatten(ocibuild_release:format_progress(1024, unknown)),
    ?assert(string:find(Result, "1.0 KB") =/= nomatch).

format_progress_with_percent_test() ->
    Result = lists:flatten(ocibuild_release:format_progress(512, 1024)),
    ?assert(string:find(Result, "50%") =/= nomatch).

format_progress_complete_test() ->
    Result = lists:flatten(ocibuild_release:format_progress(1024, 1024)),
    ?assert(string:find(Result, "100%") =/= nomatch).

format_progress_zero_total_test() ->
    %% Should handle zero/invalid total gracefully
    Result = lists:flatten(ocibuild_release:format_progress(100, 0)),
    ?assert(is_list(Result)).

format_progress_negative_total_test() ->
    %% Negative total should be handled
    Result = lists:flatten(ocibuild_release:format_progress(100, -1)),
    ?assert(is_list(Result)).

format_progress_large_values_test() ->
    %% Large values (gigabytes)
    Result = lists:flatten(
        ocibuild_release:format_progress(1024 * 1024 * 1024, 2 * 1024 * 1024 * 1024)
    ),
    ?assert(string:find(Result, "50%") =/= nomatch).

format_progress_overflow_test() ->
    %% More received than total should cap at 100%
    Result = lists:flatten(ocibuild_release:format_progress(2000, 1000)),
    ?assert(string:find(Result, "100%") =/= nomatch).

%%%===================================================================
%%% to_binary tests
%%%===================================================================

to_binary_binary_test() ->
    ?assertEqual(~"test", ocibuild_release:to_binary(~"test")).

to_binary_list_test() ->
    ?assertEqual(~"hello", ocibuild_release:to_binary("hello")).

to_binary_atom_test() ->
    ?assertEqual(~"myatom", ocibuild_release:to_binary(myatom)).

%%%===================================================================
%%% parse_tag tests
%%%===================================================================

parse_tag_exported_simple_test() ->
    ?assertEqual({~"myapp", ~"1.0.0"}, ocibuild_release:parse_tag(~"myapp:1.0.0")).

parse_tag_exported_no_version_test() ->
    ?assertEqual({~"myapp", ~"latest"}, ocibuild_release:parse_tag(~"myapp")).

%%%===================================================================
%%% build_image with custom cmd tests
%%%===================================================================

build_image_with_custom_cmd_test() ->
    Files =
        [
            {~"/app/bin/myapp", ~"#!/bin/sh\necho hello", 8#755}
        ],

    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => "myapp",
        workdir => ~"/app",
        cmd => ~"start"
    }),

    Config = maps:get(config, Image),
    InnerConfig = maps:get(~"config", Config),
    %% Should use custom cmd "start" instead of default "foreground"
    ?assertEqual(
        [~"/app/bin/myapp", ~"start"],
        maps:get(~"Entrypoint", InnerConfig)
    ).

%%%===================================================================
%%% chunk_size validation tests
%%%===================================================================

%% Test chunk_size adapter constants
chunk_size_constants_test() ->
    %% Verify the adapter exposes the expected chunk size constants
    ?assertEqual(1, ocibuild_adapter:min_chunk_size_mb()),
    ?assertEqual(100, ocibuild_adapter:max_chunk_size_mb()),
    ?assertEqual(5, ocibuild_adapter:default_chunk_size_mb()),
    %% Verify byte conversions
    ?assertEqual(1 * 1024 * 1024, ocibuild_adapter:min_chunk_size()),
    ?assertEqual(100 * 1024 * 1024, ocibuild_adapter:max_chunk_size()),
    ?assertEqual(5 * 1024 * 1024, ocibuild_adapter:default_chunk_size()).

%% Test that valid chunk_size values are accepted (integration test via build_image)
chunk_size_valid_in_config_test() ->
    Files = [{~"/app/bin/app", ~"#!/bin/sh\necho hello", 8#755}],
    %% 10MB is within valid range (1-100)
    {ok, _Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => ~"app",
        chunk_size => 10 * 1024 * 1024
    }),
    ok.

%% Test that undefined chunk_size uses default
chunk_size_undefined_uses_default_test() ->
    Files = [{~"/app/bin/app", ~"#!/bin/sh\necho hello", 8#755}],
    {ok, _Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => ~"app",
        chunk_size => undefined
    }),
    ok.

%%%===================================================================
%%% to_container_path tests
%%%===================================================================

to_container_path_simple_test() ->
    ?assertEqual(~"/app/bin/myapp", ocibuild_release:to_container_path("bin/myapp")).

to_container_path_nested_test() ->
    ?assertEqual(
        ~"/app/lib/myapp/ebin/myapp.beam",
        ocibuild_release:to_container_path("lib/myapp/ebin/myapp.beam")
    ).

%%%===================================================================
%%% get_file_mode tests
%%%===================================================================

get_file_mode_test(TmpDir) ->
    %% Create a file with specific permissions
    FilePath = filename:join(TmpDir, "test.txt"),
    ok = file:write_file(FilePath, ~"test"),
    ok = file:change_mode(FilePath, 8#644),
    ?assertEqual(8#644, ocibuild_release:get_file_mode(FilePath)),

    %% Create executable
    ExePath = filename:join(TmpDir, "test.sh"),
    ok = file:write_file(ExePath, ~"#!/bin/sh"),
    ok = file:change_mode(ExePath, 8#755),
    ?assertEqual(8#755, ocibuild_release:get_file_mode(ExePath)).

get_file_mode_nonexistent_test() ->
    %% Non-existent file should return default mode
    ?assertEqual(8#644, ocibuild_release:get_file_mode("/nonexistent/path")).

%%%===================================================================
%%% strip_prefix tests
%%%===================================================================

strip_prefix_simple_test() ->
    ?assertEqual(
        "file.txt", ocibuild_release:strip_prefix(["foo", "bar"], ["foo", "bar", "file.txt"])
    ).

strip_prefix_nested_test() ->
    ?assertEqual(
        filename:join(["a", "b", "c.txt"]),
        ocibuild_release:strip_prefix(["base"], ["base", "a", "b", "c.txt"])
    ).

strip_prefix_no_match_test() ->
    %% If no match, return full path
    ?assertEqual(
        filename:join(["other", "path"]),
        ocibuild_release:strip_prefix(["foo"], ["other", "path"])
    ).

strip_prefix_partial_match_test() ->
    %% When paths diverge, return remaining from full path
    ?assertEqual(
        filename:join(["x", "y"]),
        ocibuild_release:strip_prefix(["a", "b"], ["a", "x", "y"])
    ).

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
    Config = [{base_image, "ubuntu:22.04"}],
    ?assertEqual(~"ubuntu:22.04", ocibuild_rebar3:get_base_image(Args, Config)).

get_base_image_default_test() ->
    Args = [],
    Config = [],
    ?assertEqual(~"debian:stable-slim", ocibuild_rebar3:get_base_image(Args, Config)).

%%%===================================================================
%%% make_relative_path tests
%%%===================================================================

make_relative_path_simple_test() ->
    ?assertEqual(
        "file.txt", ocibuild_release:make_relative_path("/base/path", "/base/path/file.txt")
    ).

make_relative_path_nested_test() ->
    ?assertEqual(
        filename:join(["a", "b", "c.txt"]),
        ocibuild_release:make_relative_path("/base", "/base/a/b/c.txt")
    ).

%%%===================================================================
%%% Additional build_image tests
%%%===================================================================

build_image_all_options_test() ->
    Files =
        [
            {~"/app/bin/myapp", ~"#!/bin/sh\necho hello", 8#755}
        ],

    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => "myapp",
        workdir => ~"/app",
        env => #{~"LANG" => ~"C.UTF-8", ~"DEBUG" => ~"1"},
        expose => [8080, 443],
        labels => #{~"version" => ~"1.0.0", ~"author" => ~"test"}
    }),

    Config = maps:get(config, Image),
    InnerConfig = maps:get(~"config", Config),

    %% Check env
    EnvList = maps:get(~"Env", InnerConfig),
    ?assertEqual(2, length(EnvList)),

    %% Check ports
    ExposedPorts = maps:get(~"ExposedPorts", InnerConfig),
    ?assert(maps:is_key(~"8080/tcp", ExposedPorts)),
    ?assert(maps:is_key(~"443/tcp", ExposedPorts)),

    %% Check labels
    Labels = maps:get(~"Labels", InnerConfig),
    ?assertEqual(~"1.0.0", maps:get(~"version", Labels)),
    ?assertEqual(~"test", maps:get(~"author", Labels)).

%%%===================================================================
%%% Progress callback tests
%%%===================================================================

make_progress_callback_test() ->
    %% Get the callback
    Callback = ocibuild_release:make_progress_callback(),
    ?assert(is_function(Callback, 1)),

    %% Test calling it with manifest phase
    ok = Callback(#{phase => manifest, bytes_received => 100, total_bytes => 200}),

    %% Test calling it with config phase
    ok = Callback(#{phase => config, bytes_received => 50, total_bytes => 100}),

    %% Test calling it with layer phase
    ok = Callback(#{phase => layer, bytes_received => 1024, total_bytes => 2048}),

    %% Test with unknown total
    ok = Callback(#{phase => manifest, bytes_received => 100, total_bytes => unknown}).

%%%===================================================================
%%% Symlink security tests
%%%===================================================================

collect_symlink_inside_release_test(TmpDir) ->
    %% Create a file and a symlink pointing to it (within release)
    BinDir = filename:join(TmpDir, "bin"),
    ok = filelib:ensure_dir(filename:join(BinDir, "placeholder")),

    TargetPath = filename:join(BinDir, "real_file"),
    ok = file:write_file(TargetPath, ~"real content"),

    SymlinkPath = filename:join(BinDir, "link_to_file"),
    ok = file:make_symlink("real_file", SymlinkPath),

    %% Should successfully collect both files
    {ok, Files} = ocibuild_release:collect_release_files(TmpDir),
    ?assertEqual(2, length(Files)),

    %% Symlink should resolve to the same content
    LinkFile = lists:keyfind(~"/app/bin/link_to_file", 1, Files),
    ?assertNotEqual(false, LinkFile),
    {_, Content, _} = LinkFile,
    ?assertEqual(~"real content", Content).

collect_symlink_outside_release_test(TmpDir) ->
    %% Create a symlink pointing outside the release directory
    BinDir = filename:join(TmpDir, "bin"),
    ok = filelib:ensure_dir(filename:join(BinDir, "placeholder")),

    %% Create a regular file
    RegularPath = filename:join(BinDir, "regular"),
    ok = file:write_file(RegularPath, ~"regular content"),

    %% Create a symlink pointing to /etc/passwd (outside release)
    SymlinkPath = filename:join(BinDir, "evil_link"),
    ok = file:make_symlink("/etc/passwd", SymlinkPath),

    %% Should collect only the regular file, not the symlink
    {ok, Files} = ocibuild_release:collect_release_files(TmpDir),
    ?assertEqual(1, length(Files)),

    %% The evil symlink should not be present
    EvilFile = lists:keyfind(~"/app/bin/evil_link", 1, Files),
    ?assertEqual(false, EvilFile).

collect_symlink_relative_escape_test(TmpDir) ->
    %% Create a symlink using relative path to escape release directory
    BinDir = filename:join(TmpDir, "bin"),
    ok = filelib:ensure_dir(filename:join(BinDir, "placeholder")),

    %% Create a regular file
    RegularPath = filename:join(BinDir, "regular"),
    ok = file:write_file(RegularPath, ~"regular content"),

    %% Create a symlink using ../../.. to escape
    SymlinkPath = filename:join(BinDir, "escape_link"),
    ok = file:make_symlink("../../../etc/passwd", SymlinkPath),

    %% Should collect only the regular file
    {ok, Files} = ocibuild_release:collect_release_files(TmpDir),
    ?assertEqual(1, length(Files)),

    %% The escape symlink should not be present
    EscapeFile = lists:keyfind(~"/app/bin/escape_link", 1, Files),
    ?assertEqual(false, EscapeFile).

collect_broken_symlink_test(TmpDir) ->
    %% Create a broken symlink (target doesn't exist)
    BinDir = filename:join(TmpDir, "bin"),
    ok = filelib:ensure_dir(filename:join(BinDir, "placeholder")),

    %% Create a regular file
    RegularPath = filename:join(BinDir, "regular"),
    ok = file:write_file(RegularPath, ~"regular content"),

    %% Create a broken symlink
    SymlinkPath = filename:join(BinDir, "broken_link"),
    ok = file:make_symlink("nonexistent_file", SymlinkPath),

    %% Should collect only the regular file
    {ok, Files} = ocibuild_release:collect_release_files(TmpDir),
    ?assertEqual(1, length(Files)).

collect_symlink_to_dir_inside_test(TmpDir) ->
    %% Create a directory with a file, and a symlink to that directory
    RealDir = filename:join(TmpDir, "real_dir"),
    ok = filelib:ensure_dir(filename:join(RealDir, "placeholder")),

    FilePath = filename:join(RealDir, "file.txt"),
    ok = file:write_file(FilePath, ~"file in real dir"),

    %% Create symlink to directory (within release)
    SymlinkPath = filename:join(TmpDir, "link_dir"),
    ok = file:make_symlink("real_dir", SymlinkPath),

    %% Should collect files from both the real dir and via the symlink
    {ok, Files} = ocibuild_release:collect_release_files(TmpDir),

    %% Should have 2 files (same file accessible via two paths)
    ?assertEqual(2, length(Files)),

    %% Both paths should have the same content
    RealFile = lists:keyfind(~"/app/real_dir/file.txt", 1, Files),
    LinkFile = lists:keyfind(~"/app/link_dir/file.txt", 1, Files),
    ?assertNotEqual(false, RealFile),
    ?assertNotEqual(false, LinkFile).

%%%===================================================================
%%% Multi-platform validation tests
%%%===================================================================

has_bundled_erts_true_test(TmpDir) ->
    %% Add erts directory
    ErtsDir = filename:join(TmpDir, "erts-27.0"),
    ok = filelib:ensure_dir(filename:join(ErtsDir, "bin/placeholder")),
    ok = file:write_file(filename:join([ErtsDir, "bin", "beam.smp"]), ~"beam"),

    ?assertEqual(true, ocibuild_release:has_bundled_erts(TmpDir)).

has_bundled_erts_false_test(TmpDir) ->
    ?assertEqual(false, ocibuild_release:has_bundled_erts(TmpDir)).

has_bundled_erts_nonexistent_test() ->
    ?assertEqual(false, ocibuild_release:has_bundled_erts("/nonexistent/path")).

check_for_native_code_found_so_test(TmpDir) ->
    %% Add a .so file in lib/crypto-1.0.0/priv/
    PrivDir = filename:join([TmpDir, "lib", "crypto-1.0.0", "priv"]),
    ok = filelib:ensure_dir(filename:join(PrivDir, "placeholder")),
    ok = file:write_file(filename:join(PrivDir, "crypto_nif.so"), ~"fake so"),

    {warning, NifFiles} = ocibuild_release:check_for_native_code(TmpDir),
    ?assertEqual(1, length(NifFiles)),
    [#{app := App, file := File, extension := Ext}] = NifFiles,
    ?assertEqual(~"crypto", App),
    ?assertEqual(~"crypto_nif.so", File),
    ?assertEqual(~".so", Ext).

check_for_native_code_found_dll_test(TmpDir) ->
    %% Add a .dll file
    PrivDir = filename:join([TmpDir, "lib", "nif_app-2.0.0", "priv"]),
    ok = filelib:ensure_dir(filename:join(PrivDir, "placeholder")),
    ok = file:write_file(filename:join(PrivDir, "nif_app.dll"), ~"fake dll"),

    {warning, NifFiles} = ocibuild_release:check_for_native_code(TmpDir),
    ?assertEqual(1, length(NifFiles)),
    [#{extension := Ext}] = NifFiles,
    ?assertEqual(~".dll", Ext).

check_for_native_code_found_dylib_test(TmpDir) ->
    %% Add a .dylib file
    PrivDir = filename:join([TmpDir, "lib", "mac_nif-1.0.0", "priv"]),
    ok = filelib:ensure_dir(filename:join(PrivDir, "placeholder")),
    ok = file:write_file(filename:join(PrivDir, "mac_nif.dylib"), ~"fake dylib"),

    {warning, NifFiles} = ocibuild_release:check_for_native_code(TmpDir),
    ?assertEqual(1, length(NifFiles)),
    [#{extension := Ext}] = NifFiles,
    ?assertEqual(~".dylib", Ext).

check_for_native_code_none_test(TmpDir) ->
    ?assertEqual({ok, []}, ocibuild_release:check_for_native_code(TmpDir)).

check_for_native_code_nested_priv_test(TmpDir) ->
    %% Add a .so file in a subdirectory of priv
    NestedDir = filename:join([TmpDir, "lib", "nested-1.0.0", "priv", "native"]),
    ok = filelib:ensure_dir(filename:join(NestedDir, "placeholder")),
    ok = file:write_file(filename:join(NestedDir, "nested_nif.so"), ~"fake so"),

    {warning, NifFiles} = ocibuild_release:check_for_native_code(TmpDir),
    ?assertEqual(1, length(NifFiles)),
    [#{file := File}] = NifFiles,
    %% Should include the path relative to priv
    ?assert(binary:match(File, ~"native") =/= nomatch).

validate_multiplatform_single_platform_ok_test() ->
    %% Single platform builds don't need validation
    Platform = #{os => ~"linux", architecture => ~"amd64"},
    ?assertEqual(ok, ocibuild_release:validate_multiplatform("/any/path", [Platform])).

validate_multiplatform_empty_platforms_ok_test() ->
    %% Empty platforms list is OK
    ?assertEqual(ok, ocibuild_release:validate_multiplatform("/any/path", [])).

validate_multiplatform_erts_error_test(TmpDir) ->
    %% Add erts directory
    ErtsDir = filename:join(TmpDir, "erts-27.0"),
    ok = filelib:ensure_dir(filename:join(ErtsDir, "bin/placeholder")),

    Platforms = [
        #{os => ~"linux", architecture => ~"amd64"},
        #{os => ~"linux", architecture => ~"arm64"}
    ],
    Result = ocibuild_release:validate_multiplatform(TmpDir, Platforms),
    ?assertMatch({error, {bundled_erts, _}}, Result).

validate_multiplatform_ok_test(TmpDir) ->
    %% No ERTS, no NIFs - should be OK
    Platforms = [
        #{os => ~"linux", architecture => ~"amd64"},
        #{os => ~"linux", architecture => ~"arm64"}
    ],
    ?assertEqual(ok, ocibuild_release:validate_multiplatform(TmpDir, Platforms)).

%%%===================================================================
%%% CLI Integration tests (run/3 with platforms)
%%%===================================================================

run_single_platform_test(TmpDir) ->
    State = create_mock_adapter_state(TmpDir, #{
        platform => ~"linux/amd64"
    }),
    %% Single platform should work even with ERTS
    add_mock_erts(TmpDir),

    %% This should succeed (single platform allows ERTS)
    Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
    ?assertMatch({ok, _}, Result).

run_multiplatform_erts_error_test(TmpDir) ->
    State = create_mock_adapter_state(TmpDir, #{
        platform => ~"linux/amd64,linux/arm64"
    }),
    %% Add ERTS to trigger validation error
    add_mock_erts(TmpDir),

    Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
    ?assertMatch({error, {bundled_erts, _}}, Result).

run_multiplatform_no_erts_test(TmpDir) ->
    Platforms = [
        #{os => ~"linux", architecture => ~"amd64"},
        #{os => ~"linux", architecture => ~"arm64"}
    ],
    %% No ERTS, validation should succeed
    ?assertEqual(ok, ocibuild_release:validate_multiplatform(TmpDir, Platforms)).

run_no_platform_test(TmpDir) ->
    State = create_mock_adapter_state(TmpDir, #{
        platform => undefined
    }),
    Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
    ?assertMatch({ok, _}, Result).

run_empty_platform_test(TmpDir) ->
    State = create_mock_adapter_state(TmpDir, #{
        platform => <<>>
    }),
    Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
    ?assertMatch({ok, _}, Result).

run_nil_platform_test(TmpDir) ->
    State = create_mock_adapter_state(TmpDir, #{
        platform => nil
    }),
    Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
    ?assertMatch({ok, _}, Result).

run_multiplatform_nif_warning_test(TmpDir) ->
    %% Add NIF to the release
    add_mock_nif(TmpDir),

    Platforms = [
        #{os => ~"linux", architecture => ~"amd64"},
        #{os => ~"linux", architecture => ~"arm64"}
    ],

    %% NIFs should be detected by check_for_native_code
    {warning, NifFiles} = ocibuild_release:check_for_native_code(TmpDir),
    ?assertEqual(1, length(NifFiles)),

    %% Verify the detected NIF file details
    [NifInfo] = NifFiles,
    ?assertEqual(~"crypto", maps:get(app, NifInfo)),
    ?assertEqual(~"test_nif.so", maps:get(file, NifInfo)),

    %% Validation should succeed - NIFs warn but don't block
    ?assertEqual(ok, ocibuild_release:validate_multiplatform(TmpDir, Platforms)).

%%%===================================================================
%%% Smart Layer Partitioning tests
%%%===================================================================

%% Layer classification tests
%% New signature: classify_file_layer(Path, DepNames, AppName, Workdir, HasErts)

classify_erts_directory_test() ->
    DepNames = sets:from_list([~"cowboy"]),
    %% erts-* directory always goes to erts layer
    ?assertEqual(
        erts,
        ocibuild_release:classify_file_layer(
            ~"/app/erts-14.2.1/bin/erl", DepNames, ~"myapp", ~"/app", true
        )
    ).

classify_otp_lib_with_erts_test() ->
    DepNames = sets:from_list([~"cowboy"]),
    %% OTP libs (not in lock file, not app) -> erts layer when ERTS bundled
    ?assertEqual(
        erts,
        ocibuild_release:classify_file_layer(
            ~"/app/lib/stdlib-5.0/ebin/lists.beam", DepNames, ~"myapp", ~"/app", true
        )
    ),
    ?assertEqual(
        erts,
        ocibuild_release:classify_file_layer(
            ~"/app/lib/kernel-9.0/ebin/kernel.app", DepNames, ~"myapp", ~"/app", true
        )
    ).

classify_otp_lib_without_erts_test() ->
    DepNames = sets:from_list([~"cowboy"]),
    %% OTP libs -> dep layer when ERTS NOT bundled (stable like deps)
    ?assertEqual(
        dep,
        ocibuild_release:classify_file_layer(
            ~"/app/lib/stdlib-5.0/ebin/lists.beam", DepNames, ~"myapp", ~"/app", false
        )
    ),
    ?assertEqual(
        dep,
        ocibuild_release:classify_file_layer(
            ~"/app/lib/kernel-9.0/ebin/kernel.app", DepNames, ~"myapp", ~"/app", false
        )
    ).

classify_dependency_lib_test() ->
    DepNames = sets:from_list([~"cowboy", ~"cowlib"]),
    %% Deps from lock file -> dep layer
    ?assertEqual(
        dep,
        ocibuild_release:classify_file_layer(
            ~"/app/lib/cowboy-2.10.0/ebin/cowboy.app", DepNames, ~"myapp", ~"/app", true
        )
    ),
    ?assertEqual(
        dep,
        ocibuild_release:classify_file_layer(
            ~"/app/lib/cowlib-2.12.1/src/cow_http.erl", DepNames, ~"myapp", ~"/app", true
        )
    ).

classify_app_lib_test() ->
    DepNames = sets:from_list([~"cowboy"]),
    %% App name matches -> app layer
    ?assertEqual(
        app,
        ocibuild_release:classify_file_layer(
            ~"/app/lib/myapp-1.0.0/ebin/myapp.app", DepNames, ~"myapp", ~"/app", true
        )
    ).

classify_hyphenated_app_name_test() ->
    %% App names with hyphens (e.g., "my-cool-app-1.0.0" should extract as "my-cool-app")
    DepNames = sets:from_list([~"cowboy"]),
    ?assertEqual(
        app,
        ocibuild_release:classify_file_layer(
            ~"/app/lib/my-cool-app-1.0.0/ebin/my_cool_app.app",
            DepNames,
            ~"my-cool-app",
            ~"/app",
            true
        )
    ),
    %% Hyphenated dep should also work
    HyphenatedDeps = sets:from_list([~"plug-crypto"]),
    ?assertEqual(
        dep,
        ocibuild_release:classify_file_layer(
            ~"/app/lib/plug-crypto-2.0.0/ebin/plug_crypto.app",
            HyphenatedDeps,
            ~"myapp",
            ~"/app",
            true
        )
    ).

classify_bin_directory_test() ->
    DepNames = sets:from_list([~"cowboy"]),
    %% bin/ directory -> app layer
    ?assertEqual(
        app,
        ocibuild_release:classify_file_layer(
            ~"/app/bin/myapp", DepNames, ~"myapp", ~"/app", true
        )
    ).

classify_releases_directory_test() ->
    DepNames = sets:from_list([~"cowboy"]),
    %% releases/ directory -> app layer
    ?assertEqual(
        app,
        ocibuild_release:classify_file_layer(
            ~"/app/releases/1.0.0/myapp.rel", DepNames, ~"myapp", ~"/app", true
        )
    ).

%% Partition tests

partition_files_with_erts_test() ->
    Files = [
        {~"/app/erts-14.2.1/bin/erl", ~"erl", 8#755},
        {~"/app/lib/stdlib-5.0/ebin/lists.beam", ~"beam", 8#644},
        {~"/app/lib/cowboy-2.10.0/ebin/cowboy.app", ~"app", 8#644},
        {~"/app/lib/myapp-1.0.0/ebin/myapp.beam", ~"beam", 8#644},
        {~"/app/bin/myapp", ~"script", 8#755}
    ],
    Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],

    {ErtsFiles, DepFiles, AppFiles} =
        ocibuild_release:partition_files_by_layer(Files, Deps, ~"myapp", ~"/app", true),

    %% ERTS layer: erts dir + stdlib (OTP lib)
    ?assertEqual(2, length(ErtsFiles)),
    %% Deps layer: cowboy
    ?assertEqual(1, length(DepFiles)),
    %% App layer: myapp lib + bin
    ?assertEqual(2, length(AppFiles)).

partition_files_without_erts_test() ->
    %% When ERTS not bundled, OTP libs go to deps layer
    Files = [
        {~"/app/lib/stdlib-5.0/ebin/lists.beam", ~"beam", 8#644},
        {~"/app/lib/cowboy-2.10.0/ebin/cowboy.app", ~"app", 8#644},
        {~"/app/lib/myapp-1.0.0/ebin/myapp.beam", ~"beam", 8#644},
        {~"/app/bin/myapp", ~"script", 8#755}
    ],
    Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],

    {ErtsFiles, DepFiles, AppFiles} =
        ocibuild_release:partition_files_by_layer(Files, Deps, ~"myapp", ~"/app", false),

    %% ERTS layer: empty (no ERTS bundled)
    ?assertEqual(0, length(ErtsFiles)),
    %% Deps layer: cowboy + stdlib (OTP lib treated like dep)
    ?assertEqual(2, length(DepFiles)),
    %% App layer: myapp lib + bin
    ?assertEqual(2, length(AppFiles)).

partition_files_no_deps_with_erts_test() ->
    Files = [
        {~"/app/lib/stdlib-5.0/ebin/lists.beam", ~"beam", 8#644},
        {~"/app/lib/myapp-1.0.0/ebin/myapp.beam", ~"beam", 8#644},
        {~"/app/bin/myapp", ~"script", 8#755}
    ],

    {ErtsFiles, DepFiles, AppFiles} =
        ocibuild_release:partition_files_by_layer(Files, [], ~"myapp", ~"/app", true),

    %% stdlib goes to ERTS layer (OTP lib with ERTS bundled)
    ?assertEqual(1, length(ErtsFiles)),
    ?assertEqual(0, length(DepFiles)),
    ?assertEqual(2, length(AppFiles)).

partition_files_no_deps_without_erts_test() ->
    Files = [
        {~"/app/lib/stdlib-5.0/ebin/lists.beam", ~"beam", 8#644},
        {~"/app/lib/myapp-1.0.0/ebin/myapp.beam", ~"beam", 8#644},
        {~"/app/bin/myapp", ~"script", 8#755}
    ],

    {ErtsFiles, DepFiles, AppFiles} =
        ocibuild_release:partition_files_by_layer(Files, [], ~"myapp", ~"/app", false),

    %% stdlib goes to deps layer (OTP lib without ERTS)
    ?assertEqual(0, length(ErtsFiles)),
    ?assertEqual(1, length(DepFiles)),
    ?assertEqual(2, length(AppFiles)).

%% Layer building tests

build_layers_with_deps_and_erts_test(TmpDir) ->
    %% Create erts directory to simulate bundled ERTS
    ok = file:make_dir(filename:join(TmpDir, "erts-14.2.1")),

    Files = [
        {~"/app/erts-14.2.1/bin/erl", ~"erl", 8#755},
        {~"/app/lib/stdlib-5.0/ebin/lists.beam", ~"beam", 8#644},
        {~"/app/lib/cowboy-2.10.0/ebin/cowboy.app", ~"app", 8#644},
        {~"/app/lib/myapp-1.0.0/ebin/myapp.beam", ~"beam", 8#644}
    ],
    Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],

    {ok, Image0} = ocibuild:scratch(),
    Opts = #{release_name => ~"myapp", app_name => ~"myapp", workdir => ~"/app"},
    Image1 = ocibuild_release:build_release_layers(Image0, Files, TmpDir, Deps, Opts),
    Layers = maps:get(layers, Image1),
    %% Should have 3 layers: ERTS, deps, app
    ?assertEqual(3, length(Layers)).

build_layers_with_deps_no_erts_test(TmpDir) ->
    Files = [
        {~"/app/lib/cowboy-2.10.0/ebin/cowboy.app", ~"app", 8#644},
        {~"/app/lib/myapp-1.0.0/ebin/myapp.beam", ~"beam", 8#644}
    ],
    Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],

    {ok, Image0} = ocibuild:scratch(),
    Opts = #{release_name => ~"myapp", app_name => ~"myapp", workdir => ~"/app"},
    Image1 = ocibuild_release:build_release_layers(Image0, Files, TmpDir, Deps, Opts),
    Layers = maps:get(layers, Image1),
    %% Should have 2 layers: deps, app
    ?assertEqual(2, length(Layers)).

build_layers_fallback_no_deps_test(TmpDir) ->
    Files = [
        {~"/app/lib/myapp-1.0.0/ebin/myapp.beam", ~"beam", 8#644},
        {~"/app/bin/myapp", ~"script", 8#755}
    ],

    {ok, Image0} = ocibuild:scratch(),
    Opts = #{release_name => ~"myapp", app_name => ~"myapp", workdir => ~"/app"},
    %% Empty deps should fall back to single layer
    Image1 = ocibuild_release:build_release_layers(Image0, Files, TmpDir, [], Opts),
    Layers = maps:get(layers, Image1),
    ?assertEqual(1, length(Layers)).

build_layers_fallback_no_app_name_test(TmpDir) ->
    %% When app_name is missing, should fall back to single layer
    %% even if deps are present (can't do smart layering without knowing the app)
    Files = [
        {~"/app/lib/cowboy-2.10.0/ebin/cowboy.app", ~"app", 8#644},
        {~"/app/lib/myapp-1.0.0/ebin/myapp.beam", ~"beam", 8#644}
    ],
    Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],

    {ok, Image0} = ocibuild:scratch(),
    %% No app_name in opts - should fall back to single layer
    Opts = #{release_name => ~"myapp", workdir => ~"/app"},
    Image1 = ocibuild_release:build_release_layers(Image0, Files, TmpDir, Deps, Opts),
    Layers = maps:get(layers, Image1),
    ?assertEqual(1, length(Layers)).

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% Helper to create mock adapter state
create_mock_adapter_state(ReleasePath, Overrides) ->
    OutputPath = filename:join(ReleasePath, "test-image.tar.gz"),
    BaseState = #{
        release_path => ReleasePath,
        release_name => myapp,
        base_image => ~"scratch",
        workdir => ~"/app",
        env => #{},
        expose => [],
        labels => #{},
        cmd => ~"start",
        description => undefined,
        tags => [~"myapp:1.0.0"],
        output => list_to_binary(OutputPath),
        push => undefined,
        chunk_size => undefined,
        platform => undefined
    },
    maps:merge(BaseState, Overrides).

%% Helper to add mock ERTS to release
add_mock_erts(TmpDir) ->
    ErtsDir = filename:join(TmpDir, "erts-27.0"),
    ok = filelib:ensure_dir(filename:join([ErtsDir, "bin", "placeholder"])),
    ok = file:write_file(filename:join([ErtsDir, "bin", "beam.smp"]), ~"beam").

%% Helper to add mock NIF to release
add_mock_nif(TmpDir) ->
    PrivDir = filename:join([TmpDir, "lib", "crypto-1.0.0", "priv"]),
    ok = filelib:ensure_dir(filename:join(PrivDir, "placeholder")),
    ok = file:write_file(filename:join(PrivDir, "test_nif.so"), ~"fake so").

%%%===================================================================
%%% Test fixtures
%%%===================================================================

create_mock_release() ->
    TmpDir = make_temp_dir("ocibuild_release"),

    %% Create directory structure using filename:join for cross-platform paths
    BinDir = filename:join(TmpDir, "bin"),
    LibDir = filename:join([TmpDir, "lib", "myapp-1.0.0", "ebin"]),
    RelDir = filename:join([TmpDir, "releases", "1.0.0"]),

    ok =
        filelib:ensure_dir(
            filename:join(BinDir, "placeholder")
        ),
    ok =
        filelib:ensure_dir(
            filename:join(LibDir, "placeholder")
        ),
    ok =
        filelib:ensure_dir(
            filename:join(RelDir, "placeholder")
        ),

    %% Create bin script (executable)
    BinPath = filename:join(BinDir, "myapp"),
    ok = file:write_file(BinPath, ~"#!/bin/sh\nexec erl -boot release"),
    ok = file:change_mode(BinPath, 8#755),

    %% Create beam file
    BeamPath = filename:join(LibDir, "myapp.beam"),
    ok = file:write_file(BeamPath, ~"FOR1...(beam data)"),
    ok = file:change_mode(BeamPath, 8#644),

    %% Create release file
    RelPath = filename:join(RelDir, "myapp.rel"),
    ok = file:write_file(RelPath, ~"{release, {\"myapp\", \"1.0.0\"}, ...}."),

    TmpDir.

%%%===================================================================
%%% tag_additional tests
%%%===================================================================

%% Setup for tag_additional tests
setup_tag_additional() ->
    %% Start required apps
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    %% Safely unload any existing mocks
    safe_unload_mock(ocibuild_registry),
    %% Mock registry module only - we'll use ocibuild_release_tests as adapter
    %% since this module has info/2 defined below
    meck:new(ocibuild_registry, [unstick, passthrough]),
    ok.

cleanup_tag_additional(_) ->
    safe_unload_mock(ocibuild_registry).

%% Safely unload a mock module
safe_unload_mock(Module) ->
    try meck:validate(Module) of
        _ -> meck:unload(Module)
    catch
        error:{not_mocked, _} -> ok
    end.

%% Fake adapter callbacks for testing tag_additional
info(_Format, _Args) -> ok.
console(_Format, _Args) -> ok.
error(_Format, _Args) -> ok.

tag_additional_test_() ->
    {foreach, fun setup_tag_additional/0, fun cleanup_tag_additional/1, [
        {"tag_additional empty list", fun tag_additional_empty_list_test/0},
        {"tag_additional single tag success", fun tag_additional_single_tag_success_test/0},
        {"tag_additional multiple tags success", fun tag_additional_multiple_tags_success_test/0},
        {"tag_additional mixed success and failure", fun tag_additional_mixed_results_test/0},
        {"tag_additional all failures", fun tag_additional_all_failures_test/0}
    ]}.

tag_additional_empty_list_test() ->
    %% Empty tag list should return empty result
    Result = ocibuild_release:tag_additional(
        ?MODULE,  %% Use this test module as the adapter
        ~"registry.example.io",
        ~"myorg/myapp",
        ~"sha256:abc123",
        [],
        #{token => ~"test-token"}
    ),
    ?assertEqual([], Result).

tag_additional_single_tag_success_test() ->
    Digest = ~"sha256:abc123",

    %% Mock tag_from_digest to succeed
    meck:expect(ocibuild_registry, tag_from_digest, fun(
        _Registry, _Repo, D, _Tag, _Auth
    ) ->
        {ok, D}
    end),

    Result = ocibuild_release:tag_additional(
        ?MODULE,  %% Use this test module as the adapter
        ~"registry.example.io",
        ~"myorg/myapp",
        Digest,
        [~"myapp:v1.0.0"],
        #{token => ~"test-token"}
    ),

    ?assertEqual([{~"myapp:v1.0.0", ok}], Result).

tag_additional_multiple_tags_success_test() ->
    Digest = ~"sha256:abc123",

    meck:expect(ocibuild_registry, tag_from_digest, fun(
        _Registry, _Repo, D, _Tag, _Auth
    ) ->
        {ok, D}
    end),

    Result = ocibuild_release:tag_additional(
        ?MODULE,  %% Use this test module as the adapter
        ~"registry.example.io",
        ~"myorg/myapp",
        Digest,
        [~"myapp:v1.0.0", ~"myapp:latest", ~"myapp:stable"],
        #{token => ~"test-token"}
    ),

    ?assertEqual([
        {~"myapp:v1.0.0", ok},
        {~"myapp:latest", ok},
        {~"myapp:stable", ok}
    ], Result).

tag_additional_mixed_results_test() ->
    Digest = ~"sha256:abc123",

    %% Mock to succeed for some tags, fail for others
    meck:expect(ocibuild_registry, tag_from_digest, fun(
        _Registry, _Repo, D, Tag, _Auth
    ) ->
        case Tag of
            ~"latest" -> {error, {http_error, 403, "Forbidden"}};
            _ -> {ok, D}
        end
    end),

    Result = ocibuild_release:tag_additional(
        ?MODULE,  %% Use this test module as the adapter
        ~"registry.example.io",
        ~"myorg/myapp",
        Digest,
        [~"myapp:v1.0.0", ~"myapp:latest", ~"myapp:stable"],
        #{token => ~"test-token"}
    ),

    ?assertEqual([
        {~"myapp:v1.0.0", ok},
        {~"myapp:latest", {error, {http_error, 403, "Forbidden"}}},
        {~"myapp:stable", ok}
    ], Result).

tag_additional_all_failures_test() ->
    Digest = ~"sha256:abc123",

    %% Mock to always fail
    meck:expect(ocibuild_registry, tag_from_digest, fun(
        _Registry, _Repo, _D, _Tag, _Auth
    ) ->
        {error, {http_error, 500, "Internal Server Error"}}
    end),

    Result = ocibuild_release:tag_additional(
        ?MODULE,  %% Use this test module as the adapter
        ~"registry.example.io",
        ~"myorg/myapp",
        Digest,
        [~"myapp:v1.0.0", ~"myapp:latest"],
        #{token => ~"test-token"}
    ),

    ?assertEqual([
        {~"myapp:v1.0.0", {error, {http_error, 500, "Internal Server Error"}}},
        {~"myapp:latest", {error, {http_error, 500, "Internal Server Error"}}}
    ], Result).
