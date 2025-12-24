%%%-------------------------------------------------------------------
-module(ocibuild_rebar3_tests).
-moduledoc "Tests for the rebar3 provider".

-include_lib("eunit/include/eunit.hrl").

-import(ocibuild_test_helpers, [make_temp_dir/1, cleanup_temp_dir/1]).

%%%===================================================================
%%% File collection tests (using exported function)
%%%===================================================================

collect_release_files_test() ->
    %% Create a mock release directory structure
    TmpDir = create_mock_release(),
    try
        %% Test file collection using the actual exported function
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
        ?assertEqual(8#644, LibMode band 8#777)
    after
        %% Cleanup
        cleanup_temp_dir(TmpDir)
    end.

collect_empty_dir_test() ->
    TmpDir = make_temp_dir("ocibuild_empty"),
    try
        {ok, Files} = ocibuild_release:collect_release_files(TmpDir),
        ?assertEqual([], Files)
    after
        cleanup_temp_dir(TmpDir)
    end.

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

    %% Verify image structure
    ?assert(is_map(Image)),
    ?assertEqual(1, length(maps:get(layers, Image))),

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
%%% Tag parsing tests (internal logic test)
%%%===================================================================

parse_tag_simple_test() ->
    ?assertEqual({~"myapp", ~"1.0.0"}, parse_tag(~"myapp:1.0.0")).

parse_tag_no_version_test() ->
    ?assertEqual({~"myapp", ~"latest"}, parse_tag(~"myapp")).

parse_tag_with_path_test() ->
    ?assertEqual({~"myorg/myapp", ~"v1"}, parse_tag(~"myorg/myapp:v1")).

%% Helper to test tag parsing logic (mirrors internal function)
parse_tag(Tag) ->
    case binary:split(Tag, ~":") of
        [Repo, ImageTag] -> {Repo, ImageTag};
        [Repo] -> {Repo, ~"latest"}
    end.

%%%===================================================================
%%% Auth tests (using exported function)
%%%===================================================================

get_push_auth_empty_test() ->
    %% Clear any existing env vars
    os:unsetenv("OCIBUILD_PUSH_TOKEN"),
    os:unsetenv("OCIBUILD_PUSH_USERNAME"),
    os:unsetenv("OCIBUILD_PUSH_PASSWORD"),

    ?assertEqual(#{}, ocibuild_release:get_push_auth()).

get_push_auth_token_test() ->
    os:putenv("OCIBUILD_PUSH_TOKEN", "mytoken123"),
    try
        Auth = ocibuild_release:get_push_auth(),
        ?assertEqual(#{token => ~"mytoken123"}, Auth)
    after
        os:unsetenv("OCIBUILD_PUSH_TOKEN")
    end.

get_push_auth_username_password_test() ->
    os:unsetenv("OCIBUILD_PUSH_TOKEN"),
    os:putenv("OCIBUILD_PUSH_USERNAME", "myuser"),
    os:putenv("OCIBUILD_PUSH_PASSWORD", "mypass"),
    try
        Auth = ocibuild_release:get_push_auth(),
        ?assertEqual(#{username => ~"myuser", password => ~"mypass"}, Auth)
    after
        os:unsetenv("OCIBUILD_PUSH_USERNAME"),
        os:unsetenv("OCIBUILD_PUSH_PASSWORD")
    end.

get_pull_auth_empty_test() ->
    %% Clear any existing env vars
    os:unsetenv("OCIBUILD_PULL_TOKEN"),
    os:unsetenv("OCIBUILD_PULL_USERNAME"),
    os:unsetenv("OCIBUILD_PULL_PASSWORD"),

    ?assertEqual(#{}, ocibuild_release:get_pull_auth()).

get_pull_auth_username_password_test() ->
    os:unsetenv("OCIBUILD_PULL_TOKEN"),
    os:putenv("OCIBUILD_PULL_USERNAME", "pulluser"),
    os:putenv("OCIBUILD_PULL_PASSWORD", "pullpass"),
    try
        Auth = ocibuild_release:get_pull_auth(),
        ?assertEqual(#{username => ~"pulluser", password => ~"pullpass"}, Auth)
    after
        os:unsetenv("OCIBUILD_PULL_USERNAME"),
        os:unsetenv("OCIBUILD_PULL_PASSWORD")
    end.

%%%===================================================================
%%% Format error tests
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
%%% parse_tag tests (moved to ocibuild_release)
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
%%% Auth partial tests
%%%===================================================================

get_push_auth_username_only_test() ->
    os:unsetenv("OCIBUILD_PUSH_TOKEN"),
    os:putenv("OCIBUILD_PUSH_USERNAME", "myuser"),
    os:unsetenv("OCIBUILD_PUSH_PASSWORD"),
    try
        %% Missing password should return empty
        ?assertEqual(#{}, ocibuild_release:get_push_auth())
    after
        os:unsetenv("OCIBUILD_PUSH_USERNAME")
    end.

get_push_auth_password_only_test() ->
    os:unsetenv("OCIBUILD_PUSH_TOKEN"),
    os:unsetenv("OCIBUILD_PUSH_USERNAME"),
    os:putenv("OCIBUILD_PUSH_PASSWORD", "mypass"),
    try
        %% Missing username should return empty
        ?assertEqual(#{}, ocibuild_release:get_push_auth())
    after
        os:unsetenv("OCIBUILD_PUSH_PASSWORD")
    end.

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

get_file_mode_test() ->
    TmpDir = make_temp_dir("ocibuild_mode"),
    try
        %% Create a file with specific permissions
        FilePath = filename:join(TmpDir, "test.txt"),
        ok = file:write_file(FilePath, ~"test"),
        ok = file:change_mode(FilePath, 8#644),
        ?assertEqual(8#644, ocibuild_release:get_file_mode(FilePath)),

        %% Create executable
        ExePath = filename:join(TmpDir, "test.sh"),
        ok = file:write_file(ExePath, ~"#!/bin/sh"),
        ok = file:change_mode(ExePath, 8#755),
        ?assertEqual(8#755, ocibuild_release:get_file_mode(ExePath))
    after
        cleanup_temp_dir(TmpDir)
    end.

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
    Config = [{base_image, ~"ubuntu:22.04"}],
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
%%% format_progress edge cases
%%%===================================================================

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

collect_symlink_inside_release_test() ->
    TmpDir = make_temp_dir("ocibuild_symlink_inside"),
    try
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
        ?assertEqual(~"real content", Content)
    after
        cleanup_temp_dir(TmpDir)
    end.

collect_symlink_outside_release_test() ->
    TmpDir = make_temp_dir("ocibuild_symlink_outside"),
    try
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
        ?assertEqual(false, EvilFile)
    after
        cleanup_temp_dir(TmpDir)
    end.

collect_symlink_relative_escape_test() ->
    TmpDir = make_temp_dir("ocibuild_symlink_escape"),
    try
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
        ?assertEqual(false, EscapeFile)
    after
        cleanup_temp_dir(TmpDir)
    end.

collect_broken_symlink_test() ->
    TmpDir = make_temp_dir("ocibuild_symlink_broken"),
    try
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
        ?assertEqual(1, length(Files))
    after
        cleanup_temp_dir(TmpDir)
    end.

collect_symlink_to_dir_inside_test() ->
    TmpDir = make_temp_dir("ocibuild_symlink_dir"),
    try
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
        ?assertNotEqual(false, LinkFile)
    after
        cleanup_temp_dir(TmpDir)
    end.

%%%===================================================================
%%% Multi-platform validation tests
%%%===================================================================

has_bundled_erts_true_test() ->
    TmpDir = create_mock_release(),
    try
        %% Add erts directory
        ErtsDir = filename:join(TmpDir, "erts-27.0"),
        ok = filelib:ensure_dir(filename:join(ErtsDir, "bin/placeholder")),
        ok = file:write_file(filename:join([ErtsDir, "bin", "beam.smp"]), ~"beam"),

        ?assertEqual(true, ocibuild_release:has_bundled_erts(TmpDir))
    after
        cleanup_temp_dir(TmpDir)
    end.

has_bundled_erts_false_test() ->
    TmpDir = create_mock_release(),
    try
        ?assertEqual(false, ocibuild_release:has_bundled_erts(TmpDir))
    after
        cleanup_temp_dir(TmpDir)
    end.

has_bundled_erts_nonexistent_test() ->
    ?assertEqual(false, ocibuild_release:has_bundled_erts("/nonexistent/path")).

check_for_native_code_found_so_test() ->
    TmpDir = create_mock_release(),
    try
        %% Add a .so file in lib/crypto-1.0.0/priv/
        PrivDir = filename:join([TmpDir, "lib", "crypto-1.0.0", "priv"]),
        ok = filelib:ensure_dir(filename:join(PrivDir, "placeholder")),
        ok = file:write_file(filename:join(PrivDir, "crypto_nif.so"), ~"fake so"),

        {warning, NifFiles} = ocibuild_release:check_for_native_code(TmpDir),
        ?assertEqual(1, length(NifFiles)),
        [#{app := App, file := File, extension := Ext}] = NifFiles,
        ?assertEqual(~"crypto", App),
        ?assertEqual(~"crypto_nif.so", File),
        ?assertEqual(~".so", Ext)
    after
        cleanup_temp_dir(TmpDir)
    end.

check_for_native_code_found_dll_test() ->
    TmpDir = create_mock_release(),
    try
        %% Add a .dll file
        PrivDir = filename:join([TmpDir, "lib", "nif_app-2.0.0", "priv"]),
        ok = filelib:ensure_dir(filename:join(PrivDir, "placeholder")),
        ok = file:write_file(filename:join(PrivDir, "nif_app.dll"), ~"fake dll"),

        {warning, NifFiles} = ocibuild_release:check_for_native_code(TmpDir),
        ?assertEqual(1, length(NifFiles)),
        [#{extension := Ext}] = NifFiles,
        ?assertEqual(~".dll", Ext)
    after
        cleanup_temp_dir(TmpDir)
    end.

check_for_native_code_found_dylib_test() ->
    TmpDir = create_mock_release(),
    try
        %% Add a .dylib file
        PrivDir = filename:join([TmpDir, "lib", "mac_nif-1.0.0", "priv"]),
        ok = filelib:ensure_dir(filename:join(PrivDir, "placeholder")),
        ok = file:write_file(filename:join(PrivDir, "mac_nif.dylib"), ~"fake dylib"),

        {warning, NifFiles} = ocibuild_release:check_for_native_code(TmpDir),
        ?assertEqual(1, length(NifFiles)),
        [#{extension := Ext}] = NifFiles,
        ?assertEqual(~".dylib", Ext)
    after
        cleanup_temp_dir(TmpDir)
    end.

check_for_native_code_none_test() ->
    TmpDir = create_mock_release(),
    try
        ?assertEqual({ok, []}, ocibuild_release:check_for_native_code(TmpDir))
    after
        cleanup_temp_dir(TmpDir)
    end.

check_for_native_code_nested_priv_test() ->
    TmpDir = create_mock_release(),
    try
        %% Add a .so file in a subdirectory of priv
        NestedDir = filename:join([TmpDir, "lib", "nested-1.0.0", "priv", "native"]),
        ok = filelib:ensure_dir(filename:join(NestedDir, "placeholder")),
        ok = file:write_file(filename:join(NestedDir, "nested_nif.so"), ~"fake so"),

        {warning, NifFiles} = ocibuild_release:check_for_native_code(TmpDir),
        ?assertEqual(1, length(NifFiles)),
        [#{file := File}] = NifFiles,
        %% Should include the path relative to priv
        ?assert(binary:match(File, ~"native") =/= nomatch)
    after
        cleanup_temp_dir(TmpDir)
    end.

validate_multiplatform_single_platform_ok_test() ->
    %% Single platform builds don't need validation
    Platform = #{os => ~"linux", architecture => ~"amd64"},
    ?assertEqual(ok, ocibuild_release:validate_multiplatform("/any/path", [Platform])).

validate_multiplatform_empty_platforms_ok_test() ->
    %% Empty platforms list is OK
    ?assertEqual(ok, ocibuild_release:validate_multiplatform("/any/path", [])).

validate_multiplatform_erts_error_test() ->
    TmpDir = create_mock_release(),
    try
        %% Add erts directory
        ErtsDir = filename:join(TmpDir, "erts-27.0"),
        ok = filelib:ensure_dir(filename:join(ErtsDir, "bin/placeholder")),

        Platforms = [
            #{os => ~"linux", architecture => ~"amd64"},
            #{os => ~"linux", architecture => ~"arm64"}
        ],
        Result = ocibuild_release:validate_multiplatform(TmpDir, Platforms),
        ?assertMatch({error, {bundled_erts, _}}, Result)
    after
        cleanup_temp_dir(TmpDir)
    end.

validate_multiplatform_ok_test() ->
    TmpDir = create_mock_release(),
    try
        %% No ERTS, no NIFs - should be OK
        Platforms = [
            #{os => ~"linux", architecture => ~"amd64"},
            #{os => ~"linux", architecture => ~"arm64"}
        ],
        ?assertEqual(ok, ocibuild_release:validate_multiplatform(TmpDir, Platforms))
    after
        cleanup_temp_dir(TmpDir)
    end.

%%%===================================================================
%%% CLI Integration tests (run/3 with platforms)
%%%===================================================================

%% Test that single platform build works without validation
run_single_platform_test() ->
    TmpDir = create_mock_release(),
    try
        State = create_mock_adapter_state(TmpDir, #{
            platform => ~"linux/amd64"
        }),
        %% Single platform should work even with ERTS
        add_mock_erts(TmpDir),

        %% This should succeed (single platform allows ERTS)
        Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
        ?assertMatch({ok, _}, Result)
    after
        cleanup_temp_dir(TmpDir)
    end.

%% Test that multi-platform build fails with bundled ERTS
run_multiplatform_erts_error_test() ->
    TmpDir = create_mock_release(),
    try
        State = create_mock_adapter_state(TmpDir, #{
            platform => ~"linux/amd64,linux/arm64"
        }),
        %% Add ERTS to trigger validation error
        add_mock_erts(TmpDir),

        Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
        ?assertMatch({error, {bundled_erts, _}}, Result)
    after
        cleanup_temp_dir(TmpDir)
    end.

%% Test that multi-platform validation succeeds without ERTS
%% (Full build would require network access, so we just test validation)
run_multiplatform_no_erts_test() ->
    TmpDir = create_mock_release(),
    try
        Platforms = [
            #{os => ~"linux", architecture => ~"amd64"},
            #{os => ~"linux", architecture => ~"arm64"}
        ],
        %% No ERTS, validation should succeed
        ?assertEqual(ok, ocibuild_release:validate_multiplatform(TmpDir, Platforms))
    after
        cleanup_temp_dir(TmpDir)
    end.

%% Test that no platform specified works (default behavior)
run_no_platform_test() ->
    TmpDir = create_mock_release(),
    try
        State = create_mock_adapter_state(TmpDir, #{
            platform => undefined
        }),
        Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
        ?assertMatch({ok, _}, Result)
    after
        cleanup_temp_dir(TmpDir)
    end.

%% Test that empty platform string works
run_empty_platform_test() ->
    TmpDir = create_mock_release(),
    try
        State = create_mock_adapter_state(TmpDir, #{
            platform => <<>>
        }),
        Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
        ?assertMatch({ok, _}, Result)
    after
        cleanup_temp_dir(TmpDir)
    end.

%% Test that nil platform (Elixir interop) works
run_nil_platform_test() ->
    TmpDir = create_mock_release(),
    try
        State = create_mock_adapter_state(TmpDir, #{
            platform => nil
        }),
        Result = ocibuild_release:run(ocibuild_test_adapter, State, #{}),
        ?assertMatch({ok, _}, Result)
    after
        cleanup_temp_dir(TmpDir)
    end.

%% Test that multi-platform with NIFs passes validation but detects NIFs.
%% NIFs trigger a warning (visible in test output) but don't block the build.
run_multiplatform_nif_warning_test() ->
    TmpDir = create_mock_release(),
    try
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
        %% (Warning "Native code detected..." is printed to stderr, visible in test output)
        ?assertEqual(ok, ocibuild_release:validate_multiplatform(TmpDir, Platforms))
    after
        cleanup_temp_dir(TmpDir)
    end.

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
        tag => ~"myapp:1.0.0",
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
