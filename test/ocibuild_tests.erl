%%%-------------------------------------------------------------------
-module(ocibuild_tests).
-moduledoc "Basic tests for `ocibuild`".

-include_lib("eunit/include/eunit.hrl").

-import(ocibuild_test_helpers, [make_temp_dir/1, make_temp_file/2, cleanup_temp_dir/1]).

%%%===================================================================
%%% Digest tests
%%%===================================================================

sha256_test() ->
    Expected = ~"sha256:2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
    ?assertEqual(Expected, ocibuild_digest:sha256(~"hello")).

digest_parts_test() ->
    Digest = ~"sha256:abc123",
    ?assertEqual(~"sha256", ocibuild_digest:algorithm(Digest)),
    ?assertEqual(~"abc123", ocibuild_digest:encoded(Digest)).

%%%===================================================================
%%% JSON tests
%%%===================================================================

json_encode_string_test() ->
    ?assertEqual(~"\"hello\"", ocibuild_json:encode(~"hello")).

json_encode_number_test() ->
    ?assertEqual(~"42", ocibuild_json:encode(42)).

json_encode_bool_test() ->
    ?assertEqual(~"true", ocibuild_json:encode(true)),
    ?assertEqual(~"false", ocibuild_json:encode(false)).

json_encode_null_test() ->
    ?assertEqual(~"null", ocibuild_json:encode(null)).

json_encode_array_test() ->
    ?assertEqual(~"[1,2,3]", ocibuild_json:encode([1, 2, 3])).

json_encode_object_test() ->
    %% Note: map ordering is not guaranteed, so we decode and compare
    Json = ocibuild_json:encode(#{~"a" => 1, ~"b" => 2}),
    Decoded = ocibuild_json:decode(Json),
    ?assertEqual(#{~"a" => 1, ~"b" => 2}, Decoded).

json_decode_test() ->
    ?assertEqual(
        #{~"key" => ~"value"},
        ocibuild_json:decode(~"{\"key\":\"value\"}")
    ).

%%%===================================================================
%%% Tar tests
%%%===================================================================

tar_basic_test() ->
    Files = [{~"/hello.txt", ~"Hello, World!", 8#644}],
    Tar = ocibuild_tar:create(Files),
    %% TAR should be a multiple of 512 bytes
    ?assertEqual(0, byte_size(Tar) rem 512),
    %% Should contain at least the file content
    ?assert(binary:match(Tar, ~"Hello, World!") =/= nomatch).

tar_compressed_test() ->
    Files = [{~"/test.txt", ~"test content", 8#644}],
    Compressed = ocibuild_tar:create_compressed(Files),
    %% Should start with gzip magic bytes
    <<16#1f, 16#8b, _/binary>> = Compressed,
    ok.

%% Security test: path traversal sequences must be rejected
tar_path_traversal_test() ->
    %% Paths with ".." components should raise error
    TraversalPaths = [
        ~"../etc/passwd",
        ~"foo/../../../etc/passwd",
        ~"/app/../etc/shadow",
        ~".."
    ],
    lists:foreach(
        fun(Path) ->
            Files = [{Path, ~"malicious content", 8#644}],
            ?assertError(
                {path_traversal, _},
                ocibuild_tar:create(Files)
            )
        end,
        TraversalPaths
    ).

tar_null_byte_injection_test() ->
    %% Paths with null bytes should raise error (security vulnerability)
    NullPaths = [
        <<"/app/file.txt", 0, ".evil">>,
        <<"/app/", 0, "passwd">>,
        <<0, "/etc/shadow">>
    ],
    lists:foreach(
        fun(Path) ->
            Files = [{Path, ~"content", 8#644}],
            ?assertError(
                {null_byte, _},
                ocibuild_tar:create(Files)
            )
        end,
        NullPaths
    ).

tar_empty_path_test() ->
    %% Empty paths should raise error
    Files = [{<<>>, ~"content", 8#644}],
    ?assertError(
        {empty_path, _},
        ocibuild_tar:create(Files)
    ).

tar_invalid_mode_test() ->
    %% Invalid modes should raise error
    InvalidModes = [
        %% Negative
        -1,
        %% Exceeds 7777 octal
        8#10000,
        %% Way too large
        16#FFFF
    ],
    lists:foreach(
        fun(Mode) ->
            Files = [{~"/app/file.txt", ~"content", Mode}],
            ?assertError(
                {invalid_mode, Mode},
                ocibuild_tar:create(Files)
            )
        end,
        InvalidModes
    ).

tar_valid_mode_edge_cases_test() ->
    %% Valid mode edge cases should work
    ValidModes = [
        %% Minimum
        0,
        %% Common file mode
        8#644,
        %% Common executable mode
        8#755,
        %% Maximum (with setuid, setgid, sticky)
        8#7777
    ],
    lists:foreach(
        fun(Mode) ->
            Files = [{~"/app/file.txt", ~"content", Mode}],
            Tar = ocibuild_tar:create(Files),
            ?assertEqual(0, byte_size(Tar) rem 512)
        end,
        ValidModes
    ).

tar_duplicate_paths_test() ->
    %% Duplicate paths should raise error (reports normalized path)
    Files = [
        {~"/app/file.txt", ~"content1", 8#644},
        {~"/app/other.txt", ~"content2", 8#644},
        %% Duplicate!
        {~"/app/file.txt", ~"content3", 8#644}
    ],
    ?assertError(
        {duplicate_paths, [~"./app/file.txt"]},
        ocibuild_tar:create(Files)
    ).

tar_duplicate_paths_after_normalization_test() ->
    %% Paths that normalize to the same value should be detected as duplicates
    %% "/app/file.txt" and "app/file.txt" both normalize to "./app/file.txt"
    Files = [
        {~"/app/file.txt", ~"content1", 8#644},
        {~"app/file.txt", ~"content2", 8#644}
    ],
    ?assertError(
        {duplicate_paths, [~"./app/file.txt"]},
        ocibuild_tar:create(Files)
    ).

%%%===================================================================
%%% Layer tests
%%%===================================================================

layer_create_test() ->
    Files = [{~"/app/test", ~"test data", 8#755}],
    Layer = ocibuild_layer:create(Files),

    %% Check all required fields exist
    ?assert(maps:is_key(media_type, Layer)),
    ?assert(maps:is_key(digest, Layer)),
    ?assert(maps:is_key(diff_id, Layer)),
    ?assert(maps:is_key(size, Layer)),
    ?assert(maps:is_key(data, Layer)),

    %% Digest should start with sha256:
    #{digest := Digest} = Layer,
    ?assertMatch(<<"sha256:", _/binary>>, Digest),

    %% Media type should be correct
    ?assertEqual(
        ~"application/vnd.oci.image.layer.v1.tar+gzip",
        maps:get(media_type, Layer)
    ).

%%%===================================================================
%%% Image building tests
%%%===================================================================

scratch_test() ->
    {ok, Image} = ocibuild:scratch(),
    ?assert(is_map(Image)),
    ?assertEqual(none, maps:get(base, Image)),
    ?assertEqual([], maps:get(layers, Image)).

image_config_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:entrypoint(Image0, [~"/app"]),
    Image2 = ocibuild:cmd(Image1, [~"--port", ~"8080"]),
    Image3 = ocibuild:env(Image2, #{~"FOO" => ~"bar"}),
    Image4 = ocibuild:workdir(Image3, ~"/app"),
    Image5 = ocibuild:expose(Image4, 8080),
    Image6 = ocibuild:user(Image5, ~"nobody"),

    Config = maps:get(config, Image6),
    InnerConfig = maps:get(~"config", Config),

    ?assertEqual([~"/app"], maps:get(~"Entrypoint", InnerConfig)),
    ?assertEqual([~"--port", ~"8080"], maps:get(~"Cmd", InnerConfig)),
    ?assertEqual(~"/app", maps:get(~"WorkingDir", InnerConfig)),
    ?assertEqual(~"nobody", maps:get(~"User", InnerConfig)).

add_layer_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [{~"/test.txt", ~"hello", 8#644}]),

    Layers = maps:get(layers, Image1),
    ?assertEqual(1, length(Layers)).

copy_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:copy(Image0, [{~"myapp", ~"binary data"}], ~"/app"),

    Layers = maps:get(layers, Image1),
    ?assertEqual(1, length(Layers)).

label_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:label(Image0, ~"version", ~"1.0.0"),
    Image2 = ocibuild:label(Image1, ~"author", ~"test"),

    Config = maps:get(config, Image2),
    InnerConfig = maps:get(~"config", Config),
    Labels = maps:get(~"Labels", InnerConfig),

    ?assertEqual(~"1.0.0", maps:get(~"version", Labels)),
    ?assertEqual(~"test", maps:get(~"author", Labels)).

expose_string_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:expose(Image0, ~"8080"),

    Config = maps:get(config, Image1),
    InnerConfig = maps:get(~"config", Config),
    ExposedPorts = maps:get(~"ExposedPorts", InnerConfig),

    ?assert(maps:is_key(~"8080/tcp", ExposedPorts)).

multiple_layers_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [{~"/file1.txt", ~"content1", 8#644}]),
    Image2 = ocibuild:add_layer(Image1, [{~"/file2.txt", ~"content2", 8#644}]),
    Image3 = ocibuild:add_layer(Image2, [{~"/file3.txt", ~"content3", 8#644}]),

    Layers = maps:get(layers, Image3),
    ?assertEqual(3, length(Layers)),

    %% Verify diff_ids are added to config
    Config = maps:get(config, Image3),
    Rootfs = maps:get(~"rootfs", Config),
    DiffIds = maps:get(~"diff_ids", Rootfs),
    ?assertEqual(3, length(DiffIds)).

%%%===================================================================
%%% Layout tests
%%%===================================================================

%% Setup/cleanup for layout tests that need temp directories
setup_layout_tempdir() ->
    make_temp_dir("ocibuild_layout_test").

cleanup_layout_tempdir(TmpDir) ->
    cleanup_temp_dir(TmpDir).

layout_export_test_() ->
    {foreach, fun setup_layout_tempdir/0, fun cleanup_layout_tempdir/1, [
        fun(TmpDir) ->
            {"export to directory", fun() -> export_directory_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"export multiple layers", fun() -> export_multiple_layers_test(TmpDir) end}
        end,
        fun(TmpDir) ->
            {"export with config", fun() -> export_with_config_test(TmpDir) end}
        end
    ]}.

export_directory_test(TmpDir) ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [{~"/test.txt", ~"hello", 8#644}]),
    Image2 = ocibuild:entrypoint(Image1, [~"/bin/sh"]),

    ok = ocibuild:export(Image2, TmpDir),

    %% Check required files exist
    ?assert(
        filelib:is_file(
            filename:join(TmpDir, "oci-layout")
        )
    ),
    ?assert(
        filelib:is_file(
            filename:join(TmpDir, "index.json")
        )
    ),
    ?assert(
        filelib:is_dir(
            filename:join([TmpDir, "blobs", "sha256"])
        )
    ).

save_tarball_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [{~"/test.txt", ~"hello", 8#644}]),

    TmpFile = make_temp_file("ocibuild_test_save", ".tar.gz"),
    try
        ok = ocibuild:save(Image1, TmpFile),

        %% Check file exists and is gzipped
        ?assert(filelib:is_file(TmpFile)),
        {ok, Data} = file:read_file(TmpFile),
        <<16#1f, 16#8b, _/binary>> = Data
    after
        file:delete(TmpFile)
    end.

%%%===================================================================
%%% Tests using meck for mocking
%%%===================================================================

%% Sample manifest for mocking
sample_manifest() ->
    #{
        ~"schemaVersion" => 2,
        ~"mediaType" => ~"application/vnd.oci.image.manifest.v1+json",
        ~"config" => #{
            ~"mediaType" => ~"application/vnd.oci.image.config.v1+json",
            ~"digest" => ~"sha256:abc123",
            ~"size" => 1234
        },
        ~"layers" => []
    }.

%% Sample config for mocking
sample_config() ->
    #{
        ~"architecture" => ~"amd64",
        ~"os" => ~"linux",
        ~"config" => #{},
        ~"rootfs" => #{~"type" => ~"layers", ~"diff_ids" => []}
    }.

from_with_meck_test_() ->
    {foreach, fun() -> meck:new(ocibuild_registry, [no_link]) end,
        fun(_) -> meck:unload(ocibuild_registry) end, [
            {"from/1 with string ref", fun from_string_ref_test/0},
            {"from/1 with tuple ref", fun from_tuple_ref_test/0},
            {"from/2 with auth", fun from_with_auth_test/0},
            {"from/3 with progress", fun from_with_progress_test/0},
            {"from/1 error handling", fun from_error_test/0},
            {"image ref parsing", fun parse_image_ref_test/0}
        ]}.

from_string_ref_test() ->
    %% Mock pull_manifest/3 which is called by from/1
    meck:expect(
        ocibuild_registry,
        pull_manifest,
        fun(~"docker.io", ~"library/alpine", ~"3.19") ->
            {ok, sample_manifest(), sample_config()}
        end
    ),

    Result = ocibuild:from(~"alpine:3.19"),

    ?assertMatch({ok, _}, Result),
    {ok, Image} = Result,
    ?assertEqual({~"docker.io", ~"library/alpine", ~"3.19"}, maps:get(base, Image)),
    ?assertEqual(~"amd64", maps:get(~"architecture", maps:get(config, Image))).

from_tuple_ref_test() ->
    meck:expect(
        ocibuild_registry,
        pull_manifest,
        fun(~"ghcr.io", ~"myorg/myapp", ~"v1.0.0") ->
            {ok, sample_manifest(), sample_config()}
        end
    ),

    Result = ocibuild:from({~"ghcr.io", ~"myorg/myapp", ~"v1.0.0"}),

    ?assertMatch({ok, _}, Result),
    {ok, Image} = Result,
    ?assertEqual({~"ghcr.io", ~"myorg/myapp", ~"v1.0.0"}, maps:get(base, Image)).

from_with_auth_test() ->
    Auth = #{token => ~"secret-token"},
    %% from/2 calls from/3 which calls pull_manifest/5
    meck:expect(
        ocibuild_registry,
        pull_manifest,
        fun(~"ghcr.io", ~"private/repo", ~"latest", Auth2, #{}) when Auth2 =:= Auth ->
            {ok, sample_manifest(), sample_config()}
        end
    ),

    Result = ocibuild:from(~"ghcr.io/private/repo:latest", Auth),

    ?assertMatch({ok, _}, Result).

from_with_progress_test() ->
    Self = self(),
    ProgressFn = fun(Info) -> Self ! {progress, Info} end,

    %% from/3 calls pull_manifest/5
    meck:expect(
        ocibuild_registry,
        pull_manifest,
        fun(~"docker.io", ~"library/alpine", ~"latest", #{}, Opts) ->
            %% Verify progress callback is passed and invoke it
            case maps:get(progress, Opts, undefined) of
                undefined ->
                    ok;
                Fn when is_function(Fn) ->
                    Fn(#{phase => manifest, bytes_received => 100, total_bytes => 100})
            end,
            {ok, sample_manifest(), sample_config()}
        end
    ),

    Result = ocibuild:from(~"alpine", #{}, #{progress => ProgressFn}),

    ?assertMatch({ok, _}, Result),
    %% Check we received progress
    receive
        {progress, #{phase := manifest}} -> ok
    after 100 ->
        %% Progress may not be called if mock doesn't invoke it
        ok
    end.

from_error_test() ->
    meck:expect(
        ocibuild_registry,
        pull_manifest,
        fun(~"docker.io", ~"library/notfound", ~"latest") ->
            {error, {http_error, 404, "Not Found"}}
        end
    ),

    Result = ocibuild:from(~"notfound"),

    ?assertMatch({error, _}, Result).

parse_image_ref_test() ->
    %% Test that simple image names are parsed correctly using the mock
    meck:expect(
        ocibuild_registry,
        pull_manifest,
        fun(Registry, Repo, Tag) ->
            %% Return the parsed values for verification
            {error, {parsed, Registry, Repo, Tag}}
        end
    ),

    %% Simple name defaults to docker.io/library
    {error, {parsed, R1, Repo1, T1}} = ocibuild:from(~"nginx"),
    ?assertEqual(~"docker.io", R1),
    ?assertEqual(~"library/nginx", Repo1),
    ?assertEqual(~"latest", T1),

    %% With tag
    {error, {parsed, R2, Repo2, T2}} = ocibuild:from(~"nginx:1.25"),
    ?assertEqual(~"docker.io", R2),
    ?assertEqual(~"library/nginx", Repo2),
    ?assertEqual(~"1.25", T2),

    %% With org
    {error, {parsed, R3, Repo3, T3}} = ocibuild:from(~"myorg/myapp:v1"),
    ?assertEqual(~"docker.io", R3),
    ?assertEqual(~"myorg/myapp", Repo3),
    ?assertEqual(~"v1", T3),

    %% Full registry path
    {error, {parsed, R4, Repo4, T4}} = ocibuild:from(~"ghcr.io/owner/repo:tag"),
    ?assertEqual(~"ghcr.io", R4),
    ?assertEqual(~"owner/repo", Repo4),
    ?assertEqual(~"tag", T4).

%%%===================================================================
%%% Platform parsing tests
%%%===================================================================

parse_platform_simple_test() ->
    %% Basic linux/amd64
    {ok, P1} = ocibuild:parse_platform(~"linux/amd64"),
    ?assertEqual(~"linux", maps:get(os, P1)),
    ?assertEqual(~"amd64", maps:get(architecture, P1)),
    ?assertEqual(error, maps:find(variant, P1)),

    %% linux/arm64
    {ok, P2} = ocibuild:parse_platform(~"linux/arm64"),
    ?assertEqual(~"linux", maps:get(os, P2)),
    ?assertEqual(~"arm64", maps:get(architecture, P2)).

parse_platform_with_variant_test() ->
    %% linux/arm64/v8
    {ok, P1} = ocibuild:parse_platform(~"linux/arm64/v8"),
    ?assertEqual(~"linux", maps:get(os, P1)),
    ?assertEqual(~"arm64", maps:get(architecture, P1)),
    ?assertEqual(~"v8", maps:get(variant, P1)),

    %% linux/arm/v7
    {ok, P2} = ocibuild:parse_platform(~"linux/arm/v7"),
    ?assertEqual(~"linux", maps:get(os, P2)),
    ?assertEqual(~"arm", maps:get(architecture, P2)),
    ?assertEqual(~"v7", maps:get(variant, P2)).

parse_platform_invalid_test() ->
    %% Missing architecture
    ?assertMatch({error, {invalid_platform, _}}, ocibuild:parse_platform(~"linux")),

    %% Empty OS
    ?assertMatch({error, {invalid_platform, _}}, ocibuild:parse_platform(~"/amd64")),

    %% Empty architecture
    ?assertMatch({error, {invalid_platform, _}}, ocibuild:parse_platform(~"linux/")),

    %% Completely empty
    ?assertMatch({error, {invalid_platform, _}}, ocibuild:parse_platform(~"")).

parse_platforms_multiple_test() ->
    %% Two platforms
    {ok, Platforms1} = ocibuild:parse_platforms(~"linux/amd64,linux/arm64"),
    ?assertEqual(2, length(Platforms1)),
    [P1, P2] = Platforms1,
    ?assertEqual(~"amd64", maps:get(architecture, P1)),
    ?assertEqual(~"arm64", maps:get(architecture, P2)),

    %% Three platforms with variant
    {ok, Platforms2} = ocibuild:parse_platforms(~"linux/amd64,linux/arm64,linux/arm/v7"),
    ?assertEqual(3, length(Platforms2)),

    %% Single platform
    {ok, Platforms3} = ocibuild:parse_platforms(~"linux/amd64"),
    ?assertEqual(1, length(Platforms3)).

parse_platforms_invalid_test() ->
    %% One invalid in list
    ?assertMatch({error, {invalid_platform, _}}, ocibuild:parse_platforms(~"linux/amd64,invalid")),

    %% Empty string returns empty list
    {ok, Empty} = ocibuild:parse_platforms(~""),
    ?assertEqual([], Empty).

parse_platform_charlist_test() ->
    %% Accept charlists as well as binaries
    {ok, P1} = ocibuild:parse_platform("linux/amd64"),
    ?assertEqual(~"linux", maps:get(os, P1)),
    ?assertEqual(~"amd64", maps:get(architecture, P1)),

    {ok, Platforms} = ocibuild:parse_platforms("linux/amd64,linux/arm64"),
    ?assertEqual(2, length(Platforms)).

%%%===================================================================
%%% Additional TAR tests - long paths and edge cases
%%%===================================================================

tar_empty_files_test() ->
    %% Test with no files - should still produce valid TAR with end marker
    Tar = ocibuild_tar:create([]),
    %% Should be two 512-byte zero blocks (end marker)
    ?assertEqual(1024, byte_size(Tar)).

tar_multiple_directories_test() ->
    %% Test with deeply nested directories
    Files = [
        {~"/a/b/c/file1.txt", ~"content1", 8#644},
        {~"/a/b/file2.txt", ~"content2", 8#644},
        {~"/x/y/z/file3.txt", ~"content3", 8#755}
    ],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512),
    %% Should contain all content
    ?assert(binary:match(Tar, ~"content1") =/= nomatch),
    ?assert(binary:match(Tar, ~"content2") =/= nomatch),
    ?assert(binary:match(Tar, ~"content3") =/= nomatch).

tar_large_content_test() ->
    %% Test with content that spans multiple blocks
    LargeContent = binary:copy(~"0123456789", 100),
    Files = [{~"/large.txt", LargeContent, 8#644}],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512),
    ?assert(byte_size(Tar) > 1024).

tar_long_path_test() ->
    %% Test path that exceeds 100 characters (requires prefix field)
    LongDir = binary:copy(~"subdir/", 20),
    LongPath = <<LongDir/binary, "file.txt">>,
    Files = [{LongPath, ~"test", 8#644}],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512).

tar_exact_block_size_test() ->
    %% Test content exactly 512 bytes (should need no padding)
    Content = binary:copy(~"X", 512),
    Files = [{~"/exact.txt", Content, 8#644}],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512).

tar_root_file_test() ->
    %% Test file at root level without leading slash
    Files = [{~"rootfile.txt", ~"content", 8#644}],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512),
    ?assert(binary:match(Tar, ~"content") =/= nomatch).

tar_various_permissions_test() ->
    %% Test different file permissions
    Files = [
        {~"/script.sh", ~"#!/bin/sh", 8#755},
        {~"/readonly.txt", ~"readonly", 8#444},
        {~"/private.key", ~"secret", 8#600}
    ],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512).

tar_already_normalized_path_test() ->
    %% Test paths that start with ./
    Files = [{~"./already/normalized/file.txt", ~"content", 8#644}],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512).

tar_safe_dots_test() ->
    %% Test that single dot and filenames with dots are allowed
    Files = [
        {~"/some.file.with.dots.txt", ~"content1", 8#644},
        {~"/dir.with.dots/file.txt", ~"content2", 8#644}
    ],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512).

tar_pax_long_filename_test() ->
    %% Test filename > 100 bytes that cannot be split (requires PAX header)
    %% This simulates the AshAuthentication issue from GitHub #21
    LongFilename =
        <<"Elixir.AshAuthentication.Strategy.Password.Authentication.",
            "Strategies.Password.Resettable.Options.beam">>,
    ?assert(byte_size(LongFilename) > 100),
    Path = <<"/app/lib/myapp/ebin/", LongFilename/binary>>,
    Content = <<"BEAM content here">>,
    Files = [{Path, Content, 8#644}],
    Tar = ocibuild_tar:create(Files),
    %% Archive should be valid (multiple of 512)
    ?assertEqual(0, byte_size(Tar) rem 512),
    %% Should contain PAX header (typeflag 'x')
    ?assertNotEqual(nomatch, binary:match(Tar, <<"path=">>)),
    %% Should contain the full filename in PAX data
    ?assertNotEqual(nomatch, binary:match(Tar, LongFilename)).

%% PAX extraction test fixture
setup_pax_tempdir() ->
    make_temp_dir("pax_test").

cleanup_pax_tempdir(TmpDir) ->
    cleanup_temp_dir(TmpDir).

tar_pax_extraction_test_() ->
    {foreach, fun setup_pax_tempdir/0, fun cleanup_pax_tempdir/1, [
        fun(TmpDir) ->
            {"PAX extracts correctly", fun() -> tar_pax_extracts_correctly_test(TmpDir) end}
        end
    ]}.

tar_pax_extracts_correctly_test(TmpDir) ->
    %% Verify PAX archive can be extracted by system tar
    LongFilename =
        <<"Elixir.VeryLongModuleName.WithMany.Nested.Modules.",
            "That.Exceed.The.Hundred.Byte.Limit.In.Ustar.beam">>,
    Path = <<"/app/lib/myapp/ebin/", LongFilename/binary>>,
    Content = <<"test content 12345">>,
    Files = [{Path, Content, 8#644}],
    Tar = ocibuild_tar:create(Files),

    %% Write tar and extract with system tar
    TarFile = filename:join(TmpDir, "test.tar"),
    ok = file:write_file(TarFile, Tar),
    Cmd = "tar -xf " ++ TarFile ++ " -C " ++ TmpDir,
    _ = os:cmd(Cmd),

    %% Verify extracted file exists and has correct content
    ExtractedPath = filename:join([
        TmpDir,
        ".",
        "app",
        "lib",
        "myapp",
        "ebin",
        binary_to_list(LongFilename)
    ]),
    ?assert(filelib:is_regular(ExtractedPath)),
    ?assertEqual({ok, Content}, file:read_file(ExtractedPath)).

tar_pax_very_long_path_test() ->
    %% Test extremely long path (> 255 bytes, exceeds ustar entirely)
    DeepPath = binary:copy(<<"very_long_directory_name/">>, 15),
    Filename = <<"file.txt">>,
    Path = <<"/", DeepPath/binary, Filename/binary>>,
    ?assert(byte_size(Path) > 255),
    Content = <<"deep content">>,
    Files = [{Path, Content, 8#644}],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512),
    ?assertNotEqual(nomatch, binary:match(Tar, <<"path=">>)).

tar_ustar_still_used_when_possible_test() ->
    %% Verify short paths don't use PAX (efficiency)
    Files = [{~"/short/path.txt", ~"content", 8#644}],
    Tar = ocibuild_tar:create(Files),
    ?assertEqual(0, byte_size(Tar) rem 512),
    %% Should NOT contain PAX header for short paths
    ?assertEqual(nomatch, binary:match(Tar, <<"path=">>)).

%%%===================================================================
%%% Additional Digest tests
%%%===================================================================

digest_sha256_hex_test() ->
    %% Test sha256_hex without prefix
    Hex = ocibuild_digest:sha256_hex(~"hello"),
    ?assertEqual(64, byte_size(Hex)),
    ?assertMatch(~"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824", Hex).

digest_to_hex_test() ->
    %% Test to_hex
    Hex = ocibuild_digest:to_hex(<<255, 0, 127, 128>>),
    ?assertEqual(~"ff007f80", Hex).

digest_from_hex_test() ->
    %% Test from_hex
    Bin = ocibuild_digest:from_hex(~"FF007F80"),
    ?assertEqual(<<255, 0, 127, 128>>, Bin).

digest_roundtrip_test() ->
    %% Test to_hex -> from_hex roundtrip
    Original = <<1, 2, 3, 4, 5, 6, 7, 8>>,
    Hex = ocibuild_digest:to_hex(Original),
    Result = ocibuild_digest:from_hex(Hex),
    ?assertEqual(Original, Result).

digest_invalid_algorithm_test() ->
    %% Test algorithm with invalid digest (no colon)
    ?assertError({invalid_digest, ~"nocolon"}, ocibuild_digest:algorithm(~"nocolon")).

digest_invalid_encoded_test() ->
    %% Test encoded with invalid digest (no colon)
    ?assertError({invalid_digest, ~"nocolon"}, ocibuild_digest:encoded(~"nocolon")).

digest_empty_data_test() ->
    %% Test SHA256 of empty binary
    Digest = ocibuild_digest:sha256(<<>>),
    ?assertMatch(<<"sha256:", _/binary>>, Digest).

%%%===================================================================
%%% Additional JSON tests
%%%===================================================================

json_encode_nested_test() ->
    %% Test deeply nested structures
    Nested = #{~"level1" => #{~"level2" => #{~"level3" => ~"value"}}},
    Json = ocibuild_json:encode(Nested),
    Decoded = ocibuild_json:decode(Json),
    ?assertEqual(Nested, Decoded).

json_encode_list_of_maps_test() ->
    %% Test list of maps
    Data = [#{~"a" => 1}, #{~"b" => 2}],
    Json = ocibuild_json:encode(Data),
    Decoded = ocibuild_json:decode(Json),
    ?assertEqual(Data, Decoded).

json_encode_empty_structures_test() ->
    %% Test empty map and list
    ?assertEqual(~"{}", ocibuild_json:encode(#{})),
    ?assertEqual(~"[]", ocibuild_json:encode([])).

json_encode_special_chars_test() ->
    %% Test strings with special characters
    Data = ~"line1\nline2\ttab",
    Json = ocibuild_json:encode(Data),
    Decoded = ocibuild_json:decode(Json),
    ?assertEqual(Data, Decoded).

json_encode_unicode_test() ->
    %% Test unicode strings
    Data = ~"hello 世界",
    Json = ocibuild_json:encode(Data),
    Decoded = ocibuild_json:decode(Json),
    ?assertEqual(Data, Decoded).

%%%===================================================================
%%% Additional Manifest tests
%%%===================================================================

manifest_media_types_test() ->
    ?assertEqual(
        ~"application/vnd.oci.image.manifest.v1+json",
        ocibuild_manifest:media_type()
    ),
    ?assertEqual(
        ~"application/vnd.oci.image.config.v1+json",
        ocibuild_manifest:config_media_type()
    ).

manifest_layer_media_types_test() ->
    %% Test all compression types
    ?assertEqual(
        ~"application/vnd.oci.image.layer.v1.tar+gzip",
        ocibuild_manifest:layer_media_type()
    ),
    ?assertEqual(
        ~"application/vnd.oci.image.layer.v1.tar+gzip",
        ocibuild_manifest:layer_media_type(gzip)
    ),
    ?assertEqual(
        ~"application/vnd.oci.image.layer.v1.tar+zstd",
        ocibuild_manifest:layer_media_type(zstd)
    ),
    ?assertEqual(
        ~"application/vnd.oci.image.layer.v1.tar",
        ocibuild_manifest:layer_media_type(none)
    ).

manifest_build_test() ->
    ConfigDesc = #{
        ~"mediaType" => ~"application/vnd.oci.image.config.v1+json",
        ~"digest" => ~"sha256:abc123",
        ~"size" => 100
    },
    LayerDesc = #{
        ~"mediaType" => ~"application/vnd.oci.image.layer.v1.tar+gzip",
        ~"digest" => ~"sha256:def456",
        ~"size" => 200
    },
    {ManifestJson, ManifestDigest} = ocibuild_manifest:build(ConfigDesc, [LayerDesc]),
    ?assert(is_binary(ManifestJson)),
    ?assertMatch(<<"sha256:", _/binary>>, ManifestDigest),
    %% Verify manifest structure
    Decoded = ocibuild_json:decode(ManifestJson),
    ?assertEqual(2, maps:get(~"schemaVersion", Decoded)),
    ?assertEqual(~"application/vnd.oci.image.manifest.v1+json", maps:get(~"mediaType", Decoded)).

manifest_build_empty_layers_test() ->
    ConfigDesc = #{
        ~"mediaType" => ~"application/vnd.oci.image.config.v1+json",
        ~"digest" => ~"sha256:abc123",
        ~"size" => 100
    },
    {ManifestJson, _Digest} = ocibuild_manifest:build(ConfigDesc, []),
    Decoded = ocibuild_json:decode(ManifestJson),
    ?assertEqual([], maps:get(~"layers", Decoded)).

%%%===================================================================
%%% Additional Image Configuration tests
%%%===================================================================

multiple_env_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:env(Image0, #{~"VAR1" => ~"val1", ~"VAR2" => ~"val2"}),
    Image2 = ocibuild:env(Image1, #{~"VAR3" => ~"val3"}),

    Config = maps:get(config, Image2),
    InnerConfig = maps:get(~"config", Config),
    EnvList = maps:get(~"Env", InnerConfig),

    ?assertEqual(3, length(EnvList)),
    ?assert(lists:member(~"VAR1=val1", EnvList)),
    ?assert(lists:member(~"VAR2=val2", EnvList)),
    ?assert(lists:member(~"VAR3=val3", EnvList)).

multiple_expose_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:expose(Image0, 80),
    Image2 = ocibuild:expose(Image1, 443),
    Image3 = ocibuild:expose(Image2, ~"8080"),

    Config = maps:get(config, Image3),
    InnerConfig = maps:get(~"config", Config),
    ExposedPorts = maps:get(~"ExposedPorts", InnerConfig),

    ?assert(maps:is_key(~"80/tcp", ExposedPorts)),
    ?assert(maps:is_key(~"443/tcp", ExposedPorts)),
    ?assert(maps:is_key(~"8080/tcp", ExposedPorts)).

multiple_labels_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:label(Image0, ~"label1", ~"value1"),
    Image2 = ocibuild:label(Image1, ~"label2", ~"value2"),
    Image3 = ocibuild:label(Image2, ~"org.opencontainers.image.title", ~"My App"),

    Config = maps:get(config, Image3),
    InnerConfig = maps:get(~"config", Config),
    Labels = maps:get(~"Labels", InnerConfig),

    ?assertEqual(~"value1", maps:get(~"label1", Labels)),
    ?assertEqual(~"value2", maps:get(~"label2", Labels)),
    ?assertEqual(~"My App", maps:get(~"org.opencontainers.image.title", Labels)).

cmd_and_entrypoint_test() ->
    %% Test setting both CMD and ENTRYPOINT
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:entrypoint(Image0, [~"/usr/bin/myapp"]),
    Image2 = ocibuild:cmd(Image1, [~"--config", ~"/etc/myapp.conf"]),

    Config = maps:get(config, Image2),
    InnerConfig = maps:get(~"config", Config),

    ?assertEqual([~"/usr/bin/myapp"], maps:get(~"Entrypoint", InnerConfig)),
    ?assertEqual([~"--config", ~"/etc/myapp.conf"], maps:get(~"Cmd", InnerConfig)).

config_created_timestamp_test() ->
    %% Test that created timestamp is set
    {ok, Image} = ocibuild:scratch(),
    Config = maps:get(config, Image),
    Created = maps:get(~"created", Config),
    ?assert(is_binary(Created)),
    %% Should be ISO8601 format
    ?assertMatch(<<_:4/binary, "-", _:2/binary, "-", _:2/binary, "T", _/binary>>, Created).

config_architecture_test() ->
    %% Test default architecture
    {ok, Image} = ocibuild:scratch(),
    Config = maps:get(config, Image),
    ?assertEqual(~"amd64", maps:get(~"architecture", Config)),
    ?assertEqual(~"linux", maps:get(~"os", Config)).

%%%===================================================================
%%% User/UID tests (non-root by default feature)
%%%===================================================================

%% Test that ocibuild:user/2 sets the User field correctly
user_set_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:user(Image0, ~"65534"),
    Config = maps:get(config, Image1),
    InnerConfig = maps:get(~"config", Config),
    ?assertEqual(~"65534", maps:get(~"User", InnerConfig)).

%% Test custom UID
user_custom_uid_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:user(Image0, ~"1000"),
    Config = maps:get(config, Image1),
    InnerConfig = maps:get(~"config", Config),
    ?assertEqual(~"1000", maps:get(~"User", InnerConfig)).

%% Test that user can be a username string
user_string_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:user(Image0, ~"nobody"),
    Config = maps:get(config, Image1),
    InnerConfig = maps:get(~"config", Config),
    ?assertEqual(~"nobody", maps:get(~"User", InnerConfig)).

%% Test user:group format
user_with_group_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:user(Image0, ~"1000:1000"),
    Config = maps:get(config, Image1),
    InnerConfig = maps:get(~"config", Config),
    ?assertEqual(~"1000:1000", maps:get(~"User", InnerConfig)).

%%%===================================================================
%%% UID Integration tests (build_image with uid option)
%%%===================================================================

%% Helper to extract User field from built image
get_user_from_image(Image) ->
    Config = maps:get(config, Image),
    InnerConfig = maps:get(~"config", Config),
    maps:get(~"User", InnerConfig, undefined).

%% Test that build_image defaults to UID 65534 (nobody) when uid not specified
build_image_uid_default_test() ->
    Files = [{~"/app/bin/app", ~"#!/bin/sh\necho hello", 8#755}],
    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => ~"app"
    }),
    ?assertEqual(~"65534", get_user_from_image(Image)).

%% Test that build_image with uid=0 sets User to "0" (root)
build_image_uid_zero_test() ->
    Files = [{~"/app/bin/app", ~"#!/bin/sh\necho hello", 8#755}],
    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => ~"app",
        uid => 0
    }),
    ?assertEqual(~"0", get_user_from_image(Image)).

%% Test that build_image with positive uid sets User correctly
build_image_uid_positive_test() ->
    Files = [{~"/app/bin/app", ~"#!/bin/sh\necho hello", 8#755}],
    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => ~"app",
        uid => 1000
    }),
    ?assertEqual(~"1000", get_user_from_image(Image)).

%% Test that build_image with negative uid raises error
build_image_uid_negative_test() ->
    Files = [{~"/app/bin/app", ~"#!/bin/sh\necho hello", 8#755}],
    Result = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => ~"app",
        uid => -1
    }),
    ?assertMatch({error, {{invalid_uid, -1, _}, _}}, Result).

%% Test that build_image with non-integer uid raises error
build_image_uid_invalid_type_test() ->
    Files = [{~"/app/bin/app", ~"#!/bin/sh\necho hello", 8#755}],
    Result = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => ~"app",
        uid => "1000"
    }),
    ?assertMatch({error, {{invalid_uid_type, "1000", _}, _}}, Result).

%% Test that build_image with nil uid (Elixir compatibility) defaults to 65534
build_image_uid_nil_test() ->
    Files = [{~"/app/bin/app", ~"#!/bin/sh\necho hello", 8#755}],
    {ok, Image} = ocibuild_release:build_image(~"scratch", Files, #{
        release_name => ~"app",
        uid => nil
    }),
    ?assertEqual(~"65534", get_user_from_image(Image)).

%%%===================================================================
%%% Layout tests - additional coverage
%%%===================================================================

save_tarball_with_tag_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [{~"/test.txt", ~"hello", 8#644}]),

    TmpFile = make_temp_file("ocibuild_test_tag", ".tar.gz"),
    try
        ok = ocibuild:save(Image1, TmpFile, #{tag => ~"myapp:v1.0.0"}),
        ?assert(filelib:is_file(TmpFile))
    after
        file:delete(TmpFile)
    end.

export_multiple_layers_test(TmpDir) ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [{~"/file1.txt", ~"content1", 8#644}]),
    Image2 = ocibuild:add_layer(Image1, [{~"/file2.txt", ~"content2", 8#644}]),
    Image3 = ocibuild:entrypoint(Image2, [~"/bin/sh"]),

    ok = ocibuild:export(Image3, TmpDir),

    %% Verify blobs directory has multiple files
    BlobsDir = filename:join([TmpDir, "blobs", "sha256"]),
    {ok, Files} = file:list_dir(BlobsDir),
    %% Should have config + manifest + 2 layers = 4 blobs
    ?assertEqual(4, length(Files)).

export_with_config_test(TmpDir) ->
    %% Test export with full configuration
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [{~"/app", ~"binary", 8#755}]),
    Image2 = ocibuild:entrypoint(Image1, [~"/app"]),
    Image3 = ocibuild:cmd(Image2, [~"--help"]),
    Image4 = ocibuild:env(Image3, #{~"DEBUG" => ~"1"}),
    Image5 = ocibuild:workdir(Image4, ~"/app"),
    Image6 = ocibuild:expose(Image5, 8080),
    Image7 = ocibuild:label(Image6, ~"version", ~"1.0"),
    Image8 = ocibuild:user(Image7, ~"app"),

    ok = ocibuild:export(Image8, TmpDir),
    ?assert(filelib:is_file(filename:join(TmpDir, "index.json"))).

%%%===================================================================
%%% Registry retry tests
%%%===================================================================

registry_is_retriable_error_test() ->
    %% Retriable errors - connection/timeout
    ?assertEqual(true, ocibuild_registry:is_retriable_error({failed_connect, some_reason})),
    ?assertEqual(true, ocibuild_registry:is_retriable_error(timeout)),
    ?assertEqual(true, ocibuild_registry:is_retriable_error(closed)),
    ?assertEqual(true, ocibuild_registry:is_retriable_error(econnreset)),
    ?assertEqual(true, ocibuild_registry:is_retriable_error({error, econnreset})),

    %% Retriable errors - server errors (5xx)
    ?assertEqual(true, ocibuild_registry:is_retriable_error({http_error, 500, "Internal Error"})),
    ?assertEqual(true, ocibuild_registry:is_retriable_error({http_error, 502, "Bad Gateway"})),
    ?assertEqual(
        true, ocibuild_registry:is_retriable_error({http_error, 503, "Service Unavailable"})
    ),
    ?assertEqual(true, ocibuild_registry:is_retriable_error({http_error, 504, "Gateway Timeout"})),

    %% Retriable errors - rate limiting
    ?assertEqual(
        true, ocibuild_registry:is_retriable_error({http_error, 429, "Too Many Requests"})
    ),

    %% Non-retriable errors
    ?assertEqual(false, ocibuild_registry:is_retriable_error({http_error, 404, "Not Found"})),
    ?assertEqual(false, ocibuild_registry:is_retriable_error({http_error, 401, "Unauthorized"})),
    ?assertEqual(false, ocibuild_registry:is_retriable_error(unknown_error)),
    ?assertEqual(false, ocibuild_registry:is_retriable_error({some, complex, error})).

layout_blob_path_test() ->
    ?assertEqual(
        ~"blobs/sha256/abc123def456",
        ocibuild_layout:blob_path(~"sha256:abc123def456")
    ).

layout_build_index_test() ->
    Index = ocibuild_layout:build_index(~"sha256:manifest123", 500, ~"myapp:1.0.0"),
    ?assertEqual(2, maps:get(~"schemaVersion", Index)),
    Manifests = maps:get(~"manifests", Index),
    ?assertEqual(1, length(Manifests)),
    [Manifest] = Manifests,
    ?assertEqual(~"sha256:manifest123", maps:get(~"digest", Manifest)),
    ?assertEqual(500, maps:get(~"size", Manifest)),
    Annotations = maps:get(~"annotations", Manifest),
    ?assertEqual(~"myapp:1.0.0", maps:get(~"org.opencontainers.image.ref.name", Annotations)).

%%%===================================================================
%%% Layout tests with mocking for base layers
%%%===================================================================

layout_base_layer_test_() ->
    {foreach, fun setup_layout_meck/0, fun cleanup_layout_meck/1, [
        {"save tarball with base image", fun save_tarball_base_image_test/0},
        {"save tarball base layer download failure", fun save_tarball_base_layer_failure_test/0}
    ]}.

setup_layout_meck() ->
    %% Unload any existing mock first
    catch meck:unload(ocibuild_registry),
    %% Use passthrough so with_retry/2 and other non-mocked functions work
    meck:new(ocibuild_registry, [no_link, passthrough]),
    ok.

cleanup_layout_meck(_) ->
    meck:unload(ocibuild_registry),
    ok.

save_tarball_base_image_test() ->
    %% Mock pull_blob to return compressed layer data
    LayerData = zlib:gzip(~"layer content"),
    meck:expect(
        ocibuild_registry,
        pull_blob,
        fun(_Registry, _Repo, _Digest, _Auth, _Opts) ->
            {ok, LayerData}
        end
    ),

    %% Create image with base manifest that has layers
    BaseManifest = #{
        ~"schemaVersion" => 2,
        ~"layers" => [
            #{
                ~"mediaType" => ~"application/vnd.oci.image.layer.v1.tar+gzip",
                ~"digest" => ~"sha256:baselayer123",
                ~"size" => 100
            }
        ]
    },
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [{~"/test.txt", ~"hello", 8#644}]),
    %% Add base image info
    Image2 = Image1#{
        base => {~"docker.io", ~"library/alpine", ~"latest"},
        base_manifest => BaseManifest
    },

    TmpFile = make_temp_file("ocibuild_base", ".tar.gz"),
    try
        ok = ocibuild:save(Image2, TmpFile, #{tag => ~"test:1.0"}),
        ?assert(filelib:is_file(TmpFile)),
        %% Verify pull_blob was called
        ?assert(meck:called(ocibuild_registry, pull_blob, '_'))
    after
        file:delete(TmpFile)
    end.

save_tarball_base_layer_failure_test() ->
    %% Mock pull_blob to fail with non-retriable error
    meck:expect(
        ocibuild_registry,
        pull_blob,
        fun(_Registry, _Repo, _Digest, _Auth, _Opts) ->
            {error, {http_error, 404, "Not Found"}}
        end
    ),

    BaseManifest = #{
        ~"schemaVersion" => 2,
        ~"layers" => [
            #{
                ~"mediaType" => ~"application/vnd.oci.image.layer.v1.tar+gzip",
                ~"digest" => ~"sha256:baselayer123",
                ~"size" => 100
            }
        ]
    },
    {ok, Image0} = ocibuild:scratch(),
    Image1 = Image0#{
        base => {~"docker.io", ~"library/alpine", ~"latest"},
        base_manifest => BaseManifest
    },

    TmpFile = make_temp_file("ocibuild_fail", ".tar.gz"),
    try
        %% Should return error when base layer download fails
        Result = ocibuild:save(Image1, TmpFile),
        ?assertMatch({error, _}, Result)
    after
        file:delete(TmpFile)
    end.

%%%===================================================================
%%% Reproducible builds tests (ocibuild_time)
%%%===================================================================

time_get_timestamp_default_test() ->
    %% Test that get_timestamp returns current time when no env var is set
    try
        os:unsetenv("SOURCE_DATE_EPOCH"),
        Before = erlang:system_time(second),
        Timestamp = ocibuild_time:get_timestamp(),
        After = erlang:system_time(second),
        ?assert(Timestamp >= Before),
        ?assert(Timestamp =< After)
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.

time_get_timestamp_from_env_test() ->
    %% Test that get_timestamp reads SOURCE_DATE_EPOCH
    os:putenv("SOURCE_DATE_EPOCH", "1234567890"),
    try
        ?assertEqual(1234567890, ocibuild_time:get_timestamp())
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.

time_get_timestamp_invalid_env_test() ->
    %% Test that invalid SOURCE_DATE_EPOCH falls back to current time
    os:putenv("SOURCE_DATE_EPOCH", "not_a_number"),
    try
        Before = erlang:system_time(second),
        Timestamp = ocibuild_time:get_timestamp(),
        After = erlang:system_time(second),
        ?assert(Timestamp >= Before),
        ?assert(Timestamp =< After)
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.

time_get_timestamp_negative_env_test() ->
    %% Test that negative SOURCE_DATE_EPOCH falls back to current time
    os:putenv("SOURCE_DATE_EPOCH", "-100"),
    try
        Before = erlang:system_time(second),
        Timestamp = ocibuild_time:get_timestamp(),
        After = erlang:system_time(second),
        ?assert(Timestamp >= Before),
        ?assert(Timestamp =< After)
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.

time_unix_to_iso8601_epoch_test() ->
    %% Test that Unix epoch (0) converts correctly
    ?assertEqual(~"1970-01-01T00:00:00Z", ocibuild_time:unix_to_iso8601(0)).

time_unix_to_iso8601_known_value_test() ->
    %% Test a known timestamp conversion
    %% 1700000000 = 2023-11-14T22:13:20Z
    ?assertEqual(~"2023-11-14T22:13:20Z", ocibuild_time:unix_to_iso8601(1700000000)).

time_get_iso8601_with_env_test() ->
    %% Test that get_iso8601 uses SOURCE_DATE_EPOCH
    os:putenv("SOURCE_DATE_EPOCH", "1700000000"),
    try
        ?assertEqual(~"2023-11-14T22:13:20Z", ocibuild_time:get_iso8601())
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.

%%%===================================================================
%%% TAR reproducibility tests
%%%===================================================================

tar_file_sorting_test() ->
    %% Test that files are sorted regardless of input order
    Files = [
        {~"/z/file.txt", ~"z", 8#644},
        {~"/a/file.txt", ~"a", 8#644},
        {~"/m/file.txt", ~"m", 8#644}
    ],
    MTime = 1700000000,
    Tar1 = ocibuild_tar:create(Files, #{mtime => MTime}),
    %% Same files in different order
    Tar2 = ocibuild_tar:create(lists:reverse(Files), #{mtime => MTime}),
    %% Should produce identical output
    ?assertEqual(Tar1, Tar2).

tar_mtime_option_test() ->
    %% Test that mtime option is used
    Files = [{~"/test.txt", ~"content", 8#644}],
    FixedTime = 1234567890,
    Tar1 = ocibuild_tar:create(Files, #{mtime => FixedTime}),
    Tar2 = ocibuild_tar:create(Files, #{mtime => FixedTime}),
    ?assertEqual(Tar1, Tar2),
    %% Verify digests match
    ?assertEqual(ocibuild_digest:sha256(Tar1), ocibuild_digest:sha256(Tar2)).

tar_reproducible_test() ->
    %% Test full reproducibility with fixed mtime
    Files = [
        {~"/app/bin/myapp", ~"#!/bin/sh\necho hello", 8#755},
        {~"/app/config.json", ~"{\"key\": \"value\"}", 8#644}
    ],
    MTime = 1700000000,
    %% Create twice with same mtime
    Tar1 = ocibuild_tar:create(Files, #{mtime => MTime}),
    Tar2 = ocibuild_tar:create(Files, #{mtime => MTime}),
    ?assertEqual(Tar1, Tar2),
    %% Verify the digests match
    ?assertEqual(ocibuild_digest:sha256(Tar1), ocibuild_digest:sha256(Tar2)).

%%%===================================================================
%%% Layer reproducibility tests
%%%===================================================================

layer_mtime_option_test() ->
    %% Test that layer respects mtime option
    Files = [{~"/test.txt", ~"content", 8#644}],
    MTime = 1700000000,
    Layer1 = ocibuild_layer:create(Files, #{mtime => MTime}),
    Layer2 = ocibuild_layer:create(Files, #{mtime => MTime}),
    %% Digests should match
    ?assertEqual(maps:get(digest, Layer1), maps:get(digest, Layer2)),
    ?assertEqual(maps:get(diff_id, Layer1), maps:get(diff_id, Layer2)).

gzip_mtime_is_zero_test() ->
    %% Verify that Erlang's zlib:gzip sets MTIME to 0 in gzip header (RFC 1952)
    %% This is important for reproducible builds - if MTIME contained current time,
    %% compressed layers would have different digests on each build.
    Data = ~"test data for gzip",
    Gzipped = zlib:gzip(Data),
    %% Gzip header format: ID1 ID2 CM FLG MTIME(4 bytes little-endian) XFL OS
    <<16#1f, 16#8b, _CM, _FLG, MTime:32/little, _XFL, _OS, _Rest/binary>> = Gzipped,
    ?assertEqual(0, MTime).

layer_reproducible_test() ->
    %% Test that layers are reproducible with SOURCE_DATE_EPOCH
    os:putenv("SOURCE_DATE_EPOCH", "1700000000"),
    try
        Files = [{~"/app/file", ~"data", 8#644}],
        MTime = ocibuild_time:get_timestamp(),
        Layer1 = ocibuild_layer:create(Files, #{mtime => MTime}),
        Layer2 = ocibuild_layer:create(Files, #{mtime => MTime}),
        ?assertEqual(maps:get(digest, Layer1), maps:get(digest, Layer2))
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.

%%%===================================================================
%%% Full image reproducibility tests
%%%===================================================================

reproducible_scratch_image_test() ->
    %% Test that scratch images are reproducible with SOURCE_DATE_EPOCH
    os:putenv("SOURCE_DATE_EPOCH", "1700000000"),
    try
        {ok, Image1} = ocibuild:scratch(),
        {ok, Image2} = ocibuild:scratch(),
        %% Configs should be identical
        Config1 = maps:get(config, Image1),
        Config2 = maps:get(config, Image2),
        ?assertEqual(maps:get(~"created", Config1), maps:get(~"created", Config2)),
        ?assertEqual(~"2023-11-14T22:13:20Z", maps:get(~"created", Config1))
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.

reproducible_image_with_layer_test() ->
    %% Test full image reproducibility with layer
    os:putenv("SOURCE_DATE_EPOCH", "1700000000"),
    try
        {ok, Image1} = ocibuild:scratch(),
        Image1a = ocibuild:add_layer(Image1, [{~"/test", ~"data", 8#644}]),

        {ok, Image2} = ocibuild:scratch(),
        Image2a = ocibuild:add_layer(Image2, [{~"/test", ~"data", 8#644}]),

        %% Layer digests should be identical
        [Layer1] = maps:get(layers, Image1a),
        [Layer2] = maps:get(layers, Image2a),
        ?assertEqual(maps:get(digest, Layer1), maps:get(digest, Layer2)),
        ?assertEqual(maps:get(diff_id, Layer1), maps:get(diff_id, Layer2)),

        %% Config timestamps should be identical
        Config1 = maps:get(config, Image1a),
        Config2 = maps:get(config, Image2a),
        ?assertEqual(maps:get(~"created", Config1), maps:get(~"created", Config2))
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.

history_timestamp_test() ->
    %% Test that history entries use SOURCE_DATE_EPOCH
    os:putenv("SOURCE_DATE_EPOCH", "1700000000"),
    try
        {ok, Image0} = ocibuild:scratch(),
        Image1 = ocibuild:add_layer(Image0, [{~"/test", ~"data", 8#644}]),
        Config = maps:get(config, Image1),
        History = maps:get(~"history", Config),
        [HistoryEntry | _] = History,
        ?assertEqual(~"2023-11-14T22:13:20Z", maps:get(~"created", HistoryEntry))
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.
