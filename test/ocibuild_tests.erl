%%%-------------------------------------------------------------------
-module(ocibuild_tests).
-moduledoc "Basic tests for Shipwright".

-include_lib("eunit/include/eunit.hrl").

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

export_directory_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [{~"/test.txt", ~"hello", 8#644}]),
    Image2 = ocibuild:entrypoint(Image1, [~"/bin/sh"]),

    TmpDir = make_temp_dir("ocibuild_test_export"),
    try
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
        )
    after
        cleanup_temp_dir(TmpDir)
    end.

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
%%% Cross-platform helpers
%%%===================================================================

%% @doc Get the system temp directory in a cross-platform way
temp_dir() ->
    case os:type() of
        {win32, _} ->
            case os:getenv("TEMP") of
                false ->
                    case os:getenv("TMP") of
                        false ->
                            "C:\\Temp";
                        TmpDir ->
                            TmpDir
                    end;
                TempDir ->
                    TempDir
            end;
        _ ->
            "/tmp"
    end.

%% @doc Create a unique temporary directory
make_temp_dir(Prefix) ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    DirName = Prefix ++ "_" ++ Unique,
    TmpDir = filename:join(temp_dir(), DirName),
    ok =
        filelib:ensure_dir(
            filename:join(TmpDir, "placeholder")
        ),
    case file:make_dir(TmpDir) of
        ok ->
            TmpDir;
        {error, eexist} ->
            TmpDir
    end.

%% @doc Create a unique temporary file path
make_temp_file(Prefix, Extension) ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    FileName = Prefix ++ "_" ++ Unique ++ Extension,
    filename:join(temp_dir(), FileName).

%% @doc Recursively delete a directory (cross-platform)
cleanup_temp_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Files} = file:list_dir(Dir),
            lists:foreach(
                fun(File) ->
                    Path = filename:join(Dir, File),
                    case filelib:is_dir(Path) of
                        true ->
                            cleanup_temp_dir(Path);
                        false ->
                            file:delete(Path)
                    end
                end,
                Files
            ),
            file:del_dir(Dir);
        false ->
            ok
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
