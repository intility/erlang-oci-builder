%%%-------------------------------------------------------------------
%%% @doc
%%% Basic tests for Shipwright
%%% @end
%%%-------------------------------------------------------------------
-module(ocibuild_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Digest tests
%%%===================================================================

sha256_test() ->
    Expected = <<"sha256:2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824">>,
    ?assertEqual(Expected, ocibuild_digest:sha256(<<"hello">>)).

digest_parts_test() ->
    Digest = <<"sha256:abc123">>,
    ?assertEqual(<<"sha256">>, ocibuild_digest:algorithm(Digest)),
    ?assertEqual(<<"abc123">>, ocibuild_digest:encoded(Digest)).

%%%===================================================================
%%% JSON tests
%%%===================================================================

json_encode_string_test() ->
    ?assertEqual(<<"\"hello\"">>, ocibuild_json:encode(<<"hello">>)).

json_encode_number_test() ->
    ?assertEqual(<<"42">>, ocibuild_json:encode(42)).

json_encode_bool_test() ->
    ?assertEqual(<<"true">>, ocibuild_json:encode(true)),
    ?assertEqual(<<"false">>, ocibuild_json:encode(false)).

json_encode_null_test() ->
    ?assertEqual(<<"null">>, ocibuild_json:encode(null)).

json_encode_array_test() ->
    ?assertEqual(<<"[1,2,3]">>, ocibuild_json:encode([1,2,3])).

json_encode_object_test() ->
    %% Note: map ordering is not guaranteed, so we decode and compare
    Json = ocibuild_json:encode(#{<<"a">> => 1, <<"b">> => 2}),
    Decoded = ocibuild_json:decode(Json),
    ?assertEqual(#{<<"a">> => 1, <<"b">> => 2}, Decoded).

json_decode_test() ->
    ?assertEqual(#{<<"key">> => <<"value">>}, 
                 ocibuild_json:decode(<<"{\"key\":\"value\"}">>)).

%%%===================================================================
%%% Tar tests
%%%===================================================================

tar_basic_test() ->
    Files = [
        {<<"/hello.txt">>, <<"Hello, World!">>, 8#644}
    ],
    Tar = ocibuild_tar:create(Files),
    %% TAR should be a multiple of 512 bytes
    ?assertEqual(0, byte_size(Tar) rem 512),
    %% Should contain at least the file content
    ?assert(binary:match(Tar, <<"Hello, World!">>) =/= nomatch).

tar_compressed_test() ->
    Files = [
        {<<"/test.txt">>, <<"test content">>, 8#644}
    ],
    Compressed = ocibuild_tar:create_compressed(Files),
    %% Should start with gzip magic bytes
    <<16#1f, 16#8b, _/binary>> = Compressed,
    ok.

%%%===================================================================
%%% Layer tests
%%%===================================================================

layer_create_test() ->
    Files = [
        {<<"/app/test">>, <<"test data">>, 8#755}
    ],
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
    ?assertEqual(<<"application/vnd.oci.image.layer.v1.tar+gzip">>, 
                 maps:get(media_type, Layer)).

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
    Image1 = ocibuild:entrypoint(Image0, [<<"/app">>]),
    Image2 = ocibuild:cmd(Image1, [<<"--port">>, <<"8080">>]),
    Image3 = ocibuild:env(Image2, #{<<"FOO">> => <<"bar">>}),
    Image4 = ocibuild:workdir(Image3, <<"/app">>),
    Image5 = ocibuild:expose(Image4, 8080),
    Image6 = ocibuild:user(Image5, <<"nobody">>),
    
    Config = maps:get(config, Image6),
    InnerConfig = maps:get(<<"config">>, Config),
    
    ?assertEqual([<<"/app">>], maps:get(<<"Entrypoint">>, InnerConfig)),
    ?assertEqual([<<"--port">>, <<"8080">>], maps:get(<<"Cmd">>, InnerConfig)),
    ?assertEqual(<<"/app">>, maps:get(<<"WorkingDir">>, InnerConfig)),
    ?assertEqual(<<"nobody">>, maps:get(<<"User">>, InnerConfig)).

add_layer_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [
        {<<"/test.txt">>, <<"hello">>, 8#644}
    ]),
    
    Layers = maps:get(layers, Image1),
    ?assertEqual(1, length(Layers)).

%%%===================================================================
%%% Layout tests
%%%===================================================================

export_directory_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [
        {<<"/test.txt">>, <<"hello">>, 8#644}
    ]),
    Image2 = ocibuild:entrypoint(Image1, [<<"/bin/sh">>]),
    
    TmpDir = "/tmp/ocibuild_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = ocibuild:export(Image2, TmpDir),
    
    %% Check required files exist
    ?assert(filelib:is_file(filename:join(TmpDir, "oci-layout"))),
    ?assert(filelib:is_file(filename:join(TmpDir, "index.json"))),
    ?assert(filelib:is_dir(filename:join(TmpDir, "blobs/sha256"))),
    
    %% Cleanup
    os:cmd("rm -rf " ++ TmpDir),
    ok.

save_tarball_test() ->
    {ok, Image0} = ocibuild:scratch(),
    Image1 = ocibuild:add_layer(Image0, [
        {<<"/test.txt">>, <<"hello">>, 8#644}
    ]),
    
    TmpFile = "/tmp/ocibuild_test_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".tar.gz",
    ok = ocibuild:save(Image1, TmpFile),
    
    %% Check file exists and is gzipped
    ?assert(filelib:is_file(TmpFile)),
    {ok, Data} = file:read_file(TmpFile),
    <<16#1f, 16#8b, _/binary>> = Data,
    
    %% Cleanup
    file:delete(TmpFile),
    ok.
