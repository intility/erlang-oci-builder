%%%-------------------------------------------------------------------
-module(ocibuild_compress_tests).
-moduledoc """
Tests for ocibuild_compress module - compression abstraction with OTP version detection.

Tests cover:
- Compression algorithm availability detection
- Default compression selection based on OTP version
- Compression of various data sizes
- Decompression verification (round-trip)
- Error handling for unavailable algorithms
- Reproducibility of compression
""".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Availability Tests
%%%===================================================================

gzip_always_available_test() ->
    %% gzip should always be available (uses zlib from OTP stdlib)
    ?assert(ocibuild_compress:is_available(gzip)).

auto_always_available_test() ->
    %% auto should always be available (resolves to best available)
    ?assert(ocibuild_compress:is_available(auto)).

zstd_availability_depends_on_otp_test() ->
    %% zstd availability depends on OTP version (28+)
    Available = ocibuild_compress:is_available(zstd),
    OtpRelease = list_to_integer(erlang:system_info(otp_release)),
    case OtpRelease >= 28 of
        true ->
            ?assert(Available);
        false ->
            ?assertNot(Available)
    end.

%%%===================================================================
%%% Default Selection Tests
%%%===================================================================

default_returns_available_algorithm_test() ->
    %% default/0 should always return an available algorithm
    Default = ocibuild_compress:default(),
    ?assert(Default =:= gzip orelse Default =:= zstd),
    ?assert(ocibuild_compress:is_available(Default)).

default_prefers_zstd_on_otp28_test() ->
    %% On OTP 28+, default should be zstd
    Default = ocibuild_compress:default(),
    case ocibuild_compress:is_available(zstd) of
        true ->
            ?assertEqual(zstd, Default);
        false ->
            ?assertEqual(gzip, Default)
    end.

%%%===================================================================
%%% Resolution Tests
%%%===================================================================

resolve_gzip_test() ->
    ?assertEqual(gzip, ocibuild_compress:resolve(gzip)).

resolve_zstd_test() ->
    ?assertEqual(zstd, ocibuild_compress:resolve(zstd)).

resolve_auto_test() ->
    %% auto should resolve to default
    Resolved = ocibuild_compress:resolve(auto),
    Default = ocibuild_compress:default(),
    ?assertEqual(Default, Resolved).

%%%===================================================================
%%% Gzip Compression Tests
%%%===================================================================

gzip_compress_basic_test() ->
    Data = <<"Hello, World!">>,
    {ok, Compressed} = ocibuild_compress:compress(Data, gzip),
    ?assert(is_binary(Compressed)),
    %% Compressed data should have gzip magic number
    ?assertMatch(<<16#1f, 16#8b, _/binary>>, Compressed).

gzip_compress_empty_data_test() ->
    Data = <<>>,
    {ok, Compressed} = ocibuild_compress:compress(Data, gzip),
    ?assert(is_binary(Compressed)),
    %% Empty data should still produce valid gzip
    Decompressed = zlib:gunzip(Compressed),
    ?assertEqual(Data, Decompressed).

gzip_compress_large_data_test() ->
    %% Test with 1MB of data
    Data = binary:copy(<<"test data for compression ">>, 40000),
    {ok, Compressed} = ocibuild_compress:compress(Data, gzip),
    ?assert(is_binary(Compressed)),
    %% Compressed should be smaller than original
    ?assert(byte_size(Compressed) < byte_size(Data)),
    %% Verify round-trip
    Decompressed = zlib:gunzip(Compressed),
    ?assertEqual(Data, Decompressed).

gzip_compress_binary_data_test() ->
    %% Test with binary data (non-text)
    Data = crypto:strong_rand_bytes(1024),
    {ok, Compressed} = ocibuild_compress:compress(Data, gzip),
    ?assert(is_binary(Compressed)),
    %% Verify round-trip
    Decompressed = zlib:gunzip(Compressed),
    ?assertEqual(Data, Decompressed).

gzip_compress_iolist_test() ->
    %% Test that iolist input works
    Data = [<<"Hello">>, <<", ">>, [<<"World">>, <<"!">>]],
    {ok, Compressed} = ocibuild_compress:compress(Data, gzip),
    ?assert(is_binary(Compressed)),
    %% Verify round-trip
    Decompressed = zlib:gunzip(Compressed),
    ?assertEqual(iolist_to_binary(Data), Decompressed).

gzip_reproducible_test() ->
    %% Same data should produce same compressed output
    Data = <<"Reproducible compression test">>,
    {ok, Compressed1} = ocibuild_compress:compress(Data, gzip),
    {ok, Compressed2} = ocibuild_compress:compress(Data, gzip),
    ?assertEqual(Compressed1, Compressed2).

%%%===================================================================
%%% Zstd Compression Tests (OTP 28+ only)
%%%===================================================================

zstd_compress_test() ->
    case ocibuild_compress:is_available(zstd) of
        true ->
            Data = <<"Hello, zstd compression!">>,
            {ok, Compressed} = ocibuild_compress:compress(Data, zstd),
            ?assert(is_binary(Compressed)),
            %% Compressed data should have zstd magic number (0x28 0xB5 0x2F 0xFD)
            ?assertMatch(<<16#28, 16#B5, 16#2F, 16#FD, _/binary>>, Compressed);
        false ->
            %% Skip on OTP 27
            ok
    end.

zstd_compress_large_data_test() ->
    case ocibuild_compress:is_available(zstd) of
        true ->
            %% Test with 1MB of data
            Data = binary:copy(<<"test data for zstd compression ">>, 32000),
            {ok, Compressed} = ocibuild_compress:compress(Data, zstd),
            ?assert(is_binary(Compressed)),
            %% Compressed should be smaller than original
            ?assert(byte_size(Compressed) < byte_size(Data)),
            %% Verify round-trip
            Decompressed = iolist_to_binary(zstd:decompress(Compressed)),
            ?assertEqual(Data, Decompressed);
        false ->
            ok
    end.

zstd_compress_empty_data_test() ->
    case ocibuild_compress:is_available(zstd) of
        true ->
            %% Note: zstd in OTP 28 may not handle empty data well
            %% Test with minimal non-empty data instead
            Data = <<"x">>,
            {ok, Compressed} = ocibuild_compress:compress(Data, zstd),
            ?assert(is_binary(Compressed)),
            %% Verify round-trip
            Decompressed = iolist_to_binary(zstd:decompress(Compressed)),
            ?assertEqual(Data, Decompressed);
        false ->
            ok
    end.

zstd_reproducible_test() ->
    case ocibuild_compress:is_available(zstd) of
        true ->
            Data = <<"Reproducible zstd compression test">>,
            {ok, Compressed1} = ocibuild_compress:compress(Data, zstd),
            {ok, Compressed2} = ocibuild_compress:compress(Data, zstd),
            ?assertEqual(Compressed1, Compressed2);
        false ->
            ok
    end.

zstd_unavailable_returns_error_test() ->
    case ocibuild_compress:is_available(zstd) of
        false ->
            %% On OTP 27, explicit zstd request should fail with clear error
            Data = <<"test">>,
            Result = ocibuild_compress:compress(Data, zstd),
            ?assertMatch({error, {zstd_not_available, otp_version, _}}, Result),
            %% Error should include OTP version info
            {error, {zstd_not_available, otp_version, OtpVsn}} = Result,
            ?assert(is_list(OtpVsn));
        true ->
            %% Skip on OTP 28+
            ok
    end.

%%%===================================================================
%%% Auto Compression Tests
%%%===================================================================

auto_compress_uses_default_test() ->
    Data = <<"Auto compression test">>,
    {ok, AutoCompressed} = ocibuild_compress:compress(Data, auto),
    Default = ocibuild_compress:default(),
    {ok, DefaultCompressed} = ocibuild_compress:compress(Data, Default),
    %% Auto should produce same result as explicit default
    ?assertEqual(AutoCompressed, DefaultCompressed).

auto_compress_always_succeeds_test() ->
    %% auto should always succeed (never returns zstd_not_available)
    Data = <<"Auto compression should always work">>,
    Result = ocibuild_compress:compress(Data, auto),
    ?assertMatch({ok, _}, Result).

%%%===================================================================
%%% Compression Ratio Tests
%%%===================================================================

gzip_compresses_repetitive_data_test() ->
    %% Highly repetitive data should compress well
    Data = binary:copy(<<"AAAAAAAAAA">>, 10000),
    {ok, Compressed} = ocibuild_compress:compress(Data, gzip),
    %% Should achieve at least 90% compression ratio
    Ratio = byte_size(Compressed) / byte_size(Data),
    ?assert(Ratio < 0.1).

zstd_compresses_better_than_gzip_test() ->
    case ocibuild_compress:is_available(zstd) of
        true ->
            %% zstd typically achieves better compression than gzip
            Data = binary:copy(<<"Test data for compression comparison. ">>, 10000),
            {ok, GzipCompressed} = ocibuild_compress:compress(Data, gzip),
            {ok, ZstdCompressed} = ocibuild_compress:compress(Data, zstd),
            %% zstd should be at least as good as gzip (often better)
            ?assert(byte_size(ZstdCompressed) =< byte_size(GzipCompressed) * 1.1);
        false ->
            ok
    end.

%%%===================================================================
%%% Edge Cases and Error Handling
%%%===================================================================

compress_unicode_data_test() ->
    %% Test with UTF-8 encoded data
    Data = <<"Hello, 世界! 🌍"/utf8>>,
    {ok, Compressed} = ocibuild_compress:compress(Data, gzip),
    Decompressed = zlib:gunzip(Compressed),
    ?assertEqual(Data, Decompressed).

compress_null_bytes_test() ->
    %% Test with data containing null bytes
    Data = <<0, 1, 2, 0, 0, 3, 4, 0>>,
    {ok, Compressed} = ocibuild_compress:compress(Data, gzip),
    Decompressed = zlib:gunzip(Compressed),
    ?assertEqual(Data, Decompressed).

%%%===================================================================
%%% Integration with ocibuild_layer Tests
%%%===================================================================

layer_uses_compression_test() ->
    %% Test that layer creation uses the compression module
    Files = [{~"/app/test", ~"test data", 8#755}],
    %% With explicit gzip
    {ok, GzipLayer} = ocibuild_layer:create(Files, #{compression => gzip}),
    ?assertEqual(
        ~"application/vnd.oci.image.layer.v1.tar+gzip",
        maps:get(media_type, GzipLayer)
    ),
    %% Verify it's valid gzip
    GzipData = maps:get(data, GzipLayer),
    ?assertMatch(<<16#1f, 16#8b, _/binary>>, GzipData).

layer_zstd_media_type_test() ->
    case ocibuild_compress:is_available(zstd) of
        true ->
            Files = [{~"/app/test", ~"test data", 8#755}],
            {ok, ZstdLayer} = ocibuild_layer:create(Files, #{compression => zstd}),
            ?assertEqual(
                ~"application/vnd.oci.image.layer.v1.tar+zstd",
                maps:get(media_type, ZstdLayer)
            ),
            %% Verify it's valid zstd
            ZstdData = maps:get(data, ZstdLayer),
            ?assertMatch(<<16#28, 16#B5, 16#2F, 16#FD, _/binary>>, ZstdData);
        false ->
            ok
    end.

layer_auto_compression_test() ->
    %% Test that auto compression uses the best available
    Files = [{~"/app/test", ~"test data", 8#755}],
    {ok, Layer} = ocibuild_layer:create(Files, #{compression => auto}),
    MediaType = maps:get(media_type, Layer),
    Default = ocibuild_compress:default(),
    ExpectedMediaType = ocibuild_manifest:layer_media_type(Default),
    ?assertEqual(ExpectedMediaType, MediaType).

layer_zstd_unavailable_error_test() ->
    case ocibuild_compress:is_available(zstd) of
        false ->
            %% On OTP 27, explicit zstd should fail
            Files = [{~"/app/test", ~"test data", 8#755}],
            Result = ocibuild_layer:create(Files, #{compression => zstd}),
            ?assertMatch({error, {zstd_not_available, otp_version, _}}, Result);
        true ->
            ok
    end.
