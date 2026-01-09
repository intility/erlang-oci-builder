%%%-------------------------------------------------------------------
-module(ocibuild_digest_tests).
-moduledoc """
Tests for ocibuild_digest module - SHA256 digest utilities for OCI content addressing.

Tests cover:
- SHA256 digest calculation in OCI format (sha256:<hex>)
- Hex encoding/decoding
- Digest parsing (algorithm and encoded hash extraction)
- Error handling for invalid digests
- Known test vectors
""".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% SHA256 Tests
%%%===================================================================

sha256_empty_data_test() ->
    %% SHA256 of empty string is well-known
    Digest = ocibuild_digest:sha256(<<>>),
    ?assertEqual(
        ~"sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        Digest
    ).

sha256_hello_test() ->
    %% SHA256 of "hello" is well-known
    Digest = ocibuild_digest:sha256(~"hello"),
    ?assertEqual(
        ~"sha256:2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
        Digest
    ).

sha256_hello_world_test() ->
    %% SHA256 of "Hello, World!"
    Digest = ocibuild_digest:sha256(~"Hello, World!"),
    ?assertEqual(
        ~"sha256:dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f",
        Digest
    ).

sha256_binary_data_test() ->
    %% Test with binary data containing null bytes
    Data = <<0, 1, 2, 3, 255, 254, 253>>,
    Digest = ocibuild_digest:sha256(Data),
    %% Should have correct format
    ?assertMatch(<<"sha256:", _:64/binary>>, Digest).

sha256_large_data_test() ->
    %% Test with larger data
    Data = binary:copy(~"test", 10000),
    Digest = ocibuild_digest:sha256(Data),
    ?assertMatch(<<"sha256:", _:64/binary>>, Digest),
    %% Same data should produce same digest
    Digest2 = ocibuild_digest:sha256(Data),
    ?assertEqual(Digest, Digest2).

sha256_format_test() ->
    %% Verify format: sha256: prefix + 64 hex chars
    Digest = ocibuild_digest:sha256(~"test"),
    ?assertMatch(<<"sha256:", Hash/binary>> when byte_size(Hash) =:= 64, Digest),
    %% Hash should be lowercase hex
    <<"sha256:", Hash/binary>> = Digest,
    ?assert(is_lowercase_hex(Hash)).

%%%===================================================================
%%% SHA256 Hex Tests
%%%===================================================================

sha256_hex_empty_test() ->
    %% Should return just the hex, no prefix
    Hex = ocibuild_digest:sha256_hex(<<>>),
    ?assertEqual(
        ~"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        Hex
    ).

sha256_hex_hello_test() ->
    Hex = ocibuild_digest:sha256_hex(~"hello"),
    ?assertEqual(
        ~"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
        Hex
    ).

sha256_hex_length_test() ->
    %% Should always be 64 characters (256 bits / 4 bits per hex char)
    Hex = ocibuild_digest:sha256_hex(~"any data"),
    ?assertEqual(64, byte_size(Hex)).

sha256_vs_sha256_hex_test() ->
    %% sha256 should be "sha256:" + sha256_hex
    Data = ~"test data",
    Digest = ocibuild_digest:sha256(Data),
    Hex = ocibuild_digest:sha256_hex(Data),
    Expected = <<"sha256:", Hex/binary>>,
    ?assertEqual(Expected, Digest).

%%%===================================================================
%%% Hex Encoding/Decoding Tests
%%%===================================================================

to_hex_empty_test() ->
    ?assertEqual(<<>>, ocibuild_digest:to_hex(<<>>)).

to_hex_single_byte_test() ->
    ?assertEqual(~"00", ocibuild_digest:to_hex(<<0>>)),
    ?assertEqual(~"ff", ocibuild_digest:to_hex(<<255>>)),
    ?assertEqual(~"0a", ocibuild_digest:to_hex(<<10>>)),
    ?assertEqual(~"10", ocibuild_digest:to_hex(<<16>>)).

to_hex_multiple_bytes_test() ->
    ?assertEqual(~"0102ff", ocibuild_digest:to_hex(<<1, 2, 255>>)),
    ?assertEqual(~"deadbeef", ocibuild_digest:to_hex(<<222, 173, 190, 239>>)).

to_hex_lowercase_test() ->
    %% Should always be lowercase
    Hex = ocibuild_digest:to_hex(<<171, 205, 239>>),
    ?assertEqual(~"abcdef", Hex).

from_hex_empty_test() ->
    ?assertEqual(<<>>, ocibuild_digest:from_hex(<<>>)).

from_hex_single_byte_test() ->
    ?assertEqual(<<0>>, ocibuild_digest:from_hex(~"00")),
    ?assertEqual(<<255>>, ocibuild_digest:from_hex(~"ff")),
    ?assertEqual(<<255>>, ocibuild_digest:from_hex(~"FF")),
    ?assertEqual(<<10>>, ocibuild_digest:from_hex(~"0a")).

from_hex_multiple_bytes_test() ->
    ?assertEqual(<<1, 2, 255>>, ocibuild_digest:from_hex(~"0102ff")),
    ?assertEqual(<<222, 173, 190, 239>>, ocibuild_digest:from_hex(~"DEADBEEF")).

from_hex_case_insensitive_test() ->
    ?assertEqual(
        ocibuild_digest:from_hex(~"abcdef"),
        ocibuild_digest:from_hex(~"ABCDEF")
    ).

hex_roundtrip_test() ->
    %% from_hex(to_hex(X)) == X
    Data = <<1, 2, 3, 100, 200, 255>>,
    ?assertEqual(Data, ocibuild_digest:from_hex(ocibuild_digest:to_hex(Data))).

hex_roundtrip_random_test() ->
    %% Test with random data
    Data = crypto:strong_rand_bytes(32),
    ?assertEqual(Data, ocibuild_digest:from_hex(ocibuild_digest:to_hex(Data))).

%%%===================================================================
%%% Algorithm Extraction Tests
%%%===================================================================

algorithm_sha256_test() ->
    ?assertEqual(~"sha256", ocibuild_digest:algorithm(~"sha256:abc123")).

algorithm_sha512_test() ->
    %% Should work with any algorithm prefix
    ?assertEqual(~"sha512", ocibuild_digest:algorithm(~"sha512:abc123")).

algorithm_custom_test() ->
    ?assertEqual(~"blake2b", ocibuild_digest:algorithm(~"blake2b:somehash")).

algorithm_real_digest_test() ->
    Digest = ocibuild_digest:sha256(~"test"),
    ?assertEqual(~"sha256", ocibuild_digest:algorithm(Digest)).

algorithm_invalid_no_colon_test() ->
    %% Should error on invalid digest (no colon)
    ?assertError({invalid_digest, ~"nocolon"}, ocibuild_digest:algorithm(~"nocolon")).

%%%===================================================================
%%% Encoded Hash Extraction Tests
%%%===================================================================

encoded_basic_test() ->
    ?assertEqual(~"abc123", ocibuild_digest:encoded(~"sha256:abc123")).

encoded_real_digest_test() ->
    Digest = ocibuild_digest:sha256(~"hello"),
    Encoded = ocibuild_digest:encoded(Digest),
    ?assertEqual(
        ~"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
        Encoded
    ).

encoded_with_colons_in_hash_test() ->
    %% Edge case: what if the hash itself contains colons?
    %% binary:split with default splits on first occurrence only
    Result = ocibuild_digest:encoded(~"sha256:abc:def:ghi"),
    ?assertEqual(~"abc:def:ghi", Result).

encoded_invalid_no_colon_test() ->
    ?assertError({invalid_digest, ~"nocolon"}, ocibuild_digest:encoded(~"nocolon")).

encoded_empty_hash_test() ->
    %% Edge case: empty hash after colon
    ?assertEqual(<<>>, ocibuild_digest:encoded(~"sha256:")).

%%%===================================================================
%%% Security Tests (Path Traversal Prevention Documentation)
%%%===================================================================

%% These tests document the security concern mentioned in the module docs.
%% The encoded/1 function does NOT validate - callers must validate!

encoded_path_traversal_awareness_test() ->
    %% This demonstrates the security concern documented in the module.
    %% A malicious digest could contain path traversal sequences.
    %% The encoded/1 function returns them as-is - NO VALIDATION!
    MaliciousDigest = ~"sha256:../../etc/passwd",
    Encoded = ocibuild_digest:encoded(MaliciousDigest),
    %% This would be dangerous if used directly in file paths!
    ?assertEqual(~"../../etc/passwd", Encoded).

valid_hex_digest_format_test() ->
    %% A properly formatted digest should have 64 hex chars
    Digest = ocibuild_digest:sha256(~"test"),
    Encoded = ocibuild_digest:encoded(Digest),
    ?assertEqual(64, byte_size(Encoded)),
    ?assert(is_lowercase_hex(Encoded)).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

oci_digest_workflow_test() ->
    %% Simulate typical OCI workflow:
    %% 1. Create content
    %% 2. Calculate digest
    %% 3. Extract parts for different uses
    Content = ~"{\"config\": {}}",

    %% Calculate digest (used in manifests)
    Digest = ocibuild_digest:sha256(Content),

    %% Extract algorithm (for Content-Type header)
    Alg = ocibuild_digest:algorithm(Digest),
    ?assertEqual(~"sha256", Alg),

    %% Extract encoded hash (for blob filenames)
    Hash = ocibuild_digest:encoded(Digest),
    ?assertEqual(64, byte_size(Hash)),

    %% Verify we can reconstruct
    Reconstructed = <<Alg/binary, ":", Hash/binary>>,
    ?assertEqual(Digest, Reconstructed).

digest_determinism_test() ->
    %% Same content should always produce same digest
    Content = ~"reproducible content",
    D1 = ocibuild_digest:sha256(Content),
    D2 = ocibuild_digest:sha256(Content),
    D3 = ocibuild_digest:sha256(Content),
    ?assertEqual(D1, D2),
    ?assertEqual(D2, D3).

different_content_different_digest_test() ->
    %% Different content should produce different digests
    D1 = ocibuild_digest:sha256(~"content1"),
    D2 = ocibuild_digest:sha256(~"content2"),
    ?assertNotEqual(D1, D2).

%%%===================================================================
%%% Helpers
%%%===================================================================

is_lowercase_hex(Bin) ->
    lists:all(
        fun(C) ->
            (C >= $0 andalso C =< $9) orelse
            (C >= $a andalso C =< $f)
        end,
        binary_to_list(Bin)
    ).
