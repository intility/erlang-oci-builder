%%%-------------------------------------------------------------------
-module(ocibuild_sign_tests).
-moduledoc """
Tests for ocibuild_sign module - cosign-compatible image signing.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%%%===================================================================
%%% Artifact Type Tests
%%%===================================================================

artifact_type_test() ->
    %% Test artifact type is correct cosign simplesigning v1
    ?assertEqual(
        ~"application/vnd.dev.cosign.simplesigning.v1+json",
        ocibuild_sign:artifact_type()
    ).

%%%===================================================================
%%% Payload Building Tests
%%%===================================================================

build_payload_test() ->
    %% Test basic payload structure
    ManifestDigest = ~"sha256:abc123def456",
    DockerRef = ~"ghcr.io/myorg/myapp:v1.0.0",
    Payload = ocibuild_sign:build_payload(ManifestDigest, DockerRef),

    %% Check critical section exists
    Critical = maps:get(~"critical", Payload),
    ?assert(is_map(Critical)),

    %% Check identity
    Identity = maps:get(~"identity", Critical),
    ?assertEqual(DockerRef, maps:get(~"docker-reference", Identity)),

    %% Check image
    Image = maps:get(~"image", Critical),
    ?assertEqual(ManifestDigest, maps:get(~"docker-manifest-digest", Image)),

    %% Check type
    ?assertEqual(~"cosign container image signature", maps:get(~"type", Critical)),

    %% Check optional section exists and is empty
    Optional = maps:get(~"optional", Payload),
    ?assertEqual(#{}, Optional).

build_payload_special_chars_test() ->
    %% Test payload with special characters in reference
    ManifestDigest = ~"sha256:0123456789abcdef",
    DockerRef = ~"registry.example.com:5000/org/app-name:v1.2.3-beta+build.123",
    Payload = ocibuild_sign:build_payload(ManifestDigest, DockerRef),

    Critical = maps:get(~"critical", Payload),
    Identity = maps:get(~"identity", Critical),
    ?assertEqual(DockerRef, maps:get(~"docker-reference", Identity)).

%%%===================================================================
%%% Key Loading Tests
%%%===================================================================

load_key_nonexistent_test() ->
    %% Test loading non-existent key file
    Result = ocibuild_sign:load_key("/nonexistent/path/to/key.pem"),
    ?assertMatch({error, {key_read_failed, _, enoent}}, Result).

load_key_invalid_pem_test() ->
    %% Test loading invalid PEM content
    TmpPath = ocibuild_test_helpers:make_temp_file("invalid_key", ".pem"),
    ok = file:write_file(TmpPath, ~"not a valid pem file"),
    try
        Result = ocibuild_sign:load_key(TmpPath),
        ?assertMatch({error, _}, Result)
    after
        file:delete(TmpPath)
    end.

load_key_valid_ecdsa_p256_test() ->
    %% Test loading a valid ECDSA P-256 key
    %% Generate a test key in PEM format
    {_PubKey, PrivKey} = crypto:generate_key(ecdh, secp256r1),
    ECPrivateKey = #'ECPrivateKey'{
        version = 1,
        privateKey = PrivKey,
        parameters = {namedCurve, {1, 2, 840, 10045, 3, 1, 7}},
        publicKey = asn1_NOVALUE
    },
    PemEntry = public_key:pem_entry_encode('ECPrivateKey', ECPrivateKey),
    PemBin = public_key:pem_encode([PemEntry]),

    TmpPath = ocibuild_test_helpers:make_temp_file("valid_key", ".pem"),
    ok = file:write_file(TmpPath, PemBin),
    try
        Result = ocibuild_sign:load_key(TmpPath),
        ?assertMatch({ok, #'ECPrivateKey'{}}, Result)
    after
        file:delete(TmpPath)
    end.

%%%===================================================================
%%% Signing Tests
%%%===================================================================

sign_test() ->
    %% Test signing produces valid output
    {_PubKey, PrivKey} = crypto:generate_key(ecdh, secp256r1),
    ECPrivateKey = #'ECPrivateKey'{
        version = 1,
        privateKey = PrivKey,
        parameters = {namedCurve, {1, 2, 840, 10045, 3, 1, 7}},
        publicKey = asn1_NOVALUE
    },

    ManifestDigest = ~"sha256:abc123",
    DockerRef = ~"ghcr.io/myorg/myapp:v1",

    {ok, PayloadJson, Signature} = ocibuild_sign:sign(ManifestDigest, DockerRef, ECPrivateKey),

    %% Payload should be valid JSON
    ?assert(is_binary(PayloadJson)),
    Payload = ocibuild_json:decode(PayloadJson),
    ?assert(is_map(Payload)),

    %% Signature should be non-empty binary (DER-encoded ECDSA signature)
    ?assert(is_binary(Signature)),
    ?assert(byte_size(Signature) > 0),
    %% DER-encoded ECDSA P-256 signatures range 64-80 bytes, typically ~70-72
    ?assert(byte_size(Signature) >= 64),
    ?assert(byte_size(Signature) =< 80).

sign_payload_matches_test() ->
    %% Test that signed payload matches build_payload output
    {_PubKey, PrivKey} = crypto:generate_key(ecdh, secp256r1),
    ECPrivateKey = #'ECPrivateKey'{
        version = 1,
        privateKey = PrivKey,
        parameters = {namedCurve, {1, 2, 840, 10045, 3, 1, 7}},
        publicKey = asn1_NOVALUE
    },

    ManifestDigest = ~"sha256:abc123",
    DockerRef = ~"ghcr.io/myorg/myapp:v1",

    ExpectedPayload = ocibuild_sign:build_payload(ManifestDigest, DockerRef),
    {ok, PayloadJson, _Signature} = ocibuild_sign:sign(ManifestDigest, DockerRef, ECPrivateKey),

    ActualPayload = ocibuild_json:decode(PayloadJson),
    ?assertEqual(ExpectedPayload, ActualPayload).

%%%===================================================================
%%% Referrer Manifest Tests
%%%===================================================================

build_referrer_manifest_test() ->
    %% Test referrer manifest structure
    PayloadDigest = ~"sha256:payload123",
    PayloadSize = 256,
    Signature = <<"test-signature-bytes">>,
    SubjectDigest = ~"sha256:subject456",
    SubjectSize = 4096,

    Manifest = ocibuild_sign:build_referrer_manifest(
        PayloadDigest, PayloadSize, Signature, SubjectDigest, SubjectSize
    ),

    %% Check schema version
    ?assertEqual(2, maps:get(~"schemaVersion", Manifest)),

    %% Check media type
    ?assertEqual(
        ~"application/vnd.oci.image.manifest.v1+json",
        maps:get(~"mediaType", Manifest)
    ),

    %% Check artifact type
    ?assertEqual(
        ocibuild_sign:artifact_type(),
        maps:get(~"artifactType", Manifest)
    ),

    %% Check config blob
    Config = maps:get(~"config", Manifest),
    ?assertEqual(ocibuild_sign:artifact_type(), maps:get(~"mediaType", Config)),
    ?assertEqual(PayloadDigest, maps:get(~"digest", Config)),
    ?assertEqual(PayloadSize, maps:get(~"size", Config)),

    %% Check layers
    Layers = maps:get(~"layers", Manifest),
    ?assertEqual(1, length(Layers)),
    [Layer] = Layers,
    ?assertEqual(ocibuild_sign:artifact_type(), maps:get(~"mediaType", Layer)),
    ?assertEqual(PayloadDigest, maps:get(~"digest", Layer)),
    ?assertEqual(PayloadSize, maps:get(~"size", Layer)),

    %% Check signature annotation
    LayerAnnotations = maps:get(~"annotations", Layer),
    SignatureB64 = maps:get(~"dev.cosignproject.cosign/signature", LayerAnnotations),
    ?assertEqual(base64:encode(Signature), SignatureB64),

    %% Check subject reference
    Subject = maps:get(~"subject", Manifest),
    ?assertEqual(
        ~"application/vnd.oci.image.manifest.v1+json",
        maps:get(~"mediaType", Subject)
    ),
    ?assertEqual(SubjectDigest, maps:get(~"digest", Subject)),
    ?assertEqual(SubjectSize, maps:get(~"size", Subject)),

    %% Check annotations exist (created timestamp)
    Annotations = maps:get(~"annotations", Manifest),
    ?assert(maps:is_key(~"org.opencontainers.image.created", Annotations)).

build_referrer_manifest_signature_encoding_test() ->
    %% Test that signature is properly base64 encoded
    PayloadDigest = ~"sha256:abc",
    Signature = <<1, 2, 3, 4, 5, 255, 254, 253>>,  % Binary with non-printable bytes

    Manifest = ocibuild_sign:build_referrer_manifest(
        PayloadDigest, 100, Signature, ~"sha256:subject", 1000
    ),

    [Layer] = maps:get(~"layers", Manifest),
    LayerAnnotations = maps:get(~"annotations", Layer),
    SignatureB64 = maps:get(~"dev.cosignproject.cosign/signature", LayerAnnotations),

    %% Verify base64 encoding roundtrip
    DecodedSignature = base64:decode(SignatureB64),
    ?assertEqual(Signature, DecodedSignature).
