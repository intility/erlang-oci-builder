%%%-------------------------------------------------------------------
-module(ocibuild_digest).
-moduledoc """
Digest utilities for OCI content addressing.

OCI uses SHA256 digests in the format `sha256:<hex>` to identify
content-addressable blobs.
""".

-export([sha256/1, sha256_hex/1, from_hex/1, to_hex/1, algorithm/1, encoded/1]).

%% ~"sha256:abc123..."
-type digest() :: binary().

-export_type([digest/0]).

-doc """
Calculate SHA256 digest of data in OCI format.

Returns the digest in the standard OCI format: `sha256:<hex>`.
```
~"sha256:2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824" =
    ocibuild_digest:sha256(~"hello").
```
""".
-spec sha256(binary()) -> digest().
sha256(Data) when is_binary(Data) ->
    Hash = crypto:hash(sha256, Data),
    Hex = to_hex(Hash),
    <<"sha256:", Hex/binary>>.

-doc "Calculate raw SHA256 hash and return as hex string (without algorithm prefix).".
-spec sha256_hex(binary()) -> binary().
sha256_hex(Data) when is_binary(Data) ->
    Hash = crypto:hash(sha256, Data),
    to_hex(Hash).

-doc "Parse a hex string to binary.".
-spec from_hex(binary()) -> binary().
from_hex(Hex) when is_binary(Hex) ->
    binary:decode_hex(Hex).

-doc "Convert binary to lowercase hex string.".
-spec to_hex(binary()) -> binary().
to_hex(Bin) when is_binary(Bin) ->
    string:lowercase(
        binary:encode_hex(Bin)
    ).

-doc """
Extract the algorithm from a digest.

```
~"sha256" = ocibuild_digest:algorithm(~"sha256:abc123").
```
""".
-spec algorithm(digest()) -> binary().
algorithm(Digest) when is_binary(Digest) ->
    case binary:split(Digest, ~":") of
        [Alg, _] ->
            Alg;
        _ ->
            error({invalid_digest, Digest})
    end.

-doc """
Extract the encoded hash from a digest.

```
~"abc123" = ocibuild_digest:encoded(~"sha256:abc123").
```
""".
-spec encoded(digest()) -> binary().
encoded(Digest) when is_binary(Digest) ->
    case binary:split(Digest, ~":") of
        [_, Encoded] ->
            Encoded;
        _ ->
            error({invalid_digest, Digest})
    end.
