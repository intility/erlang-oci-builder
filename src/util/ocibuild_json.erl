%%%-------------------------------------------------------------------
-module(ocibuild_json).
-moduledoc """
JSON encoding/decoding wrapper.

This module wraps OTP 27's `json` module with convenience functions
for OCI-specific JSON handling.
""".

-export([encode/1, decode/1]).

-doc "Encode an Erlang term to JSON binary.".
-spec encode(term()) -> binary().
encode(Term) ->
    iolist_to_binary(json:encode(Term)).

-doc "Decode a JSON binary to an Erlang term. Objects are decoded as maps, arrays as lists.".
-spec decode(binary()) -> term().
decode(Json) when is_binary(Json) ->
    json:decode(Json).
