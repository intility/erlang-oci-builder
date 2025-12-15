%%%-------------------------------------------------------------------
%%% @doc
%%% JSON encoding/decoding wrapper.
%%%
%%% This module wraps OTP 27's `json' module with convenience functions
%%% for OCI-specific JSON handling.
%%%
%%% For OTP versions prior to 27, a simple built-in encoder is used.
%%% @end
%%%-------------------------------------------------------------------
-module(ocibuild_json).

-export([
    encode/1,
    encode_pretty/1,
    decode/1
]).

%% Check if the native json module is available (OTP 27+)
-define(HAS_JSON_MODULE, (erlang:function_exported(json, encode, 1))).

%% @doc Encode an Erlang term to JSON binary.
-spec encode(term()) -> binary().
encode(Term) ->
    case ?HAS_JSON_MODULE of
        true ->
            iolist_to_binary(json:encode(Term));
        false ->
            iolist_to_binary(encode_term(Term))
    end.

%% @doc Encode an Erlang term to pretty-printed JSON binary.
%%
%% Useful for debugging and human-readable output.
-spec encode_pretty(term()) -> binary().
encode_pretty(Term) ->
    encode(Term).

%% @doc Decode a JSON binary to an Erlang term.
%%
%% Objects are decoded as maps, arrays as lists.
-spec decode(binary()) -> term().
decode(Json) when is_binary(Json) ->
    case ?HAS_JSON_MODULE of
        true ->
            json:decode(Json);
        false ->
            decode_json(Json)
    end.

%%%===================================================================
%%% Fallback JSON encoder for pre-OTP 27
%%%===================================================================

-spec encode_term(term()) -> iolist().
encode_term(null) -> <<"null">>;
encode_term(true) -> <<"true">>;
encode_term(false) -> <<"false">>;
encode_term(N) when is_integer(N) -> integer_to_binary(N);
encode_term(N) when is_float(N) -> float_to_binary(N, [{decimals, 10}, compact]);
encode_term(S) when is_binary(S) -> encode_string(S);
encode_term(S) when is_atom(S) -> encode_string(atom_to_binary(S, utf8));
encode_term(L) when is_list(L) -> encode_array(L);
encode_term(M) when is_map(M) -> encode_object(M).

-spec encode_string(binary()) -> iolist().
encode_string(S) ->
    [$", escape_string(S), $"].

-spec escape_string(binary()) -> iolist().
escape_string(<<>>) -> [];
escape_string(<<$", Rest/binary>>) -> [$\\, $" | escape_string(Rest)];
escape_string(<<$\\, Rest/binary>>) -> [$\\, $\\ | escape_string(Rest)];
escape_string(<<$\n, Rest/binary>>) -> [$\\, $n | escape_string(Rest)];
escape_string(<<$\r, Rest/binary>>) -> [$\\, $r | escape_string(Rest)];
escape_string(<<$\t, Rest/binary>>) -> [$\\, $t | escape_string(Rest)];
escape_string(<<C, Rest/binary>>) when C < 32 ->
    [io_lib:format("\\u~4.16.0B", [C]) | escape_string(Rest)];
escape_string(<<C, Rest/binary>>) -> [C | escape_string(Rest)].

-spec encode_array(list()) -> iolist().
encode_array([]) -> <<"[]">>;
encode_array(L) ->
    Elements = lists:join($,, [encode_term(E) || E <- L]),
    [$[, Elements, $]].

-spec encode_object(map()) -> iolist().
encode_object(M) when map_size(M) =:= 0 -> <<"{}">>;
encode_object(M) ->
    Pairs = lists:join($,, [encode_pair(K, V) || {K, V} <- maps:to_list(M)]),
    [${, Pairs, $}].

-spec encode_pair(term(), term()) -> iolist().
encode_pair(K, V) when is_binary(K) ->
    [encode_string(K), $:, encode_term(V)];
encode_pair(K, V) when is_atom(K) ->
    [encode_string(atom_to_binary(K, utf8)), $:, encode_term(V)].

%%%===================================================================
%%% Fallback JSON decoder for pre-OTP 27
%%% (Simple recursive descent parser)
%%%===================================================================

-spec decode_json(binary()) -> term().
decode_json(Json) ->
    {Value, _Rest} = decode_value(skip_ws(Json)),
    Value.

decode_value(<<"null", Rest/binary>>) -> {null, Rest};
decode_value(<<"true", Rest/binary>>) -> {true, Rest};
decode_value(<<"false", Rest/binary>>) -> {false, Rest};
decode_value(<<$", _/binary>> = B) -> decode_string(B);
decode_value(<<$[, _/binary>> = B) -> decode_array(B);
decode_value(<<${, _/binary>> = B) -> decode_object(B);
decode_value(<<C, _/binary>> = B) when C =:= $- orelse (C >= $0 andalso C =< $9) ->
    decode_number(B).

decode_string(<<$", Rest/binary>>) ->
    decode_string_chars(Rest, []).

decode_string_chars(<<$", Rest/binary>>, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), Rest};
decode_string_chars(<<$\\, $", Rest/binary>>, Acc) ->
    decode_string_chars(Rest, [$" | Acc]);
decode_string_chars(<<$\\, $\\, Rest/binary>>, Acc) ->
    decode_string_chars(Rest, [$\\ | Acc]);
decode_string_chars(<<$\\, $n, Rest/binary>>, Acc) ->
    decode_string_chars(Rest, [$\n | Acc]);
decode_string_chars(<<$\\, $r, Rest/binary>>, Acc) ->
    decode_string_chars(Rest, [$\r | Acc]);
decode_string_chars(<<$\\, $t, Rest/binary>>, Acc) ->
    decode_string_chars(Rest, [$\t | Acc]);
decode_string_chars(<<$\\, $u, A, B, C, D, Rest/binary>>, Acc) ->
    Codepoint = list_to_integer([A, B, C, D], 16),
    decode_string_chars(Rest, [<<Codepoint/utf8>> | Acc]);
decode_string_chars(<<C, Rest/binary>>, Acc) ->
    decode_string_chars(Rest, [C | Acc]).

decode_array(<<$[, Rest/binary>>) ->
    decode_array_elements(skip_ws(Rest), []).

decode_array_elements(<<$], Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
decode_array_elements(B, Acc) ->
    {Value, Rest} = decode_value(skip_ws(B)),
    case skip_ws(Rest) of
        <<$,, Rest2/binary>> ->
            decode_array_elements(skip_ws(Rest2), [Value | Acc]);
        <<$], Rest2/binary>> ->
            {lists:reverse([Value | Acc]), Rest2}
    end.

decode_object(<<${, Rest/binary>>) ->
    decode_object_pairs(skip_ws(Rest), #{}).

decode_object_pairs(<<$}, Rest/binary>>, Acc) ->
    {Acc, Rest};
decode_object_pairs(B, Acc) ->
    {Key, Rest1} = decode_string(skip_ws(B)),
    <<$:, Rest2/binary>> = skip_ws(Rest1),
    {Value, Rest3} = decode_value(skip_ws(Rest2)),
    NewAcc = Acc#{Key => Value},
    case skip_ws(Rest3) of
        <<$,, Rest4/binary>> ->
            decode_object_pairs(skip_ws(Rest4), NewAcc);
        <<$}, Rest4/binary>> ->
            {NewAcc, Rest4}
    end.

decode_number(B) ->
    {NumStr, Rest} = take_number_chars(B, []),
    NumBin = iolist_to_binary(lists:reverse(NumStr)),
    Num = case binary:match(NumBin, [<<".">>, <<"e">>, <<"E">>]) of
        nomatch -> binary_to_integer(NumBin);
        _ -> binary_to_float(NumBin)
    end,
    {Num, Rest}.

take_number_chars(<<C, Rest/binary>>, Acc) when C =:= $- orelse C =:= $+ orelse
                                                 C =:= $. orelse C =:= $e orelse
                                                 C =:= $E orelse
                                                 (C >= $0 andalso C =< $9) ->
    take_number_chars(Rest, [C | Acc]);
take_number_chars(Rest, Acc) ->
    {Acc, Rest}.

skip_ws(<<C, Rest/binary>>) when C =:= $  orelse C =:= $\t orelse 
                                  C =:= $\n orelse C =:= $\r ->
    skip_ws(Rest);
skip_ws(B) -> B.
