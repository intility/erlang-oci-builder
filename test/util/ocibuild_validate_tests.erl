-module(ocibuild_validate_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% no_null_bytes/1 tests
%%====================================================================

no_null_bytes_ok_test() ->
    ?assertEqual(ok, ocibuild_validate:no_null_bytes(~"safe/path")).

no_null_bytes_ok_empty_test() ->
    ?assertEqual(ok, ocibuild_validate:no_null_bytes(<<>>)).

no_null_bytes_ok_with_special_chars_test() ->
    ?assertEqual(ok, ocibuild_validate:no_null_bytes(~"path/with spaces/and-dashes")).

no_null_bytes_error_test() ->
    ?assertEqual(
        {error, {null_byte, <<"has", 0, "null">>}},
        ocibuild_validate:no_null_bytes(<<"has", 0, "null">>)
    ).

no_null_bytes_error_at_start_test() ->
    ?assertEqual(
        {error, {null_byte, <<0, "start">>}},
        ocibuild_validate:no_null_bytes(<<0, "start">>)
    ).

no_null_bytes_error_at_end_test() ->
    ?assertEqual(
        {error, {null_byte, <<"end", 0>>}},
        ocibuild_validate:no_null_bytes(<<"end", 0>>)
    ).

%%====================================================================
%% no_traversal/1 tests (default separator)
%%====================================================================

no_traversal_ok_test() ->
    ?assertEqual(ok, ocibuild_validate:no_traversal(~"safe/path/here")).

no_traversal_ok_single_dot_test() ->
    ?assertEqual(ok, ocibuild_validate:no_traversal(~"./relative/path")).

no_traversal_ok_dots_in_name_test() ->
    ?assertEqual(ok, ocibuild_validate:no_traversal(~"file..name/path")).

no_traversal_error_test() ->
    ?assertEqual(
        {error, {path_traversal, ~"../etc/passwd"}},
        ocibuild_validate:no_traversal(~"../etc/passwd")
    ).

no_traversal_error_middle_test() ->
    ?assertEqual(
        {error, {path_traversal, ~"safe/../bad"}},
        ocibuild_validate:no_traversal(~"safe/../bad")
    ).

no_traversal_error_end_test() ->
    ?assertEqual(
        {error, {path_traversal, ~"safe/path/.."}},
        ocibuild_validate:no_traversal(~"safe/path/..")
    ).

%%====================================================================
%% no_traversal/2 tests (custom separators)
%%====================================================================

no_traversal_custom_separator_ok_test() ->
    ?assertEqual(ok, ocibuild_validate:no_traversal(~"safe:path", [~":"])).

no_traversal_custom_separator_error_test() ->
    ?assertEqual(
        {error, {path_traversal, ~"safe:..:bad"}},
        ocibuild_validate:no_traversal(~"safe:..:bad", [~":"])
    ).

no_traversal_multiple_separators_ok_test() ->
    ?assertEqual(ok, ocibuild_validate:no_traversal(~"safe/path:here", [~"/", ~":"])).

no_traversal_multiple_separators_error_slash_test() ->
    ?assertEqual(
        {error, {path_traversal, ~"safe/../path:here"}},
        ocibuild_validate:no_traversal(~"safe/../path:here", [~"/", ~":"])
    ).

no_traversal_multiple_separators_error_colon_test() ->
    ?assertEqual(
        {error, {path_traversal, ~"safe/path:..:bad"}},
        ocibuild_validate:no_traversal(~"safe/path:..:bad", [~"/", ~":"])
    ).

%%====================================================================
%% no_absolute/1 tests
%%====================================================================

no_absolute_ok_relative_test() ->
    ?assertEqual(ok, ocibuild_validate:no_absolute(~"relative/path")).

no_absolute_ok_dot_relative_test() ->
    ?assertEqual(ok, ocibuild_validate:no_absolute(~"./relative")).

no_absolute_ok_empty_test() ->
    ?assertEqual(ok, ocibuild_validate:no_absolute(<<>>)).

no_absolute_error_test() ->
    ?assertEqual(
        {error, {absolute_path, ~"/absolute/path"}},
        ocibuild_validate:no_absolute(~"/absolute/path")
    ).

no_absolute_error_root_test() ->
    ?assertEqual(
        {error, {absolute_path, ~"/"}},
        ocibuild_validate:no_absolute(~"/")
    ).

%%====================================================================
%% not_empty/1 tests
%%====================================================================

not_empty_ok_test() ->
    ?assertEqual(ok, ocibuild_validate:not_empty(~"content")).

not_empty_ok_single_char_test() ->
    ?assertEqual(ok, ocibuild_validate:not_empty(~"x")).

not_empty_error_test() ->
    ?assertEqual({error, empty_value}, ocibuild_validate:not_empty(<<>>)).

%%====================================================================
%% validate_user_string/1 tests
%%====================================================================

validate_user_string_ok_simple_test() ->
    ?assertEqual(ok, ocibuild_validate:validate_user_string(~"myapp:v1.0.0")).

validate_user_string_ok_annotation_key_test() ->
    ?assertEqual(ok, ocibuild_validate:validate_user_string(~"org.opencontainers.image.vendor")).

validate_user_string_ok_annotation_value_test() ->
    ?assertEqual(ok, ocibuild_validate:validate_user_string(~"Intility AS")).

validate_user_string_ok_url_test() ->
    ?assertEqual(ok, ocibuild_validate:validate_user_string(~"https://github.com/intility/ocibuild")).

validate_user_string_error_null_byte_test() ->
    ?assertEqual(
        {error, {null_byte, <<"bad", 0, "string">>}},
        ocibuild_validate:validate_user_string(<<"bad", 0, "string">>)
    ).

validate_user_string_error_path_traversal_slash_test() ->
    ?assertEqual(
        {error, {path_traversal, ~"../etc/passwd"}},
        ocibuild_validate:validate_user_string(~"../etc/passwd")
    ).

validate_user_string_error_path_traversal_colon_test() ->
    ?assertEqual(
        {error, {path_traversal, ~"safe:..:bad"}},
        ocibuild_validate:validate_user_string(~"safe:..:bad")
    ).

%% Null byte check happens before traversal check
validate_user_string_error_null_byte_priority_test() ->
    %% If both null byte and traversal, null byte error is returned first
    ?assertMatch(
        {error, {null_byte, _}},
        ocibuild_validate:validate_user_string(<<"../", 0, "bad">>)
    ).
