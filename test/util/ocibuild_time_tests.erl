%%%-------------------------------------------------------------------
-module(ocibuild_time_tests).
-moduledoc """
Tests for ocibuild_time module - timestamp utilities for reproducible builds.

Tests cover:
- Unix timestamp to ISO8601 conversion
- SOURCE_DATE_EPOCH environment variable handling
- Edge cases (epoch, year boundaries, etc.)
- Invalid SOURCE_DATE_EPOCH values
""".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup/Teardown for SOURCE_DATE_EPOCH tests
%%%===================================================================

%% Helper to run a test with SOURCE_DATE_EPOCH set to a specific value
with_source_date_epoch(Value, Fun) ->
    %% Save original value
    Original = os:getenv("SOURCE_DATE_EPOCH"),
    try
        %% Set new value
        case Value of
            false -> os:unsetenv("SOURCE_DATE_EPOCH");
            _ -> os:putenv("SOURCE_DATE_EPOCH", Value)
        end,
        Fun()
    after
        %% Restore original
        case Original of
            false -> os:unsetenv("SOURCE_DATE_EPOCH");
            Val -> os:putenv("SOURCE_DATE_EPOCH", Val)
        end
    end.

%%%===================================================================
%%% unix_to_iso8601 Tests
%%%===================================================================

unix_to_iso8601_epoch_test() ->
    %% Unix epoch (1970-01-01 00:00:00 UTC)
    ?assertEqual(~"1970-01-01T00:00:00Z", ocibuild_time:unix_to_iso8601(0)).

unix_to_iso8601_known_timestamp_test() ->
    %% 2023-11-14T22:13:20Z = 1700000000
    ?assertEqual(~"2023-11-14T22:13:20Z", ocibuild_time:unix_to_iso8601(1700000000)).

unix_to_iso8601_y2k_test() ->
    %% Y2K: 2000-01-01T00:00:00Z = 946684800
    ?assertEqual(~"2000-01-01T00:00:00Z", ocibuild_time:unix_to_iso8601(946684800)).

unix_to_iso8601_leap_year_test() ->
    %% 2024-02-29T12:00:00Z (leap year) = 1709208000
    ?assertEqual(~"2024-02-29T12:00:00Z", ocibuild_time:unix_to_iso8601(1709208000)).

unix_to_iso8601_end_of_year_test() ->
    %% 2023-12-31T23:59:59Z = 1704067199
    ?assertEqual(~"2023-12-31T23:59:59Z", ocibuild_time:unix_to_iso8601(1704067199)).

unix_to_iso8601_start_of_year_test() ->
    %% 2024-01-01T00:00:00Z = 1704067200
    ?assertEqual(~"2024-01-01T00:00:00Z", ocibuild_time:unix_to_iso8601(1704067200)).

unix_to_iso8601_format_test() ->
    %% Verify format matches ISO8601 with Z suffix
    Result = ocibuild_time:unix_to_iso8601(1700000000),
    %% Format: YYYY-MM-DDTHH:MM:SSZ (20 characters)
    ?assertEqual(20, byte_size(Result)),
    %% Check structure
    ?assertMatch(
        <<_Y:4/binary, "-", _M:2/binary, "-", _D:2/binary,
          "T", _H:2/binary, ":", _Mi:2/binary, ":", _S:2/binary, "Z">>,
        Result
    ).

unix_to_iso8601_padding_test() ->
    %% Verify zero-padding for single digit values
    %% 2000-01-02T03:04:05Z = 946782245
    Result = ocibuild_time:unix_to_iso8601(946782245),
    ?assertEqual(~"2000-01-02T03:04:05Z", Result).

unix_to_iso8601_returns_binary_test() ->
    Result = ocibuild_time:unix_to_iso8601(0),
    ?assert(is_binary(Result)).

%%%===================================================================
%%% get_timestamp Tests (without SOURCE_DATE_EPOCH)
%%%===================================================================

get_timestamp_returns_integer_test() ->
    with_source_date_epoch(false, fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        ?assert(is_integer(Timestamp))
    end).

get_timestamp_reasonable_value_test() ->
    with_source_date_epoch(false, fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        %% Should be after 2020 (1577836800) and before 2100 (4102444800)
        ?assert(Timestamp > 1577836800),
        ?assert(Timestamp < 4102444800)
    end).

get_timestamp_monotonic_test() ->
    with_source_date_epoch(false, fun() ->
        T1 = ocibuild_time:get_timestamp(),
        timer:sleep(1100),  % Sleep just over 1 second
        T2 = ocibuild_time:get_timestamp(),
        ?assert(T2 >= T1)
    end).

%%%===================================================================
%%% get_timestamp Tests (with SOURCE_DATE_EPOCH)
%%%===================================================================

get_timestamp_uses_source_date_epoch_test() ->
    with_source_date_epoch("1700000000", fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        ?assertEqual(1700000000, Timestamp)
    end).

get_timestamp_source_date_epoch_zero_test() ->
    with_source_date_epoch("0", fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        ?assertEqual(0, Timestamp)
    end).

get_timestamp_source_date_epoch_large_test() ->
    with_source_date_epoch("4000000000", fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        ?assertEqual(4000000000, Timestamp)
    end).

%%%===================================================================
%%% get_timestamp Tests (invalid SOURCE_DATE_EPOCH)
%%%===================================================================

get_timestamp_invalid_epoch_non_numeric_test() ->
    with_source_date_epoch("not_a_number", fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        %% Should fall back to current time
        ?assert(is_integer(Timestamp)),
        ?assert(Timestamp > 1577836800)
    end).

get_timestamp_invalid_epoch_negative_test() ->
    with_source_date_epoch("-1", fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        %% Should fall back to current time (negative not allowed)
        ?assert(is_integer(Timestamp)),
        ?assert(Timestamp > 1577836800)
    end).

get_timestamp_invalid_epoch_float_test() ->
    with_source_date_epoch("1700000000.5", fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        %% Should fall back to current time (trailing chars)
        ?assert(is_integer(Timestamp)),
        ?assert(Timestamp > 1577836800)
    end).

get_timestamp_invalid_epoch_empty_test() ->
    with_source_date_epoch("", fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        %% Should fall back to current time
        ?assert(is_integer(Timestamp)),
        ?assert(Timestamp > 1577836800)
    end).

get_timestamp_invalid_epoch_whitespace_test() ->
    with_source_date_epoch(" 1700000000", fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        %% Leading whitespace - should fall back to current time
        ?assert(is_integer(Timestamp)),
        ?assert(Timestamp > 1577836800)
    end).

%%%===================================================================
%%% get_iso8601 Tests
%%%===================================================================

get_iso8601_returns_binary_test() ->
    with_source_date_epoch(false, fun() ->
        Result = ocibuild_time:get_iso8601(),
        ?assert(is_binary(Result))
    end).

get_iso8601_format_test() ->
    with_source_date_epoch(false, fun() ->
        Result = ocibuild_time:get_iso8601(),
        %% Should be 20 characters in ISO8601 format
        ?assertEqual(20, byte_size(Result)),
        ?assertMatch(
            <<_:4/binary, "-", _:2/binary, "-", _:2/binary,
              "T", _:2/binary, ":", _:2/binary, ":", _:2/binary, "Z">>,
            Result
        )
    end).

get_iso8601_uses_source_date_epoch_test() ->
    with_source_date_epoch("1700000000", fun() ->
        Result = ocibuild_time:get_iso8601(),
        ?assertEqual(~"2023-11-14T22:13:20Z", Result)
    end).

get_iso8601_epoch_zero_test() ->
    with_source_date_epoch("0", fun() ->
        Result = ocibuild_time:get_iso8601(),
        ?assertEqual(~"1970-01-01T00:00:00Z", Result)
    end).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

reproducible_build_workflow_test() ->
    %% Simulate reproducible build scenario
    with_source_date_epoch("1700000000", fun() ->
        %% All timestamps in the build should be the same
        T1 = ocibuild_time:get_timestamp(),
        T2 = ocibuild_time:get_timestamp(),
        T3 = ocibuild_time:get_timestamp(),
        ?assertEqual(T1, T2),
        ?assertEqual(T2, T3),
        ?assertEqual(1700000000, T1),

        %% ISO8601 should also be consistent
        ISO1 = ocibuild_time:get_iso8601(),
        ISO2 = ocibuild_time:get_iso8601(),
        ?assertEqual(ISO1, ISO2),
        ?assertEqual(~"2023-11-14T22:13:20Z", ISO1)
    end).

consistency_between_functions_test() ->
    with_source_date_epoch("1700000000", fun() ->
        Timestamp = ocibuild_time:get_timestamp(),
        FromGetIso = ocibuild_time:get_iso8601(),
        FromConvert = ocibuild_time:unix_to_iso8601(Timestamp),
        ?assertEqual(FromGetIso, FromConvert)
    end).
