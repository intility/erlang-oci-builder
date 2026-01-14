%%%-------------------------------------------------------------------
-module(ocibuild_time).
-moduledoc """
Central timestamp utilities for reproducible builds.

This module provides timestamp functions that respect the SOURCE_DATE_EPOCH
environment variable for reproducible builds. When SOURCE_DATE_EPOCH is set,
all timestamps will use that value instead of the current time.

See: https://reproducible-builds.org/docs/source-date-epoch/

Usage:
```
%% Get Unix timestamp (uses SOURCE_DATE_EPOCH if set)
Timestamp = ocibuild_time:get_timestamp().

%% Get ISO8601 formatted timestamp
ISO8601 = ocibuild_time:get_iso8601().

%% Convert Unix timestamp to ISO8601
ISO8601 = ocibuild_time:unix_to_iso8601(1700000000).
```
""".

-export([get_timestamp/0, get_iso8601/0, unix_to_iso8601/1]).
-export([parse_rfc3339/1, normalize_rfc3339/1]).

%% Offset between Erlang's gregorian calendar epoch (year 0) and Unix epoch (1970)
-define(UNIX_EPOCH_OFFSET, 62167219200).

-doc """
Get timestamp: SOURCE_DATE_EPOCH if set, else current time.

Returns Unix timestamp (seconds since 1970-01-01 00:00:00 UTC).
""".
-spec get_timestamp() -> non_neg_integer().
get_timestamp() ->
    case os:getenv("SOURCE_DATE_EPOCH") of
        false ->
            erlang:system_time(second);
        EpochStr ->
            case string:to_integer(EpochStr) of
                {Int, []} when Int >= 0 -> Int;
                _ -> erlang:system_time(second)
            end
    end.

-doc """
Get ISO8601 timestamp using SOURCE_DATE_EPOCH if available.

Returns timestamp in format: "2024-01-15T12:30:45Z"
""".
-spec get_iso8601() -> binary().
get_iso8601() ->
    unix_to_iso8601(get_timestamp()).

-doc """
Convert Unix timestamp to ISO8601 format.

Example:
```
ocibuild_time:unix_to_iso8601(0).
%% => ~"1970-01-01T00:00:00Z"

ocibuild_time:unix_to_iso8601(1700000000).
%% => ~"2023-11-14T22:13:20Z"
```
""".
-spec unix_to_iso8601(non_neg_integer()) -> binary().
unix_to_iso8601(Timestamp) ->
    GregorianSeconds = Timestamp + ?UNIX_EPOCH_OFFSET,
    {{Y, Mo, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    iolist_to_binary(
        io_lib:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
            [Y, Mo, D, H, Mi, S]
        )
    ).

-doc """
Parse and validate an RFC 3339 timestamp string.

Uses `calendar:rfc3339_to_system_time/1` for proper validation including
date validity checks (no Feb 30th, etc.).

Returns the system time in seconds if valid.

Example:
```
{ok, 1704067200} = ocibuild_time:parse_rfc3339(~"2024-01-01T00:00:00Z").
{error, badarg} = ocibuild_time:parse_rfc3339(~"not-a-date").
{error, badarg} = ocibuild_time:parse_rfc3339(~"2024-02-30T00:00:00Z").
```
""".
-spec parse_rfc3339(binary()) -> {ok, integer()} | {error, badarg}.
parse_rfc3339(Bin) when is_binary(Bin) ->
    try
        %% calendar:rfc3339_to_system_time/2 with unit option returns seconds
        Seconds = calendar:rfc3339_to_system_time(binary_to_list(Bin), [{unit, second}]),
        {ok, Seconds}
    catch
        error:_ -> {error, badarg}
    end.

-doc """
Parse and normalize an RFC 3339 timestamp to a consistent format.

Accepts RFC 3339 strings with various timezone formats and normalizes
to UTC with Z suffix: "YYYY-MM-DDTHH:MM:SSZ"

Example:
```
{ok, ~"2024-01-01T00:00:00Z"} = ocibuild_time:normalize_rfc3339(~"2024-01-01T00:00:00Z").
{ok, ~"2024-01-01T00:00:00Z"} = ocibuild_time:normalize_rfc3339(~"2024-01-01T01:00:00+01:00").
{error, badarg} = ocibuild_time:normalize_rfc3339(~"invalid").
```
""".
-spec normalize_rfc3339(binary()) -> {ok, binary()} | {error, badarg}.
normalize_rfc3339(Bin) when is_binary(Bin) ->
    case parse_rfc3339(Bin) of
        {ok, Seconds} ->
            {ok, unix_to_iso8601(Seconds)};
        {error, _} = Err ->
            Err
    end.
