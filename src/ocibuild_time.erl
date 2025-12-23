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
%% => <<"1970-01-01T00:00:00Z">>

ocibuild_time:unix_to_iso8601(1700000000).
%% => <<"2023-11-14T22:13:20Z">>
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
