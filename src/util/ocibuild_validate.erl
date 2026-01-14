%% @doc Shared validation utilities for security checks.
%%
%% This module provides reusable validation functions for checking user-provided
%% inputs for common security issues like null bytes and path traversal attacks.
%%
%% These functions are used throughout the codebase:
%% - `ocibuild_tar.erl` - validates file paths in tar archives
%% - `ocibuild_release.erl` - validates image tags
%% - `ocibuild_layout.erl` - validates tar extraction paths
%% - Annotation validation for user-provided manifest annotations
-module(ocibuild_validate).

-export([
    no_null_bytes/1,
    no_traversal/1,
    no_traversal/2,
    no_absolute/1,
    not_empty/1,
    validate_user_string/1
]).

-doc """
Check that a binary contains no null bytes.

Null bytes can be used to truncate strings in C-based tools, potentially
leading to security issues when paths are processed by external programs.

## Examples

```
ok = ocibuild_validate:no_null_bytes(~"safe/path").
{error, {null_byte, _}} = ocibuild_validate:no_null_bytes(<<"has\0null">>).
```
""".
-spec no_null_bytes(binary()) -> ok | {error, {null_byte, binary()}}.
no_null_bytes(Bin) when is_binary(Bin) ->
    case binary:match(Bin, <<0>>) of
        nomatch -> ok;
        _ -> {error, {null_byte, Bin}}
    end.

-doc """
Check that a binary contains no path traversal sequences.

Uses `/` as the default separator. For custom separators, use `no_traversal/2`.

## Examples

```
ok = ocibuild_validate:no_traversal(~"safe/path/here").
{error, {path_traversal, _}} = ocibuild_validate:no_traversal(~"../etc/passwd").
```
""".
-spec no_traversal(binary()) -> ok | {error, {path_traversal, binary()}}.
no_traversal(Bin) ->
    no_traversal(Bin, [~"/"]).

-doc """
Check that a binary contains no path traversal sequences using custom separators.

The binary is split by the provided separators, and each component is checked
for `..` (parent directory reference).

## Examples

```
ok = ocibuild_validate:no_traversal(~"safe:path", [~":"]).
{error, {path_traversal, _}} = ocibuild_validate:no_traversal(~"safe:../bad", [~":"]).
%% Multiple separators
ok = ocibuild_validate:no_traversal(~"safe/path:here", [~"/", ~":"]).
```
""".
-spec no_traversal(binary(), [binary()]) -> ok | {error, {path_traversal, binary()}}.
no_traversal(Bin, Separators) when is_binary(Bin), is_list(Separators) ->
    Components = binary:split(Bin, Separators, [global]),
    case lists:member(~"..", Components) of
        true -> {error, {path_traversal, Bin}};
        false -> ok
    end.

-doc """
Check that a binary is not an absolute path (does not start with `/`).

## Examples

```
ok = ocibuild_validate:no_absolute(~"relative/path").
{error, {absolute_path, _}} = ocibuild_validate:no_absolute(~"/absolute/path").
```
""".
-spec no_absolute(binary()) -> ok | {error, {absolute_path, binary()}}.
no_absolute(<<"/", _/binary>> = Bin) ->
    {error, {absolute_path, Bin}};
no_absolute(Bin) when is_binary(Bin) ->
    ok.

-doc """
Check that a binary is not empty.

## Examples

```
ok = ocibuild_validate:not_empty(~"content").
{error, empty_value} = ocibuild_validate:not_empty(<<>>).
```
""".
-spec not_empty(binary()) -> ok | {error, empty_value}.
not_empty(<<>>) ->
    {error, empty_value};
not_empty(Bin) when is_binary(Bin) ->
    ok.

-doc """
Combined validation for user-provided strings.

Checks for both null bytes and path traversal attacks. Uses `/` and `:` as
separators for path traversal detection, which covers both filesystem paths
and OCI reference formats.

This is the recommended function for validating user inputs like:
- Image tags
- Annotation keys and values
- Other user-provided metadata

## Examples

```
ok = ocibuild_validate:validate_user_string(~"myapp:v1.0.0").
ok = ocibuild_validate:validate_user_string(~"org.opencontainers.image.vendor").
{error, {null_byte, _}} = ocibuild_validate:validate_user_string(<<"bad\0string">>).
{error, {path_traversal, _}} = ocibuild_validate:validate_user_string(~"../etc/passwd").
```
""".
-spec validate_user_string(binary()) -> ok | {error, term()}.
validate_user_string(Bin) when is_binary(Bin) ->
    case no_null_bytes(Bin) of
        ok -> no_traversal(Bin, [~"/", ~":"]);
        Error -> Error
    end.
