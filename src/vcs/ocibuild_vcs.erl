%%%-------------------------------------------------------------------
-module(ocibuild_vcs).
-moduledoc """
Version Control System behaviour for OCI annotation support.

This module defines the behaviour for VCS adapters (Git, Mercurial, etc.)
and provides detection and annotation extraction functions.

VCS adapters are used to automatically populate OCI annotations:
- `org.opencontainers.image.source` - Repository URL
- `org.opencontainers.image.revision` - Commit SHA/revision

## Implementing an Adapter

```erlang
-module(ocibuild_vcs_hg).
-behaviour(ocibuild_vcs).

-export([detect/1, get_source_url/1, get_revision/1]).

detect(Path) ->
    %% Check for .hg directory
    filelib:is_dir(filename:join(Path, ".hg")).

get_source_url(Path) ->
    %% Get default remote URL
    {ok, ~"https://example.com/repo"}.

get_revision(Path) ->
    %% Get current revision
    {ok, ~"abc123"}.
```

## Usage

```erlang
%% Detect VCS and get annotations
case ocibuild_vcs:detect("/path/to/project") of
    {ok, VcsModule} ->
        Annotations = ocibuild_vcs:get_annotations(VcsModule, "/path/to/project");
    not_found ->
        #{}
end.
```
""".

%%%===================================================================
%%% Behaviour Definition
%%%===================================================================

-doc """
Check if this VCS manages the given path.

Should check for VCS-specific markers (e.g., `.git/` for Git, `.hg/` for Mercurial).
The implementation should walk up the directory tree to find the VCS root.
""".
-callback detect(Path :: file:filename()) -> boolean().

-doc """
Get the source repository URL.

Should try CI environment variables first for reliability, then fall back to
VCS commands. The returned URL should be HTTPS for public visibility.
""".
-callback get_source_url(Path :: file:filename()) -> {ok, binary()} | {error, term()}.

-doc """
Get the current revision/commit identifier.

Should try CI environment variables first, then fall back to VCS commands.
For Git, this is the full commit SHA. For SVN, the revision number.
""".
-callback get_revision(Path :: file:filename()) -> {ok, binary()} | {error, term()}.

%%%===================================================================
%%% Exports
%%%===================================================================

-export([detect/1, get_annotations/2]).

%% List of known VCS adapter modules (in order of detection priority)
-define(VCS_ADAPTERS, [ocibuild_vcs_git]).

%%%===================================================================
%%% Public API
%%%===================================================================

-doc """
Detect which VCS manages the given path.

Walks up the directory tree checking each registered VCS adapter.
Returns `{ok, Module}` if a VCS is found, `not_found` otherwise.

Example:
```erlang
case ocibuild_vcs:detect("/path/to/project") of
    {ok, ocibuild_vcs_git} -> io:format("Git repository~n");
    not_found -> io:format("No VCS detected~n")
end.
```
""".
-spec detect(file:filename()) -> {ok, module()} | not_found.
detect(Path) ->
    detect_vcs(Path, ?VCS_ADAPTERS).

-doc """
Get VCS annotations for the given path using the specified adapter.

Returns a map with available annotations:
- `~"org.opencontainers.image.source"` - Repository URL (if available)
- `~"org.opencontainers.image.revision"` - Commit/revision (if available)

Missing or failed values are simply omitted from the map.

Example:
```erlang
{ok, VcsModule} = ocibuild_vcs:detect(Path),
Annotations = ocibuild_vcs:get_annotations(VcsModule, Path).
%% #{~"org.opencontainers.image.source" => ~"https://github.com/org/repo",
%%   ~"org.opencontainers.image.revision" => ~"abc123..."}
```
""".
-spec get_annotations(module(), file:filename()) -> #{binary() => binary()}.
get_annotations(VcsModule, Path) ->
    Annotations0 = #{},

    %% Add source URL if available
    Annotations1 =
        case VcsModule:get_source_url(Path) of
            {ok, Url} when is_binary(Url), byte_size(Url) > 0 ->
                Annotations0#{~"org.opencontainers.image.source" => Url};
            _ ->
                Annotations0
        end,

    %% Add revision if available
    case VcsModule:get_revision(Path) of
        {ok, Rev} when is_binary(Rev), byte_size(Rev) > 0 ->
            Annotations1#{~"org.opencontainers.image.revision" => Rev};
        _ ->
            Annotations1
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Try each VCS adapter until one detects a repository
-spec detect_vcs(file:filename(), [module()]) -> {ok, module()} | not_found.
detect_vcs(_Path, []) ->
    not_found;
detect_vcs(Path, [Adapter | Rest]) ->
    case Adapter:detect(Path) of
        true -> {ok, Adapter};
        false -> detect_vcs(Path, Rest)
    end.
