%%%-------------------------------------------------------------------
-module(ocibuild_vcs_git).
-feature(maybe_expr, enable).
-moduledoc """
Git adapter for OCI annotation support.

This module implements the `ocibuild_vcs` behaviour for Git repositories.
It extracts source URL and revision information for OCI image annotations.

## CI Environment Variables

For reliability in CI environments, this adapter checks environment variables
before falling back to git commands:

### Source URL (checked in order):
1. `GITHUB_SERVER_URL` + `GITHUB_REPOSITORY` (GitHub Actions)
2. `CI_PROJECT_URL` (GitLab CI)
3. `BUILD_REPOSITORY_URI` (Azure DevOps)
4. `git remote get-url origin` (fallback)

### Revision (checked in order):
1. `GITHUB_SHA` (GitHub Actions)
2. `CI_COMMIT_SHA` (GitLab CI)
3. `BUILD_SOURCEVERSION` (Azure DevOps)
4. `git rev-parse HEAD` (fallback)

## SSH to HTTPS Conversion

SSH URLs are automatically converted to HTTPS for public visibility:
- `git@github.com:org/repo.git` → `https://github.com/org/repo`
- `ssh://git@github.com/org/repo.git` → `https://github.com/org/repo`
""".

-behaviour(ocibuild_vcs).

-export([detect/1, get_source_url/1, get_revision/1]).

%% Timeout for git commands (milliseconds)
-define(GIT_TIMEOUT, 5000).

%%%===================================================================
%%% Behaviour Implementation
%%%===================================================================

-doc """
Detect if Git VCS information is available.

Returns true if either:
1. The path is within a Git repository (`.git` directory found), or
2. CI environment variables are present (GITHUB_*, CI_PROJECT_URL, BUILD_REPOSITORY_URI)

This allows VCS annotations to work in CI environments where the build
runs in a different directory than the repository checkout.
""".
-spec detect(file:filename()) -> boolean().
detect(Path) ->
    find_git_root(Path) =/= not_found orelse has_ci_env_vars().

-doc """
Get the source repository URL.

Tries CI environment variables first, then falls back to `git remote get-url origin`.
SSH URLs are converted to HTTPS.
""".
-spec get_source_url(file:filename()) -> {ok, binary()} | {error, term()}.
get_source_url(Path) ->
    %% Try CI environment variables first
    case get_source_url_from_env() of
        {ok, Url} ->
            %% Sanitize CI URL (remove credentials, .git extension)
            {ok, convert_ssh_to_https(Url)};
        not_found ->
            %% Fall back to git command
            maybe
                {ok, GitRoot} ?= find_git_root(Path),
                {ok, Url} ?= run_git_command(GitRoot, ["remote", "get-url", "origin"]),
                {ok, convert_ssh_to_https(Url)}
            else
                not_found -> {error, not_a_git_repo};
                {error, _} = Err -> Err
            end
    end.

-doc """
Get the current commit SHA.

Tries CI environment variables first, then falls back to `git rev-parse HEAD`.
""".
-spec get_revision(file:filename()) -> {ok, binary()} | {error, term()}.
get_revision(Path) ->
    %% Try CI environment variables first
    case get_revision_from_env() of
        {ok, Rev} ->
            {ok, Rev};
        not_found ->
            %% Fall back to git command
            maybe
                {ok, GitRoot} ?= find_git_root(Path),
                run_git_command(GitRoot, ["rev-parse", "HEAD"])
            else
                not_found -> {error, not_a_git_repo}
            end
    end.

%%%===================================================================
%%% Internal Functions - Environment Variables
%%%===================================================================

%% @private Get source URL from CI environment variables
-spec get_source_url_from_env() -> {ok, binary()} | not_found.
get_source_url_from_env() ->
    %% Try sources in priority order: GitHub Actions, GitLab CI, Azure DevOps
    case try_github_source_url() of
        {ok, _} = Result ->
            Result;
        not_found ->
            case try_env_var("CI_PROJECT_URL") of
                {ok, _} = Result -> Result;
                not_found -> try_env_var("BUILD_REPOSITORY_URI")
            end
    end.

%% @private Try GitHub Actions source URL (SERVER_URL + REPOSITORY)
-spec try_github_source_url() -> {ok, binary()} | not_found.
try_github_source_url() ->
    case {os:getenv("GITHUB_SERVER_URL"), os:getenv("GITHUB_REPOSITORY")} of
        {Server, Repo} when is_list(Server), is_list(Repo), Server =/= "", Repo =/= "" ->
            {ok, iolist_to_binary([Server, "/", Repo])};
        _ ->
            not_found
    end.

%% @private Try to get URL/SHA from a single environment variable
-spec try_env_var(string()) -> {ok, binary()} | not_found.
try_env_var(VarName) ->
    case os:getenv(VarName) of
        Value when is_list(Value), Value =/= "" ->
            {ok, list_to_binary(Value)};
        _ ->
            not_found
    end.

%% @private Get revision from CI environment variables
-spec get_revision_from_env() -> {ok, binary()} | not_found.
get_revision_from_env() ->
    %% Try sources in priority order: GitHub Actions, GitLab CI, Azure DevOps
    case try_env_var("GITHUB_SHA") of
        {ok, _} = Result ->
            Result;
        not_found ->
            case try_env_var("CI_COMMIT_SHA") of
                {ok, _} = Result -> Result;
                not_found -> try_env_var("BUILD_SOURCEVERSION")
            end
    end.

%% @private Check if any CI environment variables are present
%% This allows detection to succeed in CI even when .git is in a different directory
-spec has_ci_env_vars() -> boolean().
has_ci_env_vars() ->
    %% Check for GitHub Actions
    (is_list(os:getenv("GITHUB_SERVER_URL")) andalso
        is_list(os:getenv("GITHUB_REPOSITORY"))) orelse
        %% Check for GitLab CI
        is_list(os:getenv("CI_PROJECT_URL")) orelse
        %% Check for Azure DevOps
        is_list(os:getenv("BUILD_REPOSITORY_URI")).

%%%===================================================================
%%% Internal Functions - Git Detection
%%%===================================================================

%% @private Find the git root directory by walking up the tree
-spec find_git_root(file:filename()) -> {ok, file:filename()} | not_found.
find_git_root(Path) ->
    try
        AbsPath = filename:absname(Path),
        %% Normalize: if path is a file, start from its parent directory
        StartPath =
            case filelib:is_regular(AbsPath) of
                true -> filename:dirname(AbsPath);
                false -> AbsPath
            end,
        %% Verify the path exists before trying to walk up
        case filelib:is_dir(StartPath) of
            true ->
                find_git_root_recursive(StartPath);
            false ->
                not_found
        end
    catch
        _:_ ->
            not_found
    end.

-spec find_git_root_recursive(file:filename()) -> {ok, file:filename()} | not_found.
find_git_root_recursive(Path) ->
    GitDir = filename:join(Path, ".git"),
    %% .git can be a directory, a regular file (worktrees), or a symlink
    case filelib:is_dir(GitDir) orelse filelib:is_regular(GitDir) orelse filelib:is_link(GitDir) of
        true ->
            {ok, Path};
        false ->
            Parent = filename:dirname(Path),
            %% Platform-independent root check: at root, dirname returns same path
            case Parent =:= Path of
                true -> not_found;
                false -> find_git_root_recursive(Parent)
            end
    end.

%%%===================================================================
%%% Internal Functions - Git Commands via Port
%%%===================================================================

%% @private Run a git command using Erlang port for security and proper error handling
-spec run_git_command(file:filename(), [string()]) -> {ok, binary()} | {error, term()}.
run_git_command(WorkDir, Args) ->
    case os:find_executable("git") of
        false ->
            {error, git_not_found};
        GitPath ->
            try
                Port = open_port(
                    {spawn_executable, GitPath},
                    [
                        {args, Args},
                        {cd, WorkDir},
                        binary,
                        exit_status,
                        stderr_to_stdout,
                        hide
                    ]
                ),
                receive_port_output(Port, <<>>)
            catch
                error:Reason ->
                    {error, {port_error, Reason}}
            end
    end.

%% @private Collect output from port until exit
-spec receive_port_output(port(), binary()) -> {ok, binary()} | {error, term()}.
receive_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            receive_port_output(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, 0}} ->
            %% Trim trailing whitespace (especially newlines)
            {ok, string:trim(Acc, trailing)};
        {Port, {exit_status, Code}} ->
            {error, {git_exit, Code, Acc}}
    after ?GIT_TIMEOUT ->
        catch port_close(Port),
        {error, timeout}
    end.

%%%===================================================================
%%% Internal Functions - URL Conversion
%%%===================================================================

%% @private Convert SSH URLs to HTTPS for public visibility
-spec convert_ssh_to_https(binary()) -> binary().
convert_ssh_to_https(Url) ->
    %% Trim both leading and trailing whitespace
    UrlStr = binary_to_list(string:trim(Url)),
    ConvertedStr = convert_ssh_to_https_str(UrlStr),
    list_to_binary(ConvertedStr).

-spec convert_ssh_to_https_str(string()) -> string().
convert_ssh_to_https_str("git@" ++ Rest) ->
    %% git@github.com:org/repo.git -> https://github.com/org/repo
    case string:split(Rest, ":") of
        [Host, PathWithExt] ->
            Path = strip_git_extension(PathWithExt),
            "https://" ++ Host ++ "/" ++ Path;
        _ ->
            "git@" ++ Rest
    end;
convert_ssh_to_https_str("ssh://git@" ++ Rest) ->
    %% ssh://git@github.com/org/repo.git -> https://github.com/org/repo
    %% ssh://git@github.com:22/org/repo.git -> https://github.com/org/repo
    case string:split(Rest, "/") of
        [HostMaybePort, PathPart] ->
            Host = strip_port(HostMaybePort),
            Path = strip_git_extension(PathPart),
            "https://" ++ Host ++ "/" ++ Path;
        [HostMaybePort | PathParts] ->
            Host = strip_port(HostMaybePort),
            %% Filter empty path components to avoid malformed URLs
            case [P || P <- PathParts, P =/= ""] of
                [] ->
                    %% No usable path, fall back to original
                    "ssh://git@" ++ Rest;
                FilteredParts ->
                    Path = strip_git_extension(string:join(FilteredParts, "/")),
                    "https://" ++ Host ++ "/" ++ Path
            end;
        _ ->
            "ssh://git@" ++ Rest
    end;
convert_ssh_to_https_str(Url) ->
    %% Already HTTPS or other format, sanitize and strip .git extension
    sanitize_url(strip_git_extension(Url)).

%% @private Strip .git extension from URL
-spec strip_git_extension(string()) -> string().
strip_git_extension(Path) ->
    case string:find(Path, ".git", trailing) of
        ".git" ->
            string:slice(Path, 0, length(Path) - 4);
        _ ->
            Path
    end.

%% @private Strip port number from host (e.g., "github.com:22" -> "github.com")
-spec strip_port(string()) -> string().
strip_port(HostMaybePort) ->
    case string:split(HostMaybePort, ":") of
        [Host, _Port] -> Host;
        [Host] -> Host
    end.

%% @private Sanitize URL by removing credentials and sensitive components
%% Prevents credential leakage via image annotations for URLs like:
%% https://token:pass@example.com/org/repo.git
-spec sanitize_url(string()) -> string().
sanitize_url(Url) ->
    try uri_string:parse(Url) of
        #{scheme := Scheme} = UriMap when Scheme =:= "http"; Scheme =:= "https" ->
            %% Remove userinfo (credentials), query params, and fragments
            CleanMap = maps:without([userinfo, query, fragment], UriMap),
            uri_string:recompose(CleanMap);
        _ ->
            %% Non-HTTP(S) URL or no scheme, return as-is
            Url
    catch
        _:_ ->
            %% Parsing failed, return original
            Url
    end.
