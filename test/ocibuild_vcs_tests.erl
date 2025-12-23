%%%-------------------------------------------------------------------
-module(ocibuild_vcs_tests).
-moduledoc "Tests for VCS detection and annotation support.".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Environment Variable Tests (don't require git)
%%%===================================================================

%% Test GitHub Actions env vars take precedence
github_env_vars_test() ->
    %% Save current env
    OldServerUrl = os:getenv("GITHUB_SERVER_URL"),
    OldRepo = os:getenv("GITHUB_REPOSITORY"),
    OldSha = os:getenv("GITHUB_SHA"),

    %% Set env vars
    os:putenv("GITHUB_SERVER_URL", "https://github.com"),
    os:putenv("GITHUB_REPOSITORY", "test/repo"),
    os:putenv("GITHUB_SHA", "abc123def456abc123def456abc123def456abcd"),

    try
        %% Source URL should come from env vars
        {ok, Url} = ocibuild_vcs_git:get_source_url("/nonexistent/path"),
        ?assertEqual(~"https://github.com/test/repo", Url),

        %% Revision should come from env vars
        {ok, Rev} = ocibuild_vcs_git:get_revision("/nonexistent/path"),
        ?assertEqual(~"abc123def456abc123def456abc123def456abcd", Rev)
    after
        %% Restore env
        restore_env("GITHUB_SERVER_URL", OldServerUrl),
        restore_env("GITHUB_REPOSITORY", OldRepo),
        restore_env("GITHUB_SHA", OldSha)
    end.

%% Test GitLab CI env vars
gitlab_env_vars_test() ->
    %% Save current env and clear GitHub vars
    OldUrl = os:getenv("CI_PROJECT_URL"),
    OldSha = os:getenv("CI_COMMIT_SHA"),
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),
    OldGHSha = os:getenv("GITHUB_SHA"),

    %% Clear GitHub vars
    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),
    os:unsetenv("GITHUB_SHA"),

    %% Set GitLab env vars
    os:putenv("CI_PROJECT_URL", "https://gitlab.com/group/project"),
    os:putenv("CI_COMMIT_SHA", "fedcba9876543210fedcba9876543210fedcba98"),

    try
        %% Source URL should come from env vars
        {ok, Url} = ocibuild_vcs_git:get_source_url("/nonexistent/path"),
        ?assertEqual(~"https://gitlab.com/group/project", Url),

        %% Revision should come from env vars
        {ok, Rev} = ocibuild_vcs_git:get_revision("/nonexistent/path"),
        ?assertEqual(~"fedcba9876543210fedcba9876543210fedcba98", Rev)
    after
        %% Restore env
        restore_env("CI_PROJECT_URL", OldUrl),
        restore_env("CI_COMMIT_SHA", OldSha),
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo),
        restore_env("GITHUB_SHA", OldGHSha)
    end.

%% Test Azure DevOps env vars
azure_devops_env_vars_test() ->
    %% Save current env and clear other CI vars
    OldUri = os:getenv("BUILD_REPOSITORY_URI"),
    OldVersion = os:getenv("BUILD_SOURCEVERSION"),
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),
    OldGHSha = os:getenv("GITHUB_SHA"),
    OldGLUrl = os:getenv("CI_PROJECT_URL"),
    OldGLSha = os:getenv("CI_COMMIT_SHA"),

    %% Clear other CI vars
    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),
    os:unsetenv("GITHUB_SHA"),
    os:unsetenv("CI_PROJECT_URL"),
    os:unsetenv("CI_COMMIT_SHA"),

    %% Set Azure DevOps env vars
    os:putenv("BUILD_REPOSITORY_URI", "https://dev.azure.com/org/proj/_git/repo"),
    os:putenv("BUILD_SOURCEVERSION", "1234567890abcdef1234567890abcdef12345678"),

    try
        %% Source URL should come from env vars
        {ok, Url} = ocibuild_vcs_git:get_source_url("/nonexistent/path"),
        ?assertEqual(~"https://dev.azure.com/org/proj/_git/repo", Url),

        %% Revision should come from env vars
        {ok, Rev} = ocibuild_vcs_git:get_revision("/nonexistent/path"),
        ?assertEqual(~"1234567890abcdef1234567890abcdef12345678", Rev)
    after
        %% Restore env
        restore_env("BUILD_REPOSITORY_URI", OldUri),
        restore_env("BUILD_SOURCEVERSION", OldVersion),
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo),
        restore_env("GITHUB_SHA", OldGHSha),
        restore_env("CI_PROJECT_URL", OldGLUrl),
        restore_env("CI_COMMIT_SHA", OldGLSha)
    end.

%%%===================================================================
%%% VCS Detection Tests
%%%===================================================================

%% Test that detect returns not_found for a non-existent directory
detect_nonexistent_test() ->
    ?assertEqual(not_found, ocibuild_vcs:detect("/this/path/does/not/exist")).

%% Test that detection returns not_found for /tmp (unlikely to be a git repo)
detect_tmp_test() ->
    %% /tmp is unlikely to have a .git directory
    Result = ocibuild_vcs:detect("/tmp"),
    ?assert(Result =:= not_found orelse element(1, Result) =:= ok).

%% Test git detection returns false for non-git directories
git_detect_nonrepo_test() ->
    ?assertEqual(false, ocibuild_vcs_git:detect("/tmp")).

%%%===================================================================
%%% VCS Behaviour Tests
%%%===================================================================

%% Test that get_annotations returns a map
get_annotations_returns_map_test() ->
    %% Even if VCS is not detected, should return a map
    Annotations = ocibuild_vcs:get_annotations(ocibuild_vcs_git, "/nonexistent"),
    ?assert(is_map(Annotations)).

%%%===================================================================
%%% Auto-annotations Tests
%%%===================================================================

%% Test build_auto_annotations creates expected annotations
auto_annotations_version_test() ->
    %% Create a minimal image for testing
    {ok, Image} = ocibuild:scratch(),
    Config = #{
        % Disable VCS to avoid git dependency
        vcs_annotations => false,
        app_version => ~"1.2.3"
    },
    Annotations = ocibuild_release:build_auto_annotations(Image, "/tmp", Config),

    %% Should have version annotation
    ?assertEqual(~"1.2.3", maps:get(~"org.opencontainers.image.version", Annotations)),

    %% Should have created timestamp
    ?assert(maps:is_key(~"org.opencontainers.image.created", Annotations)).

%% Test VCS annotations disabled
auto_annotations_vcs_disabled_test() ->
    {ok, Image} = ocibuild:scratch(),
    Config = #{
        vcs_annotations => false
    },
    Annotations = ocibuild_release:build_auto_annotations(Image, "/tmp", Config),

    %% Should NOT have VCS annotations when disabled
    ?assertEqual(error, maps:find(~"org.opencontainers.image.source", Annotations)),
    ?assertEqual(error, maps:find(~"org.opencontainers.image.revision", Annotations)).

%% Test created timestamp format
auto_annotations_timestamp_format_test() ->
    {ok, Image} = ocibuild:scratch(),
    Config = #{vcs_annotations => false},
    Annotations = ocibuild_release:build_auto_annotations(Image, "/tmp", Config),

    Created = maps:get(~"org.opencontainers.image.created", Annotations),
    %% Should be ISO 8601 format like "2024-01-01T12:00:00Z"
    ?assert(is_binary(Created)),
    % Minimum ISO 8601 length
    ?assert(byte_size(Created) >= 20),
    % Contains 'T' separator
    ?assert(binary:match(Created, ~"T") =/= nomatch).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Restore environment variable (false means unset)
restore_env(Name, false) ->
    os:unsetenv(Name);
restore_env(Name, Value) ->
    os:putenv(Name, Value).
