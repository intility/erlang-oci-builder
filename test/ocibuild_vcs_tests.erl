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
%%% URL Sanitization Tests (security)
%%%===================================================================

%% Test that credentials are stripped from HTTPS URLs via CI env vars
url_sanitization_credentials_test() ->
    %% Save and clear env
    OldUrl = os:getenv("CI_PROJECT_URL"),
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),

    %% Clear GitHub vars so GitLab CI var is used
    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),

    %% Set a URL with embedded credentials (security risk)
    os:putenv("CI_PROJECT_URL", "https://token:secret@gitlab.com/group/project.git"),

    try
        {ok, Url} = ocibuild_vcs_git:get_source_url("/nonexistent/path"),
        %% Credentials should be stripped, .git extension removed
        ?assertEqual(~"https://gitlab.com/group/project", Url)
    after
        restore_env("CI_PROJECT_URL", OldUrl),
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo)
    end.

%% Test that query params and fragments are also stripped
url_sanitization_query_params_test() ->
    OldUrl = os:getenv("CI_PROJECT_URL"),
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),

    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),

    %% Set a URL with query params (could leak tokens)
    os:putenv("CI_PROJECT_URL", "https://github.com/org/repo?token=secret#ref"),

    try
        {ok, Url} = ocibuild_vcs_git:get_source_url("/nonexistent/path"),
        %% Query params and fragment should be stripped
        ?assertEqual(~"https://github.com/org/repo", Url)
    after
        restore_env("CI_PROJECT_URL", OldUrl),
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo)
    end.

%%%===================================================================
%%% SSH to HTTPS URL Conversion Tests
%%%===================================================================

%% Test git@host:path SSH format is converted to HTTPS
ssh_git_at_format_test() ->
    OldUrl = os:getenv("CI_PROJECT_URL"),
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),

    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),

    %% Set SSH URL in git@host:path format
    os:putenv("CI_PROJECT_URL", "git@github.com:myorg/myrepo.git"),

    try
        {ok, Url} = ocibuild_vcs_git:get_source_url("/nonexistent/path"),
        ?assertEqual(~"https://github.com/myorg/myrepo", Url)
    after
        restore_env("CI_PROJECT_URL", OldUrl),
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo)
    end.

%% Test ssh://git@host/path SSH format is converted to HTTPS
ssh_protocol_format_test() ->
    OldUrl = os:getenv("CI_PROJECT_URL"),
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),

    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),

    %% Set SSH URL in ssh:// format
    os:putenv("CI_PROJECT_URL", "ssh://git@github.com/myorg/myrepo.git"),

    try
        {ok, Url} = ocibuild_vcs_git:get_source_url("/nonexistent/path"),
        ?assertEqual(~"https://github.com/myorg/myrepo", Url)
    after
        restore_env("CI_PROJECT_URL", OldUrl),
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo)
    end.

%% Test ssh://git@host:port/path SSH format with port is converted to HTTPS
ssh_protocol_with_port_test() ->
    OldUrl = os:getenv("CI_PROJECT_URL"),
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),

    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),

    %% Set SSH URL with custom port
    os:putenv("CI_PROJECT_URL", "ssh://git@github.com:22/myorg/myrepo.git"),

    try
        {ok, Url} = ocibuild_vcs_git:get_source_url("/nonexistent/path"),
        ?assertEqual(~"https://github.com/myorg/myrepo", Url)
    after
        restore_env("CI_PROJECT_URL", OldUrl),
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo)
    end.

%%%===================================================================
%%% VCS Detection Tests
%%%===================================================================

%% Test that detect returns not_found for a non-existent directory (without CI env vars)
detect_nonexistent_test() ->
    %% Clear CI env vars that would cause detection to succeed
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),
    OldGLUrl = os:getenv("CI_PROJECT_URL"),
    OldAzureUri = os:getenv("BUILD_REPOSITORY_URI"),

    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),
    os:unsetenv("CI_PROJECT_URL"),
    os:unsetenv("BUILD_REPOSITORY_URI"),

    try
        ?assertEqual(not_found, ocibuild_vcs:detect("/this/path/does/not/exist"))
    after
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo),
        restore_env("CI_PROJECT_URL", OldGLUrl),
        restore_env("BUILD_REPOSITORY_URI", OldAzureUri)
    end.

%% Test that detection returns not_found for /tmp (unlikely to be a git repo)
detect_tmp_test() ->
    %% /tmp is unlikely to have a .git directory
    %% In CI, env vars may cause detection to succeed, which is also valid
    Result = ocibuild_vcs:detect("/tmp"),
    ?assert(Result =:= not_found orelse element(1, Result) =:= ok).

%% Test git detection returns false for non-git directories (without CI env vars)
git_detect_nonrepo_test() ->
    %% Clear CI env vars that would cause detection to succeed
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),
    OldGLUrl = os:getenv("CI_PROJECT_URL"),
    OldAzureUri = os:getenv("BUILD_REPOSITORY_URI"),

    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),
    os:unsetenv("CI_PROJECT_URL"),
    os:unsetenv("BUILD_REPOSITORY_URI"),

    try
        ?assertEqual(false, ocibuild_vcs_git:detect("/tmp"))
    after
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo),
        restore_env("CI_PROJECT_URL", OldGLUrl),
        restore_env("BUILD_REPOSITORY_URI", OldAzureUri)
    end.

%% Test git detection returns true when CI env vars are present (even without .git)
git_detect_with_ci_env_vars_test() ->
    %% Save and clear all CI env vars
    OldGHServer = os:getenv("GITHUB_SERVER_URL"),
    OldGHRepo = os:getenv("GITHUB_REPOSITORY"),
    OldGLUrl = os:getenv("CI_PROJECT_URL"),
    OldAzureUri = os:getenv("BUILD_REPOSITORY_URI"),

    os:unsetenv("GITHUB_SERVER_URL"),
    os:unsetenv("GITHUB_REPOSITORY"),
    os:unsetenv("CI_PROJECT_URL"),
    os:unsetenv("BUILD_REPOSITORY_URI"),

    try
        %% Without CI env vars, detection should fail for /tmp
        ?assertEqual(false, ocibuild_vcs_git:detect("/tmp")),

        %% Set GitHub Actions env vars
        os:putenv("GITHUB_SERVER_URL", "https://github.com"),
        os:putenv("GITHUB_REPOSITORY", "org/repo"),

        %% Now detection should succeed (even though /tmp has no .git)
        ?assertEqual(true, ocibuild_vcs_git:detect("/tmp"))
    after
        restore_env("GITHUB_SERVER_URL", OldGHServer),
        restore_env("GITHUB_REPOSITORY", OldGHRepo),
        restore_env("CI_PROJECT_URL", OldGLUrl),
        restore_env("BUILD_REPOSITORY_URI", OldAzureUri)
    end.

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
        %% Disable VCS annotations to avoid git dependency in tests
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
    %% Minimum ISO 8601 length: YYYY-MM-DDTHH:MM:SSZ = 20 characters
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
