%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for the rebar3 provider
%%% @end
%%%-------------------------------------------------------------------
-module(ocibuild_rebar3_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% File collection tests
%%%===================================================================

collect_release_files_test() ->
    %% Create a mock release directory structure
    TmpDir = create_mock_release(),
    try
        %% Test file collection
        {ok, Files} = collect_release_files(TmpDir),

        %% Should have collected the files we created
        ?assert(length(Files) >= 3),

        %% Check that bin/myapp has executable permissions
        BinFile = lists:keyfind(<<"/app/bin/myapp">>, 1, Files),
        ?assertNotEqual(false, BinFile),
        {_, _, BinMode} = BinFile,
        ?assertEqual(8#755, BinMode band 8#777),

        %% Check that lib file has regular permissions
        LibFile = lists:keyfind(<<"/app/lib/myapp-1.0.0/ebin/myapp.beam">>, 1, Files),
        ?assertNotEqual(false, LibFile),
        {_, _, LibMode} = LibFile,
        ?assertEqual(8#644, LibMode band 8#777)
    after
        %% Cleanup
        cleanup_temp_dir(TmpDir)
    end.

collect_empty_dir_test() ->
    TmpDir = make_temp_dir("ocibuild_empty"),
    try
        {ok, Files} = collect_release_files(TmpDir),
        ?assertEqual([], Files)
    after
        cleanup_temp_dir(TmpDir)
    end.

%%%===================================================================
%%% Build image tests
%%%===================================================================

build_scratch_image_test() ->
    Files = [
        {<<"/app/bin/myapp">>, <<"#!/bin/sh\necho hello">>, 8#755},
        {<<"/app/lib/myapp.beam">>, <<"beam_data">>, 8#644}
    ],

    {ok, Image} = build_test_image(<<"scratch">>, Files, "myapp"),

    %% Verify image structure
    ?assert(is_map(Image)),
    ?assertEqual(1, length(maps:get(layers, Image))),

    %% Verify config
    Config = maps:get(config, Image),
    InnerConfig = maps:get(<<"config">>, Config),
    ?assertEqual([<<"/app/bin/myapp">>, <<"foreground">>],
                 maps:get(<<"Entrypoint">>, InnerConfig)),
    ?assertEqual(<<"/app">>, maps:get(<<"WorkingDir">>, InnerConfig)).

build_with_env_test() ->
    Files = [{<<"/app/test">>, <<"data">>, 8#644}],
    EnvMap = #{<<"LANG">> => <<"C.UTF-8">>, <<"PORT">> => <<"8080">>},

    {ok, Image} = build_test_image_with_opts(<<"scratch">>, Files, "myapp",
                                              <<"/app">>, EnvMap, [], #{}),

    Config = maps:get(config, Image),
    InnerConfig = maps:get(<<"config">>, Config),
    EnvList = maps:get(<<"Env">>, InnerConfig),

    %% Should contain both env vars
    ?assert(lists:any(fun(E) -> binary:match(E, <<"LANG=">>) =/= nomatch end, EnvList)),
    ?assert(lists:any(fun(E) -> binary:match(E, <<"PORT=">>) =/= nomatch end, EnvList)).

build_with_exposed_ports_test() ->
    Files = [{<<"/app/test">>, <<"data">>, 8#644}],

    {ok, Image} = build_test_image_with_opts(<<"scratch">>, Files, "myapp",
                                              <<"/app">>, #{}, [8080, 443], #{}),

    Config = maps:get(config, Image),
    InnerConfig = maps:get(<<"config">>, Config),
    ExposedPorts = maps:get(<<"ExposedPorts">>, InnerConfig),

    ?assert(maps:is_key(<<"8080/tcp">>, ExposedPorts)),
    ?assert(maps:is_key(<<"443/tcp">>, ExposedPorts)).

build_with_labels_test() ->
    Files = [{<<"/app/test">>, <<"data">>, 8#644}],
    Labels = #{<<"org.opencontainers.image.version">> => <<"1.0.0">>},

    {ok, Image} = build_test_image_with_opts(<<"scratch">>, Files, "myapp",
                                              <<"/app">>, #{}, [], Labels),

    Config = maps:get(config, Image),
    InnerConfig = maps:get(<<"config">>, Config),
    ImageLabels = maps:get(<<"Labels">>, InnerConfig),

    ?assertEqual(<<"1.0.0">>, maps:get(<<"org.opencontainers.image.version">>, ImageLabels)).

%%%===================================================================
%%% Tag parsing tests
%%%===================================================================

parse_tag_simple_test() ->
    ?assertEqual({<<"myapp">>, <<"1.0.0">>}, parse_tag(<<"myapp:1.0.0">>)).

parse_tag_no_version_test() ->
    ?assertEqual({<<"myapp">>, <<"latest">>}, parse_tag(<<"myapp">>)).

parse_tag_with_path_test() ->
    ?assertEqual({<<"myorg/myapp">>, <<"v1">>}, parse_tag(<<"myorg/myapp:v1">>)).

%%%===================================================================
%%% Auth tests
%%%===================================================================

get_auth_empty_test() ->
    %% Clear any existing env vars
    os:unsetenv("OCIBUILD_TOKEN"),
    os:unsetenv("OCIBUILD_USERNAME"),
    os:unsetenv("OCIBUILD_PASSWORD"),

    ?assertEqual(#{}, get_auth()).

get_auth_token_test() ->
    os:putenv("OCIBUILD_TOKEN", "mytoken123"),
    try
        Auth = get_auth(),
        ?assertEqual(#{token => <<"mytoken123">>}, Auth)
    after
        os:unsetenv("OCIBUILD_TOKEN")
    end.

get_auth_username_password_test() ->
    os:unsetenv("OCIBUILD_TOKEN"),
    os:putenv("OCIBUILD_USERNAME", "myuser"),
    os:putenv("OCIBUILD_PASSWORD", "mypass"),
    try
        Auth = get_auth(),
        ?assertEqual(#{username => <<"myuser">>, password => <<"mypass">>}, Auth)
    after
        os:unsetenv("OCIBUILD_USERNAME"),
        os:unsetenv("OCIBUILD_PASSWORD")
    end.

%%%===================================================================
%%% Helper functions - Extract from provider for testing
%%%===================================================================

%% These functions mirror the internal functions from ocibuild_rebar3
%% for testing purposes

collect_release_files(ReleasePath) ->
    try
        Files = collect_files_recursive(ReleasePath, ReleasePath),
        {ok, Files}
    catch
        throw:{file_error, Path, Reason} ->
            {error, {file_read_error, Path, Reason}}
    end.

collect_files_recursive(BasePath, CurrentPath) ->
    case file:list_dir(CurrentPath) of
        {ok, Entries} ->
            lists:flatmap(fun(Entry) ->
                FullPath = filename:join(CurrentPath, Entry),
                case filelib:is_dir(FullPath) of
                    true ->
                        collect_files_recursive(BasePath, FullPath);
                    false ->
                        [collect_single_file(BasePath, FullPath)]
                end
            end, Entries);
        {error, Reason} ->
            throw({file_error, CurrentPath, Reason})
    end.

collect_single_file(BasePath, FilePath) ->
    %% Get relative path from base (cross-platform)
    RelPath = make_relative_path(BasePath, FilePath),
    %% Convert to container path with forward slashes
    ContainerPath = to_container_path(RelPath),
    {ok, Content} = file:read_file(FilePath),
    Mode = get_file_mode(FilePath),
    {ContainerPath, Content, Mode}.

%% @doc Make a path relative to a base path (cross-platform)
make_relative_path(BasePath, FullPath) ->
    %% Normalize both paths to use consistent separators
    BaseNorm = filename:split(BasePath),
    FullNorm = filename:split(FullPath),
    %% Remove the base prefix from the full path
    strip_prefix(BaseNorm, FullNorm).

strip_prefix([H|T1], [H|T2]) ->
    strip_prefix(T1, T2);
strip_prefix([], Remaining) ->
    filename:join(Remaining);
strip_prefix(_, FullPath) ->
    filename:join(FullPath).

%% @doc Convert a local path to a container path (always forward slashes)
to_container_path(RelPath) ->
    %% Split and rejoin with forward slashes for container
    Parts = filename:split(RelPath),
    UnixPath = string:join(Parts, "/"),
    list_to_binary("/app/" ++ UnixPath).

get_file_mode(FilePath) ->
    case file:read_file_info(FilePath) of
        {ok, FileInfo} ->
            element(8, FileInfo) band 8#777;
        {error, _} ->
            8#644
    end.

build_test_image(BaseImage, Files, ReleaseName) ->
    build_test_image_with_opts(BaseImage, Files, ReleaseName, <<"/app">>, #{}, [], #{}).

build_test_image_with_opts(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels) ->
    Image0 = case BaseImage of
        <<"scratch">> ->
            {ok, Img} = ocibuild:scratch(),
            Img
    end,

    Image1 = ocibuild:add_layer(Image0, Files),
    Image2 = ocibuild:workdir(Image1, Workdir),

    ReleaseNameBin = list_to_binary(ReleaseName),
    Entrypoint = [<<"/app/bin/", ReleaseNameBin/binary>>, <<"foreground">>],
    Image3 = ocibuild:entrypoint(Image2, Entrypoint),

    Image4 = case map_size(EnvMap) of
        0 -> Image3;
        _ -> ocibuild:env(Image3, EnvMap)
    end,

    Image5 = lists:foldl(fun(Port, AccImg) ->
        ocibuild:expose(AccImg, Port)
    end, Image4, ExposePorts),

    Image6 = maps:fold(fun(Key, Value, AccImg) ->
        ocibuild:label(AccImg, Key, Value)
    end, Image5, Labels),

    {ok, Image6}.

parse_tag(Tag) ->
    case binary:split(Tag, <<":">>) of
        [Repo, ImageTag] -> {Repo, ImageTag};
        [Repo] -> {Repo, <<"latest">>}
    end.

get_auth() ->
    case os:getenv("OCIBUILD_TOKEN") of
        false ->
            case {os:getenv("OCIBUILD_USERNAME"), os:getenv("OCIBUILD_PASSWORD")} of
                {false, _} -> #{};
                {_, false} -> #{};
                {User, Pass} ->
                    #{username => list_to_binary(User),
                      password => list_to_binary(Pass)}
            end;
        Token ->
            #{token => list_to_binary(Token)}
    end.

%%%===================================================================
%%% Cross-platform helpers
%%%===================================================================

%% @doc Get the system temp directory in a cross-platform way
temp_dir() ->
    case os:type() of
        {win32, _} ->
            case os:getenv("TEMP") of
                false ->
                    case os:getenv("TMP") of
                        false -> "C:\\Temp";
                        TmpDir -> TmpDir
                    end;
                TempDir -> TempDir
            end;
        _ ->
            "/tmp"
    end.

%% @doc Create a unique temporary directory
make_temp_dir(Prefix) ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    DirName = Prefix ++ "_" ++ Unique,
    TmpDir = filename:join(temp_dir(), DirName),
    ok = filelib:ensure_dir(filename:join(TmpDir, "placeholder")),
    case file:make_dir(TmpDir) of
        ok -> TmpDir;
        {error, eexist} -> TmpDir
    end.

%% @doc Recursively delete a directory (cross-platform)
cleanup_temp_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Files} = file:list_dir(Dir),
            lists:foreach(fun(File) ->
                Path = filename:join(Dir, File),
                case filelib:is_dir(Path) of
                    true -> cleanup_temp_dir(Path);
                    false -> file:delete(Path)
                end
            end, Files),
            file:del_dir(Dir);
        false ->
            ok
    end.

%%%===================================================================
%%% Test fixtures
%%%===================================================================

create_mock_release() ->
    TmpDir = make_temp_dir("ocibuild_release"),

    %% Create directory structure using filename:join for cross-platform paths
    BinDir = filename:join(TmpDir, "bin"),
    LibDir = filename:join([TmpDir, "lib", "myapp-1.0.0", "ebin"]),
    RelDir = filename:join([TmpDir, "releases", "1.0.0"]),

    ok = filelib:ensure_dir(filename:join(BinDir, "placeholder")),
    ok = filelib:ensure_dir(filename:join(LibDir, "placeholder")),
    ok = filelib:ensure_dir(filename:join(RelDir, "placeholder")),

    %% Create bin script (executable)
    BinPath = filename:join(BinDir, "myapp"),
    ok = file:write_file(BinPath, <<"#!/bin/sh\nexec erl -boot release">>),
    ok = file:change_mode(BinPath, 8#755),

    %% Create beam file
    BeamPath = filename:join(LibDir, "myapp.beam"),
    ok = file:write_file(BeamPath, <<"FOR1...(beam data)">>),
    ok = file:change_mode(BeamPath, 8#644),

    %% Create release file
    RelPath = filename:join(RelDir, "myapp.rel"),
    ok = file:write_file(RelPath, <<"{release, {\"myapp\", \"1.0.0\"}, ...}.">>),

    TmpDir.
