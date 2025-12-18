%%%-------------------------------------------------------------------
-module(ocibuild_rebar3).
-moduledoc """
Rebar3 provider for building OCI container images from releases.

## Usage

```
rebar3 release
rebar3 ocibuild -t myapp:1.0.0
```

Or push directly to a registry:

```
rebar3 ocibuild -t myapp:1.0.0 --push ghcr.io/myorg
```

## Options

  * `-t, --tag` - Image tag (e.g., myapp:1.0.0). Required.
  * `-o, --output` - Output tarball path (default: <tag>.tar.gz)
  * `-p, --push` - Push to registry (e.g., ghcr.io/myorg)
  * `-d, --desc` - Image description (OCI manifest annotation)
  * `--base` - Override base image
  * `--release` - Release name (if multiple configured)

## Configuration

Add to your `rebar.config`:

```
{ocibuild, [
    {base_image, "debian:slim"},
    {workdir, "/app"},
    {env, #{<<"LANG">> => <<"C.UTF-8">>}},
    {expose, [8080]},
    {labels, #{}},
    {description, "My awesome application"}
]}.
```

## Authentication

Set environment variables for registry authentication:

For pushing to registry:

```
export OCIBUILD_PUSH_USERNAME="user"
export OCIBUILD_PUSH_PASSWORD="pass"
```

For pulling private base images (optional, anonymous pull used if unset):

```
export OCIBUILD_PULL_USERNAME="user"
export OCIBUILD_PULL_PASSWORD="pass"
```
""".

%% Note: The provider behaviour is part of rebar3's internal API and is only
%% available at runtime when used as a rebar3 plugin. The "behaviour provider
%% undefined" warning during standalone compilation is expected and harmless.
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%% Exported for use by Mix task (Elixir integration)
%% These delegate to ocibuild_release but are kept for backward compatibility
-export([collect_release_files/1, build_image/7, build_image/8, get_push_auth/0, get_pull_auth/0]).

%% Exports for testing
-ifdef(TEST).
-export([format_bytes/1, format_progress/2, parse_tag/1]).
-export([find_relx_release/1, get_base_image/2, make_progress_callback/0]).
-endif.

-define(PROVIDER, ocibuild).
-define(DEPS, [release]).
-define(DEFAULT_BASE_IMAGE, ~"debian:stable-slim").
-define(DEFAULT_WORKDIR, ~"/app").

%%%===================================================================
%%% Provider callbacks
%%%===================================================================

-doc "Initialize the provider and register CLI options.".
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {desc, "Build OCI container images from Erlang releases"},
            {short_desc, "Build OCI images"},
            {example, "rebar3 ocibuild -t myapp:1.0.0"},
            {opts, [
                {tag, $t, "tag", string, "Image tag (e.g., myapp:1.0.0)"},
                {output, $o, "output", string, "Output tarball path"},
                {push, $p, "push", string, "Push to registry (e.g., ghcr.io/myorg)"},
                {base, undefined, "base", string, "Override base image"},
                {release, undefined, "release", string, "Release name (if multiple)"},
                {desc, $d, "desc", string, "Image description (manifest annotation)"}
            ]},
            {profiles, [default, prod]}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-doc "Execute the provider - build OCI image from release.".
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, term()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Config = rebar_state:get(State, ocibuild, []),

    %% Get tag (required)
    case proplists:get_value(tag, Args) of
        undefined ->
            {error, {?MODULE, missing_tag}};
        Tag ->
            do_build(State, Args, Config, list_to_binary(Tag))
    end.

-doc "Format error messages for display.".
-spec format_error(term()) -> iolist().
format_error(missing_tag) ->
    "Missing required --tag (-t) option. Usage: rebar3 ocibuild -t myapp:1.0.0";
format_error({release_not_found, Name, Path}) ->
    io_lib:format("Release '~s' not found at ~s. Run 'rebar3 release' first.", [Name, Path]);
format_error({no_release_configured, RelxConfig}) ->
    io_lib:format("No release configured in rebar.config. Got: ~p", [RelxConfig]);
format_error({file_read_error, Path, Reason}) ->
    io_lib:format("Failed to read file ~s: ~p", [Path, Reason]);
format_error({save_failed, Reason}) ->
    io_lib:format("Failed to save image: ~p", [Reason]);
format_error({push_failed, Reason}) ->
    io_lib:format("Failed to push image: ~p", [Reason]);
format_error({base_image_failed, Reason}) ->
    io_lib:format("Failed to fetch base image: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("OCI build error: ~p", [Reason]).

%%%===================================================================
%%% Delegated API (for backward compatibility with Mix tasks)
%%%===================================================================

-doc "Collect all files from a release directory. Delegates to ocibuild_release.".
-spec collect_release_files(file:filename()) ->
    {ok, [{binary(), binary(), non_neg_integer()}]} | {error, term()}.
collect_release_files(ReleasePath) ->
    ocibuild_release:collect_release_files(ReleasePath).

-doc "Build an OCI image from release files. Delegates to ocibuild_release.".
-spec build_image(
    binary(),
    [{binary(), binary(), non_neg_integer()}],
    string() | binary(),
    binary(),
    map(),
    [non_neg_integer()],
    map()
) -> {ok, ocibuild:image()} | {error, term()}.
build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels) ->
    build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, ~"foreground").

-doc "Build an OCI image from release files with custom start command. Delegates to ocibuild_release.".
-spec build_image(
    binary(),
    [{binary(), binary(), non_neg_integer()}],
    string() | binary(),
    binary(),
    map(),
    [non_neg_integer()],
    map(),
    binary()
) -> {ok, ocibuild:image()} | {error, term()}.
build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd) ->
    %% Create progress callback for terminal display
    ProgressFn = make_progress_callback(),
    PullAuth = get_pull_auth(),
    Opts = #{auth => PullAuth, progress => ProgressFn},
    Result = ocibuild_release:build_image(
        BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd, Opts
    ),
    %% Clear progress line after build
    io:format("\r\e[K"),
    Result.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Main build logic
do_build(State, Args, Config, Tag) ->
    rebar_api:info("Building OCI image: ~s", [Tag]),

    %% Find release
    case find_release(State, Args) of
        {ok, ReleaseName, ReleasePath} ->
            rebar_api:info("Using release: ~s at ~s", [ReleaseName, ReleasePath]),

            %% Collect files from release
            case collect_release_files(ReleasePath) of
                {ok, Files} ->
                    rebar_api:info("Collected ~p files from release", [length(Files)]),
                    build_and_output(State, Args, Config, Tag, ReleaseName, Files);
                {error, Reason} ->
                    {error, {?MODULE, Reason}}
            end;
        {error, Reason} ->
            {error, {?MODULE, Reason}}
    end.

%% @private Find the release directory
find_release(State, Args) ->
    RelxConfig = rebar_state:get(State, relx, []),

    %% Get release name from args or config
    case get_release_name(Args, RelxConfig) of
        {ok, ReleaseName} ->
            %% Build the release path
            BaseDir = rebar_dir:base_dir(State),
            ReleasePath = filename:join([BaseDir, "rel", ReleaseName]),

            case filelib:is_dir(ReleasePath) of
                true ->
                    {ok, ReleaseName, ReleasePath};
                false ->
                    {error, {release_not_found, ReleaseName, ReleasePath}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Extract release name from args or relx config
get_release_name(Args, RelxConfig) ->
    case proplists:get_value(release, Args) of
        undefined ->
            %% Try to get from relx config
            case find_relx_release(RelxConfig) of
                {ok, Name} ->
                    {ok, Name};
                error ->
                    {error, {no_release_configured, RelxConfig}}
            end;
        Name ->
            {ok, Name}
    end.

%% @private Find release definition in relx config
find_relx_release([]) ->
    error;
find_relx_release([{release, {Name, _Vsn}, _Apps} | _]) ->
    {ok, atom_to_list(Name)};
find_relx_release([{release, {Name, _Vsn}, _Apps, _Opts} | _]) ->
    {ok, atom_to_list(Name)};
find_relx_release([_ | Rest]) ->
    find_relx_release(Rest).

%% @private Build image and output
build_and_output(State, Args, Config, Tag, ReleaseName, Files) ->
    %% Get configuration options
    BaseImage = get_base_image(Args, Config),
    Workdir = proplists:get_value(workdir, Config, ?DEFAULT_WORKDIR),
    EnvMap = proplists:get_value(env, Config, #{}),
    ExposePorts = proplists:get_value(expose, Config, []),
    Labels = proplists:get_value(labels, Config, #{}),

    rebar_api:info("Base image: ~s", [BaseImage]),

    %% Build the image
    case build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels) of
        {ok, Image0} ->
            %% Add description annotation if provided
            Image = add_description_annotation(Args, Config, Image0),
            output_image(State, Args, Config, Tag, Image);
        {error, Reason} ->
            {error, {?MODULE, Reason}}
    end.

%% @private Get base image from args or config
get_base_image(Args, Config) ->
    case proplists:get_value(base, Args) of
        undefined ->
            proplists:get_value(base_image, Config, ?DEFAULT_BASE_IMAGE);
        Base ->
            list_to_binary(Base)
    end.

%% @private Add description annotation if provided via CLI or config
add_description_annotation(Args, Config, Image) ->
    case proplists:get_value(desc, Args) of
        undefined ->
            %% Check config for description
            case proplists:get_value(description, Config) of
                undefined ->
                    Image;
                Descr ->
                    ocibuild:annotation(
                        Image, ~"org.opencontainers.image.description", list_to_binary(Descr)
                    )
            end;
        Descr ->
            ocibuild:annotation(
                Image, ~"org.opencontainers.image.description", list_to_binary(Descr)
            )
    end.

%% @private Output the image (save and/or push)
output_image(State, Args, Config, Tag, Image) ->
    PushRegistry = proplists:get_value(push, Args),

    %% Determine output path
    OutputPath =
        case proplists:get_value(output, Args) of
            undefined ->
                %% Extract just the image name (last path segment) for the filename
                TagStr = binary_to_list(Tag),
                ImageName = lists:last(string:split(TagStr, "/", all)),
                SafeName = lists:map(
                    fun
                        ($:) -> $-;
                        (C) -> C
                    end,
                    ImageName
                ),
                SafeName ++ ".tar.gz";
            Path ->
                Path
        end,

    %% Save tarball
    rebar_api:info("Saving image to ~s", [OutputPath]),
    SaveOpts = #{tag => Tag},
    case ocibuild:save(Image, OutputPath, SaveOpts) of
        ok ->
            rebar_api:info("Image saved successfully", []),

            %% Push if requested
            case PushRegistry of
                undefined ->
                    rebar_api:console("~nTo load the image:~n  podman load < ~s~n", [OutputPath]),
                    {ok, State};
                _ ->
                    push_image(State, Config, Tag, Image, list_to_binary(PushRegistry))
            end;
        {error, Reason} ->
            {error, {?MODULE, {save_failed, Reason}}}
    end.

%% @private Push image to registry
push_image(State, _Config, Tag, Image, Registry) ->
    %% Parse tag to get repository and tag parts
    {Repo, ImageTag} = parse_tag(Tag),

    %% Get auth from environment (for pushing)
    Auth = get_push_auth(),

    rebar_api:info("Pushing to ~s/~s:~s", [Registry, Repo, ImageTag]),

    case ocibuild:push(Image, Registry, <<Repo/binary, ":", ImageTag/binary>>, Auth) of
        ok ->
            rebar_api:info("Push successful!", []),
            {ok, State};
        {error, Reason} ->
            {error, {?MODULE, {push_failed, Reason}}}
    end.

%% @private Parse tag into repo and tag parts
parse_tag(Tag) ->
    case binary:split(Tag, ~":") of
        [Repo, ImageTag] ->
            {Repo, ImageTag};
        [Repo] ->
            {Repo, ~"latest"}
    end.

%% @private Get authentication for pushing images
-spec get_push_auth() -> map().
get_push_auth() ->
    case os:getenv("OCIBUILD_PUSH_TOKEN") of
        false ->
            case {os:getenv("OCIBUILD_PUSH_USERNAME"), os:getenv("OCIBUILD_PUSH_PASSWORD")} of
                {false, _} ->
                    #{};
                {_, false} ->
                    #{};
                {User, Pass} ->
                    #{username => list_to_binary(User), password => list_to_binary(Pass)}
            end;
        Token ->
            #{token => list_to_binary(Token)}
    end.

%% @private Get authentication for pulling base images
-spec get_pull_auth() -> map().
get_pull_auth() ->
    case os:getenv("OCIBUILD_PULL_TOKEN") of
        false ->
            case {os:getenv("OCIBUILD_PULL_USERNAME"), os:getenv("OCIBUILD_PULL_PASSWORD")} of
                {false, _} ->
                    #{};
                {_, false} ->
                    #{};
                {User, Pass} ->
                    #{username => list_to_binary(User), password => list_to_binary(Pass)}
            end;
        Token ->
            #{token => list_to_binary(Token)}
    end.

%% @private Create a progress callback for terminal display
make_progress_callback() ->
    fun(#{phase := Phase, bytes_received := Received, total_bytes := Total}) ->
        PhaseStr =
            case Phase of
                manifest -> "Fetching manifest";
                config -> "Fetching config  ";
                layer -> "Downloading layer"
            end,
        ProgressStr = format_progress(Received, Total),
        %% Use carriage return to overwrite the line
        io:format("\r\e[K  ~s: ~s", [PhaseStr, ProgressStr])
    end.

%% @private Format progress as a string with bar
format_progress(Received, unknown) ->
    io_lib:format("~s", [format_bytes(Received)]);
format_progress(Received, Total) when is_integer(Total), Total > 0 ->
    Percent = min(100, (Received * 100) div Total),
    BarWidth = 30,
    Filled = (Percent * BarWidth) div 100,
    Empty = BarWidth - Filled,
    Bar = lists:duplicate(Filled, $=) ++ lists:duplicate(Empty, $\s),
    io_lib:format("[~s] ~3B% ~s/~s", [Bar, Percent, format_bytes(Received), format_bytes(Total)]);
format_progress(Received, _) ->
    io_lib:format("~s", [format_bytes(Received)]).

%% @private Format bytes as human-readable string
format_bytes(Bytes) when Bytes < 1024 ->
    io_lib:format("~B B", [Bytes]);
format_bytes(Bytes) when Bytes < 1024 * 1024 ->
    io_lib:format("~.1f KB", [Bytes / 1024]);
format_bytes(Bytes) when Bytes < 1024 * 1024 * 1024 ->
    io_lib:format("~.1f MB", [Bytes / (1024 * 1024)]);
format_bytes(Bytes) ->
    io_lib:format("~.2f GB", [Bytes / (1024 * 1024 * 1024)]).
