-module(ocibuild).
-moduledoc """
`ocibuild` - Build and publish OCI container images from the BEAM.

This module provides the public API for building OCI-compliant
container images without requiring Docker or any container runtime.

# Quick Start

```
%% Build from a base image
{ok, Image0} = ocibuild:from(~"docker.io/library/alpine:3.19"),

%% Add your application
Image1 = ocibuild:copy(Image0, [{~"myapp", AppBinary}], ~"/app"),

%% Configure the container
Image2 = ocibuild:entrypoint(Image1, [~"/app/myapp", ~"start"]),
Image3 = ocibuild:env(Image2, #{~"MIX_ENV" => ~"prod"}),

%% Push to a registry (GHCR uses username + token as password)
Auth = #{username => ~"github-username", password => ~"github-token"},
ok = ocibuild:push(Image3, ~"ghcr.io", ~"myorg/myapp:v1", Auth).
```

# Memory Requirements

This library processes layers entirely in memory. Ensure your VM has
sufficient memory to hold your release files, compressed layer data, and
any downloaded base image layers. As a rule of thumb, allocate at least
2x your release size plus base image layers. For very large images (>1 GB),
consider breaking content into multiple smaller layers or increasing VM
memory limits.
""".

%% API - Building images
-export([from/1, from/2, from/3, scratch/0]).
%% API - Adding content
-export([add_layer/2, add_layer/3, copy/3]).
%% API - Configuration
-export([entrypoint/2, cmd/2, env/2, workdir/2, expose/2, label/3, user/2, annotation/3]).
%% API - Output
-export([push/3, push/4, push/5, push_multi/4, push_multi/5, save/2, save/3, export/2]).

%% rebar3 plugin entry point
-export([init/1]).

%% Types
-export_type([image/0, layer/0, auth/0, base_ref/0, platform/0]).

%% Platform utilities
-export([parse_platform/1, parse_platforms/1]).

%% Image type - use API functions to construct and modify.
%% Note: Changed from -opaque to -type for eqWalizer compatibility.
%% While this allows direct pattern matching, prefer using the API functions
%% (from/1, copy/3, env/2, etc.) to ensure consistent image construction.
-type image() ::
    #{
        base := base_ref() | none,
        base_manifest => map(),
        base_config => map(),
        auth => auth() | #{},
        layers := [layer()],
        config := map(),
        annotations => map(),
        platform => platform()
    }.

-type base_ref() :: {Registry :: binary(), Repo :: binary(), Ref :: binary()}.
-type layer() ::
    #{
        media_type := binary(),
        digest := binary(),
        diff_id := binary(),
        size := non_neg_integer(),
        data := binary()
    }.
-type auth() :: #{username := binary(), password := binary()} | #{token := binary()}.

-type platform() ::
    #{
        os := binary(),
        architecture := binary(),
        variant => binary(),
        os_version => binary()
    }.

%%%===================================================================
%%% API - Building images
%%%===================================================================

-doc """
Start building an image from a base image reference.
The reference can be either a binary string or a tuple:
```
%% As a string (will be parsed)
{ok, Image} = ocibuild:from(~"docker.io/library/alpine:3.19").

%% As a tuple
{ok, Image} = ocibuild:from({~"ghcr.io", ~"hexpm/elixir", ~"1.16"}).
```
""".
-spec from(binary() | base_ref()) -> {ok, image()} | {error, term()}.
from(Ref) when is_binary(Ref) ->
    case parse_image_ref(Ref) of
        {ok, ParsedRef} ->
            from(ParsedRef);
        {error, _} = Err ->
            Err
    end;
from({Registry, Repo, Tag} = Ref) when
    is_binary(Registry), is_binary(Repo), is_binary(Tag)
->
    %% Fetch the base image manifest and config from registry
    case ocibuild_registry:pull_manifest(Registry, Repo, Tag) of
        {ok, Manifest, Config} ->
            {ok, #{
                base => Ref,
                base_manifest => Manifest,
                base_config => Config,
                layers => [],
                config => init_config(Config)
            }};
        {error, _} = Err ->
            Err
    end.

-doc """
Start building an image from a base image with authentication.
""".
-spec from(binary() | base_ref(), auth()) -> {ok, image()} | {error, term()}.
from(Ref, Auth) when is_binary(Ref) ->
    case parse_image_ref(Ref) of
        {ok, ParsedRef} ->
            from(ParsedRef, Auth);
        {error, _} = Err ->
            Err
    end;
from({_Registry, _Repo, _Tag} = Ref, Auth) ->
    from(Ref, Auth, #{}).

-doc """
Start building an image from a base image with authentication and options.

Options:
- `progress`: A callback function `fun(ProgressInfo) -> ok` that receives progress updates.
  ProgressInfo is a map with keys: `phase` (manifest|config|layer), `bytes_received`, `total_bytes`.
- `platform`: A specific platform to pull (e.g., `#{os => ~"linux", architecture => ~"amd64"}`).
- `platforms`: A list of platforms for multi-platform builds. Returns a list of images.

Example with progress callback:
```
Progress = fun(#{phase := Phase, bytes_received := Recv, total_bytes := Total}) ->
    io:format("~p: ~p/~p bytes~n", [Phase, Recv, Total])
end,
{ok, Image} = ocibuild:from(~"alpine:3.19", #{}, #{progress => Progress}).
```

Example with multiple platforms (returns list of images):
```
{ok, Images} = ocibuild:from(~"alpine:3.19", #{}, #{
    platforms => [#{os => ~"linux", architecture => ~"amd64"},
                  #{os => ~"linux", architecture => ~"arm64"}]
}).
```
""".
-spec from(binary() | base_ref(), auth(), map()) ->
    {ok, image()} | {ok, [image()]} | {error, term()}.
from(Ref, Auth, Opts) when is_binary(Ref) ->
    case parse_image_ref(Ref) of
        {ok, ParsedRef} ->
            from(ParsedRef, Auth, Opts);
        {error, _} = Err ->
            Err
    end;
from({_Registry, _Repo, _Tag} = Ref, Auth, Opts) ->
    case maps:find(platforms, Opts) of
        {ok, Platforms} when is_list(Platforms), length(Platforms) > 0 ->
            %% Multi-platform: pull each platform separately
            from_multi_platform(Ref, Auth, Opts, Platforms);
        _ ->
            %% Single platform (possibly with explicit platform option)
            from_single_platform(Ref, Auth, Opts)
    end.

%% @private Pull a single platform image
-spec from_single_platform(base_ref(), auth(), map()) -> {ok, image()} | {error, term()}.
from_single_platform({Registry, Repo, Tag} = Ref, Auth, Opts) ->
    case ocibuild_registry:pull_manifest(Registry, Repo, Tag, Auth, Opts) of
        {ok, Manifest, Config} ->
            Platform =
                case maps:find(platform, Opts) of
                    {ok, P} ->
                        P;
                    error ->
                        %% Extract from config if available
                        #{
                            os => maps:get(~"os", Config, ~"linux"),
                            architecture => maps:get(~"architecture", Config, ~"amd64")
                        }
                end,
            {ok, #{
                base => Ref,
                base_manifest => Manifest,
                base_config => Config,
                auth => Auth,
                layers => [],
                config => init_config(Config),
                platform => Platform
            }};
        {error, _} = Err ->
            Err
    end.

%% @private Pull multiple platform images
-spec from_multi_platform(base_ref(), auth(), map(), [platform()]) ->
    {ok, [image()]} | {error, term()}.
from_multi_platform({Registry, Repo, Tag} = Ref, Auth, _Opts, Platforms) ->
    case ocibuild_registry:pull_manifests_for_platforms(Registry, Repo, Tag, Auth, Platforms) of
        {ok, PlatformResults} ->
            Images = [
                #{
                    base => Ref,
                    base_manifest => Manifest,
                    base_config => Config,
                    auth => Auth,
                    layers => [],
                    config => init_config(Config),
                    platform => Platform
                }
             || {Platform, Manifest, Config} <- PlatformResults
            ],
            {ok, Images};
        {error, _} = Err ->
            Err
    end.

-doc """
Start building an image from scratch (no base image).
Use this when you want complete control over the image contents,
typically for statically compiled binaries.
```
{ok, Image} = ocibuild:scratch(),
Image1 = ocibuild:copy(Image, [{~"myapp", Binary}], ~"/"),
Image2 = ocibuild:entrypoint(Image1, [~"/myapp"]).
```
""".
-spec scratch() -> {ok, image()}.
scratch() ->
    {ok, #{
        base => none,
        layers => [],
        config =>
            #{
                ~"created" => iso8601_now(),
                ~"architecture" => ~"amd64",
                ~"os" => ~"linux",
                ~"config" => #{},
                ~"rootfs" => #{~"type" => ~"layers", ~"diff_ids" => []},
                ~"history" => []
            }
    }}.

%%%===================================================================
%%% API - Adding content
%%%===================================================================

-doc """
Add a layer to the image from a list of files.
Files are specified as `{Path, Content, Mode}` tuples:
```
Image1 = ocibuild:add_layer(Image, [
    {~"/app/myapp", AppBinary, 8#755},
    {~"/app/config.json", ConfigJson, 8#644}
]).
```
""".
-spec add_layer(image(), [{Path :: binary(), Content :: binary(), Mode :: integer()}]) ->
    image().
add_layer(Image, Files) ->
    add_layer(Image, Files, #{}).

-doc """
Add a layer to the image from a list of files with options.

Options:
- `layer_type`: Type of layer content (erts, deps, app) for progress display
- `compression`: Compression algorithm (`gzip`, `zstd`, or `auto`). Default: `auto`

```
Image1 = ocibuild:add_layer(Image, Files, #{layer_type => app}).

%% With explicit compression
Image2 = ocibuild:add_layer(Image, Files, #{compression => zstd}).
```

Raises an error if compression fails (e.g., zstd requested on OTP 27).
""".
-spec add_layer(image(), [{Path :: binary(), Content :: binary(), Mode :: integer()}], map()) ->
    image().
add_layer(#{layers := Layers, config := Config} = Image, Files, Opts) ->
    %% Get reproducible timestamp for layer mtime
    MTime = ocibuild_time:get_timestamp(),
    LayerOpts = Opts#{mtime => MTime},
    case ocibuild_layer:create(Files, LayerOpts) of
        {ok, Layer} ->
            NewConfig = add_layer_to_config(Config, Layer),
            %% Prepend for O(1) - layers are stored in reverse order, reversed on export
            Image#{layers := [Layer | Layers], config := NewConfig};
        {error, Reason} ->
            erlang:error(Reason)
    end.

-doc """
Copy files to a destination directory in the image.
This is a convenience function that creates a layer with files
placed under the specified destination path:
```
Image1 = ocibuild:copy(Image, [
    {~"myapp", AppBinary},
    {~"config.json", ConfigJson}
], ~"/app").
```
Files will be created as `/app/myapp` and `/app/config.json`.
""".
-spec copy(image(), [{Name :: binary(), Content :: binary()}], Dest :: binary()) ->
    image().
copy(Image, Files, Dest) ->
    LayerFiles =
        lists:map(
            fun({Name, Content}) ->
                Path = filename:join(Dest, Name),
                {Path, Content, 8#644}
            end,
            Files
        ),
    add_layer(Image, LayerFiles).

%%%===================================================================
%%% API - Configuration
%%%===================================================================

-doc """
Set the entrypoint for the container.
The entrypoint is the command that will be executed when the container starts.
```
Image1 = ocibuild:entrypoint(Image, [~"/app/myapp", ~"start"]).
```
""".
-spec entrypoint(image(), [binary()]) -> image().
entrypoint(#{config := Config} = Image, Entrypoint) when is_list(Entrypoint) ->
    Image#{config := set_config_field(Config, ~"Entrypoint", Entrypoint)}.

-doc """
Set the default command arguments.
CMD provides default arguments to the entrypoint:
```
Image1 = ocibuild:cmd(Image, [~"--port", ~"8080"]).
```
""".
-spec cmd(image(), [binary()]) -> image().
cmd(#{config := Config} = Image, Cmd) when is_list(Cmd) ->
    Image#{config := set_config_field(Config, ~"Cmd", Cmd)}.

-doc """
Set environment variables.
Environment variables are specified as a map:
```
Image1 = ocibuild:env(Image, #{
    ~"MIX_ENV" => ~"prod",
    ~"PORT" => ~"4000"
}).
```
""".
-spec env(image(), #{binary() => binary()}) -> image().
env(#{config := Config} = Image, EnvMap) when is_map(EnvMap) ->
    EnvList =
        maps:fold(
            fun(K, V, Acc) -> [<<K/binary, "=", V/binary>> | Acc] end,
            get_config_field(Config, ~"Env", []),
            EnvMap
        ),
    Image#{config := set_config_field(Config, ~"Env", EnvList)}.

-doc "Set the working directory.".
-spec workdir(image(), binary()) -> image().
workdir(#{config := Config} = Image, Dir) when is_binary(Dir) ->
    Image#{config := set_config_field(Config, ~"WorkingDir", Dir)}.

-doc "Expose a port.".
-spec expose(image(), integer() | binary()) -> image().
expose(#{config := Config} = Image, Port) when is_integer(Port) ->
    expose(Image#{config := Config}, integer_to_binary(Port));
expose(#{config := Config} = Image, Port) when is_binary(Port) ->
    ExposedPorts = get_config_field(Config, ~"ExposedPorts", #{}),
    PortKey = <<Port/binary, "/tcp">>,
    NewExposed = ExposedPorts#{PortKey => #{}},
    Image#{config := set_config_field(Config, ~"ExposedPorts", NewExposed)}.

-doc "Add a label to the image config.".
-spec label(image(), binary(), binary()) -> image().
label(#{config := Config} = Image, Key, Value) when is_binary(Key), is_binary(Value) ->
    Labels = get_config_field(Config, ~"Labels", #{}),
    Image#{config := set_config_field(Config, ~"Labels", Labels#{Key => Value})}.

-doc """
Add an annotation to the image manifest.

Annotations are key-value metadata stored in the manifest (not the config).
Some registries like GHCR display these on the package page.

Common OCI annotations:
- `org.opencontainers.image.description` - Human-readable description
- `org.opencontainers.image.source` - URL to source code
- `org.opencontainers.image.version` - Version of the packaged software
- `org.opencontainers.image.authors` - Contact details of maintainers
""".
-spec annotation(image(), binary(), binary()) -> image().
annotation(Image, Key, Value) when is_binary(Key), is_binary(Value) ->
    Annotations = maps:get(annotations, Image, #{}),
    Image#{annotations => Annotations#{Key => Value}}.

-doc "Set the user to run as.".
-spec user(image(), binary()) -> image().
user(#{config := Config} = Image, User) when is_binary(User) ->
    Image#{config := set_config_field(Config, ~"User", User)}.

%%%===================================================================
%%% API - Output
%%%===================================================================

-doc """
Push the image to a container registry.

```
{ok, Digest} = ocibuild:push(Image, ~"ghcr.io", ~"myorg/myapp:v1.0.0").
```

Returns `{ok, Digest}` where Digest is the sha256 digest of the pushed manifest.
""".
-spec push(image(), Registry :: binary(), RepoTag :: binary()) ->
    {ok, Digest :: binary()} | {error, term()}.
push(Image, Registry, RepoTag) ->
    push(Image, Registry, RepoTag, #{}).

-doc """
Push the image to a container registry with authentication.

```
%% GHCR uses username + token as password
Auth = #{username => ~"github-user", password => ~"github-token"},
{ok, Digest} = ocibuild:push(Image, ~"ghcr.io", ~"myorg/myapp:v1.0.0", Auth).
```

Returns `{ok, Digest}` where Digest is the sha256 digest of the pushed manifest.
""".
-spec push(image(), Registry :: binary(), RepoTag :: binary(), auth()) ->
    {ok, Digest :: binary()} | {error, term()}.
push(Image, Registry, RepoTag, Auth) ->
    push(Image, Registry, RepoTag, Auth, #{}).

-doc """
Push the image to a container registry with authentication and options.

Options:
- `chunk_size`: Size in bytes for chunked uploads (default: 5MB). Blobs >= this size
  use OCI chunked upload. Set to a large value to disable chunked uploads.
- `progress`: A callback function for upload progress updates.

Example:
```
Auth = #{username => ~"user", password => ~"pass"},
Opts = #{chunk_size => 10 * 1024 * 1024},  % 10MB chunks
{ok, Digest} = ocibuild:push(Image, ~"ghcr.io", ~"myorg/myapp:v1", Auth, Opts).
```

Returns `{ok, Digest}` where Digest is the sha256 digest of the pushed manifest.
""".
-spec push(image(), Registry :: binary(), RepoTag :: binary(), auth(), map()) ->
    {ok, Digest :: binary()} | {error, term()}.
push(Image, Registry, RepoTag, Auth, Opts) ->
    {Repo, Tag} = parse_repo_tag(RepoTag),
    ocibuild_registry:push(Image, Registry, Repo, Tag, Auth, Opts).

-doc """
Push multiple platform-specific images as a single multi-platform image.

This function pushes all images and creates an OCI image index that references
them. When clients pull the image, they receive the appropriate platform variant.

Each image in the list must have a `platform` field set.

Example:
```
%% Build for multiple platforms
{ok, Images} = ocibuild:from(~"alpine:3.19", #{}, #{
    platforms => [#{os => ~"linux", architecture => ~"amd64"},
                  #{os => ~"linux", architecture => ~"arm64"}]
}),
%% Add layers to each image
Images2 = [ocibuild:copy(I, Files, ~"/app") || I <- Images],
%% Push as multi-platform image
Auth = #{username => ~"user", password => ~"pass"},
{ok, Digest} = ocibuild:push_multi(Images2, ~"ghcr.io", ~"myorg/myapp:v1", Auth).
```

Returns `{ok, Digest}` where Digest is the sha256 digest of the pushed image index.
""".
-spec push_multi([image()], Registry :: binary(), RepoTag :: binary(), auth()) ->
    {ok, Digest :: binary()} | {error, term()}.
push_multi(Images, Registry, RepoTag, Auth) ->
    push_multi(Images, Registry, RepoTag, Auth, #{}).

-doc """
Push multiple images as a multi-platform image with options.

Returns `{ok, Digest}` where Digest is the sha256 digest of the pushed image index.
""".
-spec push_multi([image()], Registry :: binary(), RepoTag :: binary(), auth(), map()) ->
    {ok, Digest :: binary()} | {error, term()}.
push_multi(Images, Registry, RepoTag, Auth, Opts) ->
    {Repo, Tag} = parse_repo_tag(RepoTag),
    ocibuild_registry:push_multi(Images, Registry, Repo, Tag, Auth, Opts).

-doc """
Save the image as a tarball.

The resulting tarball can be loaded with `docker load` or `podman load`:
```
ok = ocibuild:save(Image, "myimage.tar.gz").
ok = ocibuild:save(Image, "myimage.tar.gz", #{tag => ~"myapp:1.0"}).
```

Options:
- `tag`: Image tag for Docker format (required for proper image naming)
- `format`: `docker` (default) or `oci`
""".
-spec save(image(), file:filename()) -> ok | {error, term()}.
save(Image, Path) ->
    save(Image, Path, #{}).

-spec save(image(), file:filename(), map()) -> ok | {error, term()}.
save(Image, Path, Opts) ->
    ocibuild_layout:save_tarball(Image, Path, Opts).

-doc """
Export the image as an OCI layout directory.

Creates the standard OCI directory structure:
```
ok = ocibuild:export(Image, "./myimage").
%% Creates:
%%   ./myimage/oci-layout
%%   ./myimage/index.json
%%   ./myimage/blobs/sha256/...
```
""".
-spec export(image(), file:filename()) -> ok | {error, term()}.
export(Image, Path) ->
    ocibuild_layout:export_directory(Image, Path).

%%%===================================================================
%%% rebar3 plugin dispatch
%%%===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    ocibuild_rebar3:init(State).

%%%===================================================================
%%% Platform utilities
%%%===================================================================

-doc """
Parse a platform string into a platform map.

Accepts formats like:
- "linux/amd64"
- "linux/arm64"
- "linux/arm64/v8" (with variant)

```
{ok, #{os := ~"linux", architecture := ~"amd64"}} =
    ocibuild:parse_platform(~"linux/amd64").
```
""".
-spec parse_platform(binary()) -> {ok, platform()} | {error, term()}.
parse_platform(PlatformStr) when is_binary(PlatformStr) ->
    case binary:split(PlatformStr, ~"/", [global]) of
        [Os, Arch] when byte_size(Os) > 0, byte_size(Arch) > 0 ->
            {ok, #{os => Os, architecture => Arch}};
        [Os, Arch, Variant] when byte_size(Os) > 0, byte_size(Arch) > 0, byte_size(Variant) > 0 ->
            {ok, #{os => Os, architecture => Arch, variant => Variant}};
        _ ->
            {error, {invalid_platform, PlatformStr}}
    end;
parse_platform(PlatformStr) when is_list(PlatformStr) ->
    parse_platform(list_to_binary(PlatformStr)).

-doc """
Parse a comma-separated list of platforms.

```
{ok, [#{os := ~"linux", architecture := ~"amd64"},
      #{os := ~"linux", architecture := ~"arm64"}]} =
    ocibuild:parse_platforms(~"linux/amd64,linux/arm64").
```
""".
-spec parse_platforms(binary()) -> {ok, [platform()]} | {error, term()}.
parse_platforms(PlatformsStr) when is_binary(PlatformsStr) ->
    Parts = binary:split(PlatformsStr, ~",", [global, trim_all]),
    parse_platforms_list(Parts, []);
parse_platforms(PlatformsStr) when is_list(PlatformsStr) ->
    parse_platforms(list_to_binary(PlatformsStr)).

-spec parse_platforms_list([binary()], [platform()]) -> {ok, [platform()]} | {error, term()}.
parse_platforms_list([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_platforms_list([P | Rest], Acc) ->
    case parse_platform(P) of
        {ok, Platform} ->
            parse_platforms_list(Rest, [Platform | Acc]);
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_image_ref(binary()) -> {ok, base_ref()} | {error, invalid_ref}.
parse_image_ref(Ref) ->
    %% Parse references like:
    %% - "alpine:3.19" -> {~"docker.io", ~"library/alpine", ~"3.19"}
    %% - "docker.io/library/alpine:3.19"
    %% - "ghcr.io/myorg/myapp:v1"
    %% - "myregistry.com:5000/myapp:latest"
    case binary:split(Ref, ~"/", [global]) of
        [Image] ->
            %% Simple case: "alpine:3.19" -> Docker Hub library
            {Name, Tag} = split_name_tag(Image),
            {ok, {~"docker.io", <<"library/", Name/binary>>, Tag}};
        [First | Rest] ->
            case has_registry_chars(First) of
                true ->
                    %% First part is a registry
                    RepoAndTag = iolist_to_binary(lists:join(~"/", Rest)),
                    {Name, Tag} = split_name_tag(RepoAndTag),
                    {ok, {First, Name, Tag}};
                false ->
                    %% No registry, assume Docker Hub
                    {Name, Tag} = split_name_tag(Ref),
                    {ok, {~"docker.io", Name, Tag}}
            end
    end.

-spec split_name_tag(binary()) -> {binary(), binary()}.
split_name_tag(NameTag) ->
    case binary:split(NameTag, ~":", [global]) of
        [Name] ->
            {Name, ~"latest"};
        [Name, Tag] ->
            {Name, Tag};
        [Name | TagParts] ->
            %% Handle case with port: "registry:5000/repo:tag"
            %% Use reverse pattern for O(n) instead of last+droplast O(2n)
            [Tag | RevRest] = lists:reverse(TagParts),
            NameParts = [Name | lists:reverse(RevRest)],
            {iolist_to_binary(lists:join(~":", NameParts)), Tag}
    end.

-spec has_registry_chars(binary()) -> boolean().
has_registry_chars(Part) ->
    %% A registry typically has a dot or colon (for port)
    binary:match(Part, ~".") =/= nomatch orelse
        binary:match(Part, ~":") =/= nomatch orelse
        Part =:= ~"localhost".

-spec parse_repo_tag(binary()) -> {binary(), binary()}.
parse_repo_tag(RepoTag) ->
    case binary:split(RepoTag, ~":", [global]) of
        [Repo] ->
            {Repo, ~"latest"};
        Parts ->
            %% Use reverse pattern for O(n) instead of last+droplast O(2n)
            [Tag | RevRest] = lists:reverse(Parts),
            Repo = iolist_to_binary(lists:join(~":", lists:reverse(RevRest))),
            {Repo, Tag}
    end.

-spec init_config(map()) -> map().
init_config(BaseConfig) ->
    %% Initialize config from base, preserving architecture/os
    %% Base diff_ids and history come in forward order from registry,
    %% but we store in reverse order for O(1) prepend (reversed on export)
    BaseRootfs = maps:get(~"rootfs", BaseConfig, #{~"type" => ~"layers", ~"diff_ids" => []}),
    BaseDiffIds = maps:get(~"diff_ids", BaseRootfs, []),
    BaseHistory = maps:get(~"history", BaseConfig, []),
    #{
        ~"created" => iso8601_now(),
        ~"architecture" => maps:get(~"architecture", BaseConfig, ~"amd64"),
        ~"os" => maps:get(~"os", BaseConfig, ~"linux"),
        ~"config" => maps:get(~"config", BaseConfig, #{}),
        ~"rootfs" => BaseRootfs#{~"diff_ids" => lists:reverse(BaseDiffIds)},
        ~"history" => lists:reverse(BaseHistory)
    }.

-spec add_layer_to_config(map(), layer()) -> map().
add_layer_to_config(Config, #{diff_id := DiffId}) ->
    Rootfs = maps:get(~"rootfs", Config),
    DiffIds = maps:get(~"diff_ids", Rootfs, []),
    %% Prepend for O(1) - stored in reverse order, reversed on export
    NewRootfs = Rootfs#{~"diff_ids" => [DiffId | DiffIds]},

    History = maps:get(~"history", Config, []),
    %% Prepend for O(1) - stored in reverse order, reversed on export
    NewHistory = [#{~"created" => iso8601_now(), ~"created_by" => ~"ocibuild"} | History],

    Config#{~"rootfs" => NewRootfs, ~"history" => NewHistory}.

-spec set_config_field(map(), binary(), term()) -> map().
set_config_field(Config, Field, Value) ->
    InnerConfig = maps:get(~"config", Config, #{}),
    Config#{~"config" => InnerConfig#{Field => Value}}.

-spec get_config_field(map(), binary(), term()) -> term().
get_config_field(Config, Field, Default) ->
    InnerConfig = maps:get(~"config", Config, #{}),
    maps:get(Field, InnerConfig, Default).

-spec iso8601_now() -> binary().
iso8601_now() ->
    %% Uses SOURCE_DATE_EPOCH if set for reproducible builds
    ocibuild_time:get_iso8601().
