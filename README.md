# ocibuild

[![Hex.pm](https://img.shields.io/hexpm/v/ocibuild.svg)](https://hex.pm/packages/ocibuild)
[![Hex.pm](https://img.shields.io/hexpm/l/ocibuild.svg)](https://hex.pm/packages/ocibuild)

Build and publish OCI container images from the BEAM — no Docker required.

ocibuild is a pure Erlang library for building OCI-compliant container images
programmatically. It works from any BEAM language (Erlang, Elixir, Gleam, LFE)
and has **zero dependencies** outside OTP 27+.

## Features

- 🚀 **No Docker daemon required** — builds images directly
- 📦 **Push to any registry** — Docker Hub, GHCR, ECR, GCR, etc.
- 🔧 **Pure Erlang** — works from any BEAM language
- 🪶 **Zero dependencies** — only OTP 27+ required
- 📋 **OCI compliant** — produces standard OCI image layouts

## Installation

### Erlang (rebar3)

```erlang
{deps, [
    {ocibuild, "~> 0.1"}
]}.
```

### Elixir (mix)

```elixir
def deps do
  [
    {:ocibuild, "~> 0.1"}
  ]
end
```

## Quick Start

### Mix Task (Elixir)

The easiest way to use ocibuild with Elixir:

```elixir
# mix.exs
def deps do
  [{:ocibuild, "~> 0.1"}]
end

def project do
  [
    # ...
    ocibuild: [
      base_image: "debian:slim",
      env: %{"LANG" => "C.UTF-8"},
      expose: [8080]
    ]
  ]
end
```

```bash
# Build release and create OCI image
MIX_ENV=prod mix release
MIX_ENV=prod mix ocibuild -t myapp:1.0.0

# Load into Docker
docker load < myapp-1.0.0.tar.gz

# Or push directly to a registry
OCIBUILD_TOKEN=$GITHUB_TOKEN mix ocibuild -t myapp:1.0.0 --push -r ghcr.io/myorg
```

#### Automatic Release Step

You can also build OCI images automatically during `mix release`:

```elixir
# mix.exs
releases: [
  myapp: [
    steps: [:assemble, &Ocibuild.MixRelease.build_image/1]
  ]
]
```

Then simply run `MIX_ENV=prod mix release` and the OCI image is built automatically.

### Rebar3 Plugin (Erlang)

The easiest way to use ocibuild with Erlang:

```erlang
%% rebar.config
{deps, [{ocibuild, "~> 0.1"}]}.

{ocibuild, [
    {base_image, "debian:slim"},
    {env, #{<<"LANG">> => <<"C.UTF-8">>}},
    {expose, [8080]}
]}.
```

```bash
# Build release and create OCI image
rebar3 ocibuild -t myapp:1.0.0

# Load into Docker
docker load < myapp-1.0.0.tar.gz

# Or push directly to a registry
OCIBUILD_TOKEN=$GITHUB_TOKEN rebar3 ocibuild -t myapp:1.0.0 --push -r ghcr.io/myorg
```

### Programmatic API (Erlang)

```erlang
%% Build from a base image
{ok, Image0} = ocibuild:from(<<"docker.io/library/alpine:3.19">>),

%% Add your application
{ok, AppBinary} = file:read_file("_build/prod/rel/myapp/myapp"),
Image1 = ocibuild:copy(Image0, [{<<"myapp">>, AppBinary}], <<"/app">>),

%% Configure the container
Image2 = ocibuild:entrypoint(Image1, [<<"/app/myapp">>, <<"start">>]),
Image3 = ocibuild:env(Image2, #{<<"MIX_ENV">> => <<"prod">>}),

%% Push to a registry
ok = ocibuild:push(Image3, <<"ghcr.io">>, <<"myorg/myapp:v1.0.0">>,
                     #{token => os:getenv("GITHUB_TOKEN")}).

%% Or save as a tarball for docker load
ok = ocibuild:save(Image3, "myapp.tar.gz").
```

### Elixir

```elixir
# Build from a base image
{:ok, image} = :ocibuild.from("docker.io/library/alpine:3.19")

# Add your application
{:ok, app_binary} = File.read("_build/prod/rel/myapp/bin/myapp")
image = :ocibuild.copy(image, [{"myapp", app_binary}], "/app")

# Configure the container
image = :ocibuild.entrypoint(image, ["/app/myapp", "start"])
image = :ocibuild.env(image, %{"MIX_ENV" => "prod"})

# Push to a registry
:ok = :ocibuild.push(image, "ghcr.io", "myorg/myapp:v1.0.0",
                       %{token: System.get_env("GITHUB_TOKEN")})
```

## Rebar3 Plugin Reference

### CLI Options

| Option | Short | Description |
|--------|-------|-------------|
| `--tag` | `-t` | Image tag (required), e.g., `myapp:1.0.0` |
| `--registry` | `-r` | Registry for push, e.g., `ghcr.io` |
| `--output` | `-o` | Output tarball path (default: `<tag>.tar.gz`) |
| `--push` | | Push to registry after build |
| `--base` | | Override base image |
| `--release` | | Release name (if multiple configured) |

### Configuration (`rebar.config`)

```erlang
{ocibuild, [
    {base_image, "debian:slim"},           % Base image (default: debian:slim)
    {registry, "docker.io"},               % Default registry for --push
    {workdir, "/app"},                     % Working directory in container
    {env, #{                               % Environment variables
        <<"LANG">> => <<"C.UTF-8">>
    }},
    {expose, [8080, 443]},                 % Ports to expose
    {labels, #{                            % Image labels
        <<"org.opencontainers.image.source">> => <<"https://github.com/...">>
    }}
]}.
```

### Authentication

Set environment variables for registry authentication:

```bash
# Token-based (GitHub, etc.)
export OCIBUILD_TOKEN="your-token"

# Username/password
export OCIBUILD_USERNAME="user"
export OCIBUILD_PASSWORD="pass"
```

## Mix Task Reference (Elixir)

### CLI Options

| Option | Short | Description |
|--------|-------|-------------|
| `--tag` | `-t` | Image tag, e.g., `myapp:1.0.0`. Defaults to `release:version` |
| `--registry` | `-r` | Registry for push, e.g., `ghcr.io` |
| `--output` | `-o` | Output tarball path (default: `<tag>.tar.gz`) |
| `--push` | | Push to registry after build |
| `--base` | | Override base image |
| `--release` | | Release name (if multiple configured) |

### Configuration (`mix.exs`)

```elixir
def project do
  [
    app: :myapp,
    version: "1.0.0",
    # ...
    ocibuild: [
      base_image: "debian:slim",           # Base image (default: debian:slim)
      tag: "myapp:1.0.0",                  # Optional, defaults to app:version
      registry: "ghcr.io/myorg",           # Default registry for --push
      workdir: "/app",                     # Working directory in container
      env: %{"LANG" => "C.UTF-8"},         # Environment variables
      expose: [8080, 443],                 # Ports to expose
      labels: %{                           # Image labels
        "org.opencontainers.image.source" => "https://github.com/..."
      },
      push: false                          # Auto-push (for release step)
    ]
  ]
end
```

## Programmatic API Reference

### Building Images

| Function | Description |
|----------|-------------|
| `from/1`, `from/2` | Start from a base image |
| `scratch/0` | Start from an empty image |

### Adding Content

| Function | Description |
|----------|-------------|
| `add_layer/2` | Add a layer from files with modes |
| `copy/3` | Copy files to a destination directory |

### Configuration

| Function | Description |
|----------|-------------|
| `entrypoint/2` | Set the container entrypoint |
| `cmd/2` | Set default command arguments |
| `env/2` | Set environment variables |
| `workdir/2` | Set working directory |
| `expose/2` | Expose a port |
| `label/3` | Add a label |
| `user/2` | Set the user to run as |

### Output

| Function | Description |
|----------|-------------|
| `push/3`, `push/4` | Push to a container registry |
| `save/2` | Save as OCI layout tarball |
| `export/2` | Export as OCI layout directory |

## Registry Authentication

### Docker Hub

```erlang
Auth = #{username => <<"myuser">>, password => <<"mypassword">>}.
ocibuild:push(Image, <<"docker.io">>, <<"myuser/myapp:latest">>, Auth).
```

### GitHub Container Registry (GHCR)

```erlang
Auth = #{token => os:getenv("GITHUB_TOKEN")}.
ocibuild:push(Image, <<"ghcr.io">>, <<"myorg/myapp:latest">>, Auth).
```

### Generic Token Auth

```erlang
Auth = #{token => <<"my-registry-token">>}.
ocibuild:push(Image, <<"my.registry.com">>, <<"myapp:latest">>, Auth).
```

## How It Works

ocibuild builds OCI images by:

1. **Fetching base image metadata** from the registry (manifest + config)
2. **Creating new layers** as gzip-compressed tar archives in memory
3. **Calculating content digests** (SHA256) for all blobs
4. **Generating OCI config and manifest** JSON
5. **Pushing blobs and manifest** to the target registry

This is the same approach used by .NET's `Microsoft.NET.Build.Containers`,
Google's `ko`, and Java's `jib`.

## Requirements

- OTP 27 or newer (uses the new `json` module)

## License

Apache License 2.0
