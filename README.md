<div align="center">
  <img src="assets/logo.png" height="124px">
  <h1 align="center">Build and Publish OCI Container Images</h1>
</div>

<p align="center">
<code>ocibuild</code> is an Erlang library for building <a href="https://opencontainers.org/">OCI-compliant</a> container images. 
It works from any BEAM language (Erlang, Elixir, Gleam, LFE) and has no dependencies outside OTP 27+.
</p>

<p align="center">
<a href="https://github.com/intility/erlang-oci-builder/actions/workflows/ci.yaml"><img alt="CI" src="https://github.com/intility/erlang-oci-builder/actions/workflows/ci.yaml/badge.svg"/></a>
<a href="https://hex.pm/packages/ocibuild"><img alt="hex.pm" src="https://img.shields.io/hexpm/v/ocibuild.svg"/></a>
<a href="https://github.com/intility/erlang-oci-builder/blob/main/LICENSE"><img alt="license" src="https://img.shields.io/badge/license-MIT-blue"/></a>
</p>

## Features

- 🚀 **No Docker required** — Builds images directly.
- 📦 **Push to any registry** — Docker Hub, GHCR, ECR, GCR, etc.
- 📋 **OCI compliant** — Produces standard OCI image layouts.

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

The easiest way to use `ocibuild` with Elixir:

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

# Load into podman
podman load < myapp-1.0.0.tar.gz

# Or push directly to a registry
export OCIBUILD_PUSH_USERNAME="myuser"
export OCIBUILD_PUSH_PASSWORD="mytoken"
mix ocibuild -t myapp:1.0.0 --push ghcr.io/myorg
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

The easiest way to use `ocibuild` with Erlang:

```erlang
%% rebar.config
{deps, [{ocibuild, "~> 0.1"}]}.

{ocibuild, [
    {base_image, "debian:slim"},
    {env, #{~"LANG" => ~"C.UTF-8"}},
    {expose, [8080]}
]}.
```

```bash
# Build release and create OCI image
rebar3 ocibuild -t myapp:1.0.0

# Load into podman
podman load < myapp-1.0.0.tar.gz

# Or push directly to a registry
export OCIBUILD_PUSH_USERNAME="myuser"
export OCIBUILD_PUSH_PASSWORD="mytoken"
rebar3 ocibuild -t myapp:1.0.0 --push ghcr.io/myorg
```

### Programmatic API (Erlang)

```erlang
%% Build from a base image
{ok, Image0} = ocibuild:from(~"docker.io/library/alpine:3.19"),

%% Add your application
{ok, AppBinary} = file:read_file("_build/prod/rel/myapp/myapp"),
Image1 = ocibuild:copy(Image0, [{~"myapp", AppBinary}], ~"/app"),

%% Configure the container
Image2 = ocibuild:entrypoint(Image1, [~"/app/myapp", ~"start"]),
Image3 = ocibuild:env(Image2, #{~"MIX_ENV" => ~"prod"}),

%% Push to a registry
Auth = #{username => list_to_binary(os:getenv("OCIBUILD_PUSH_USERNAME")),
         password => list_to_binary(os:getenv("OCIBUILD_PUSH_PASSWORD"))},
ok = ocibuild:push(Image3, ~"ghcr.io", ~"myorg/myapp:v1.0.0", Auth).

%% Or save as a tarball for podman load
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
auth = %{username: System.get_env("OCIBUILD_PUSH_USERNAME"),
         password: System.get_env("OCIBUILD_PUSH_PASSWORD")}
:ok = :ocibuild.push(image, "ghcr.io", "myorg/myapp:v1.0.0", auth)
```

## CLI Reference

Both `mix ocibuild` and `rebar3 ocibuild` share the same CLI options:

| Option       | Short | Description                                   |
|--------------|-------|-----------------------------------------------|
| `--tag`      | `-t`  | Image tag, e.g., `myapp:1.0.0`                |
| `--output`   | `-o`  | Output tarball path (default: `<tag>.tar.gz`) |
| `--push`     | `-p`  | Push to registry, e.g., `ghcr.io/myorg`       |
| `--desc`     | `-d`  | Image description (OCI manifest annotation)   |
| `--base`     |       | Override base image                           |
| `--release`  |       | Release name (if multiple configured)         |
| `--cmd`      | `-c`  | Release start command (Elixir only)           |

**Notes:**
- Tag defaults to `app:version` in Mix, required in rebar3
- `--cmd` options (Elixir): `start`, `start_iex`, `daemon`, `daemon_iex`

## Configuration

### rebar.config (Erlang)

```erlang
{ocibuild, [
    {base_image, "debian:slim"},           % Base image (default: debian:slim)
    {workdir, "/app"},                     % Working directory in container
    {env, #{                               % Environment variables
        ~"LANG" => ~"C.UTF-8"
    }},
    {expose, [8080, 443]},                 % Ports to expose
    {labels, #{                            % Image labels
        ~"org.opencontainers.image.source" => ~"https://github.com/..."
    }},
    {description, "My application"}        % OCI manifest annotation
]}.
```

### mix.exs (Elixir)

```elixir
def project do
  [
    app: :myapp,
    version: "1.0.0",
    # ...
    ocibuild: [
      base_image: "debian:slim",           # Base image (default: debian:slim)
      tag: "myapp:1.0.0",                  # Optional, defaults to app:version
      workdir: "/app",                     # Working directory in container
      cmd: "start",                        # Release command (default: start)
      env: %{"LANG" => "C.UTF-8"},         # Environment variables
      expose: [8080, 443],                 # Ports to expose
      labels: %{                           # Image labels
        "org.opencontainers.image.source" => "https://github.com/..."
      },
      description: "My application"        # OCI manifest annotation
    ]
  ]
end
```

## Authentication

### CLI (Environment Variables)

```bash
# Push credentials (for pushing to registries)
export OCIBUILD_PUSH_USERNAME="user"
export OCIBUILD_PUSH_PASSWORD="pass"

# Pull credentials (optional, for private base images)
# If not set, anonymous pull is attempted (works for public images)
export OCIBUILD_PULL_USERNAME="user"
export OCIBUILD_PULL_PASSWORD="pass"
```

### Programmatic API

```erlang
%% Read credentials from environment
Auth = #{username => list_to_binary(os:getenv("OCIBUILD_PUSH_USERNAME")),
         password => list_to_binary(os:getenv("OCIBUILD_PUSH_PASSWORD"))}.

%% Push to GHCR, Docker Hub, or any OCI registry
ocibuild:push(Image, ~"ghcr.io", ~"myorg/myapp:latest", Auth).
ocibuild:push(Image, ~"docker.io", ~"myuser/myapp:latest", Auth).
```

## How It Works

`ocibuild` builds OCI images by:

1. **Fetching base image metadata** from the registry (manifest + config)
2. **Creating new layers** as gzip-compressed tar archives in memory
3. **Calculating content digests** (SHA256) for all blobs
4. **Generating OCI config and manifest** JSON
5. **Pushing blobs and manifest** to the target registry

### Memory Requirements

`ocibuild` processes layers entirely in memory for simplicity and performance.
This means your VM needs sufficient memory to hold:

- **Your release files** (typically 20-100 MB for BEAM applications)
- **Compressed layer data** (gzip typically achieves 2-4x compression)
- **Base image layers** when downloading (cached after first download)

**Rule of thumb:** Allocate at least 2x your release size plus base image layers.
For a typical 50 MB release with a 30 MB base image, ensure ~200 MB available memory.

For very large images (>1 GB), consider:
- Breaking into multiple smaller layers
- Increasing VM memory limits (`+MBas` in `vm.args`)

## Choosing a Base Image

`ocibuild` is a build-time tool that creates OCI layers — it doesn't have a container runtime, 
so there's no equivalent to Dockerfile's `RUN apt-get install`. 

If your application needs libraries not in the base image, you have several options:

### Use Official Runtime Images

The easiest approach — official Erlang/Elixir images include common runtime dependencies:

```erlang
{ocibuild, [{base_image, "erlang:27-slim"}]}.
```

```elixir
ocibuild: [base_image: "elixir:1.17-slim"]
```

### Create a Custom Base Image

For specific dependencies, create a base image once and reuse it:

```dockerfile
# Dockerfile.base
FROM debian:slim
RUN apt-get update && apt-get install -y libncurses6 libssl3 \
    && rm -rf /var/lib/apt/lists/*
```

```bash
docker build -t myorg/erlang-base:1.0 -f Dockerfile.base .
docker push myorg/erlang-base:1.0
```

Then use it with `ocibuild`:

```erlang
{ocibuild, [{base_image, "myorg/erlang-base:1.0"}]}.
```
