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
<a href="https://hexdocs.pm/ocibuild"><img alt="docs" src="https://img.shields.io/badge/docs-hexdocs.pm-blue"/></a>
<a href="https://github.com/intility/erlang-oci-builder/blob/main/LICENSE"><img alt="license" src="https://img.shields.io/badge/license-MIT-blue"/></a>
</p>

## Features 🚀

| Feature                       | Status | Description                                                                                               |
|-------------------------------|--------|-----------------------------------------------------------------------------------------------------------|
| **No Docker required**        | ✅     | Builds images directly without container runtime.                                                         |
| **Push to any registry**      | ✅     | Docker Hub, GHCR, ECR, GCR, and any OCI-compliant registry.                                               |
| **OCI compliant**             | ✅     | Produces standard OCI image layouts.                                                                      |
| **Layer caching**             | ✅     | Base image layers cached locally for faster rebuilds.                                                     |
| **Tarball export**            | ✅     | Export images for podman, skopeo, crane, buildah, etc.                                                    |
| **OCI annotations**           | ✅     | Add custom annotations to image manifests. Automatically populate source URL, revision, version from VCS. |
| **Build system integration**  | ✅     | Native rebar3 and Mix task support.                                                                       |
| **Multi-platform images**     | ✅     | Build for multiple architectures (amd64, arm64) from a single command.                                    |
| **Reproducible builds**       | ✅     | Identical images from identical inputs using `SOURCE_DATE_EPOCH`.                                         |
| **Smart dependency layering** | ✅     | Separate layers for ERTS, dependencies, and application code.                                             |
| **Non-root by default**       | ✅     | Run as non-root (UID 65534) by default; override with `--uid`.                                            |
| **SBOM generation**           | ✅     | SPDX 2.2 SBOM embedded at `/sbom.spdx.json` and attached via referrers.                                   |
| **Image signing**             | ✅     | Sign images with ECDSA keys (cosign-compatible format).                                                   |

## Installation

### Erlang (rebar3)

```erlang
{deps, [
    {ocibuild, "~> 0.8"}
]}.
```

### Elixir (mix)

```elixir
def deps do
  [
    {:ocibuild, "~> 0.8"}
  ]
end
```

## Quick Start

### Mix Task (Elixir)

The easiest way to use `ocibuild` with Elixir:

```elixir
# mix.exs
def deps do
  [{:ocibuild, "~> 0.8"}]
end

def project do
  [
    # ...
    ocibuild: [
      base_image: "debian:stable-slim",
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

# Push with multiple tags (e.g., version + latest)
mix ocibuild -t myapp:1.0.0 -t myapp:latest --push ghcr.io/myorg
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
{project_plugin, [{ocibuild, "~> 0.8"}]}.

{ocibuild, [
    {base_image, "debian:stable-slim"},
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

# Push with multiple tags (e.g., version + latest)
rebar3 ocibuild -t myapp:1.0.0 -t myapp:latest --push ghcr.io/myorg
```

### Programmatic API (Erlang)

```erlang
%% Build from a base image
{ok, Image0} = ocibuild:from(~"docker.io/library/alpine:3.19"),

%% Add your application
{ok, AppBinary} = file:read_file("_build/prod/rel/myapp/myapp"),
Image1 = ocibuild:copy(Image0, [{~"myapp", AppBinary}], ~"/app"),

%% Configure the container
Image2 = ocibuild:entrypoint(Image1, [~"/app/myapp", ~"foreground"]),
Image3 = ocibuild:env(Image2, #{~"LANG" => ~"C.UTF-8"}),

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

| Option                 | Short | Description                                       |
|------------------------|-------|---------------------------------------------------|
| `--tag`                | `-t`  | Image tag (repeatable for multiple tags)          |
| `--output`             | `-o`  | Output tarball path (default: `<tag>.tar.gz`)     |
| `--push`               | `-p`  | Push to registry, e.g., `ghcr.io/myorg`           |
| `--desc`               | `-d`  | Image description (OCI manifest annotation)       |
| `--platform`           | `-P`  | Target platforms, e.g., `linux/amd64,linux/arm64` |
| `--base`               |       | Override base image                               |
| `--release`            |       | Release name (if multiple configured)             |
| `--cmd`                | `-c`  | Release start command (Elixir only)               |
| `--uid`                |       | User ID to run as (default: 65534 for nobody)     |
| `--chunk-size`         |       | Chunk size in MB for uploads (default: 5)         |
| `--sbom`               |       | Export SBOM to file path (SBOM always in image)   |
| `--no-vcs-annotations` |       | Disable automatic VCS annotations                 |
| `--sign-key`           |       | Path to cosign private key for image signing      |

**Notes:**
- Tag defaults to `app:version` in Mix, required in rebar3
- `--cmd` options (Elixir): `start`, `start_iex`, `daemon`, `daemon_iex`
- Multi-platform builds require `include_erts: false` and a base image with ERTS

### Multiple Tags

Push the same image with multiple tags in a single command:

```bash
# Build and push with multiple tags
rebar3 ocibuild -t myapp:1.0.0 -t myapp:latest --push ghcr.io/myorg
mix ocibuild -t myapp:1.0.0 -t myapp:latest --push ghcr.io/myorg

# Push existing tarball with multiple tags
rebar3 ocibuild --push ghcr.io/myorg -t myapp:2.0.0 -t myapp:latest myimage.tar.gz
```

This is efficient: the first tag does a full upload, additional tags just reference the same manifest (no blob re-upload). All tags report the same digest.

### Push Existing Tarball

You can also push a pre-built OCI tarball to a registry without rebuilding. This is useful for CI/CD pipelines where build and push are separate steps:

```bash
# Push existing tarball (uses embedded tag from image)
rebar3 ocibuild --push ghcr.io/myorg myimage.tar.gz
mix ocibuild --push ghcr.io/myorg myimage.tar.gz

# Push with tag override
rebar3 ocibuild --push ghcr.io/myorg --tag myapp:2.0.0 myimage.tar.gz
mix ocibuild --push ghcr.io/myorg -t myapp:2.0.0 myimage.tar.gz
```

**How it works:**
- Provide a tarball path (`.tar.gz`, `.tar`, or `.tgz`) as a positional argument after `--push`
- By default, the tag embedded in the tarball's `org.opencontainers.image.ref.name` annotation is used
- Use `--tag` to override the target tag (the manifest annotation is updated to match)
- Works in standalone mode — no release context required

**Typical CI/CD workflow:**
```yaml
# Build stage (on runner with source code)
build:
  script:
    - export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
    - mix release
    - mix ocibuild -t myapp:$CI_COMMIT_SHA

# Push stage (can be separate job/runner)
push:
  script:
    - mix ocibuild --push ghcr.io/myorg myapp.tar.gz
```

## Configuration

### rebar.config (Erlang)

```erlang
{ocibuild, [
    {base_image, "debian:stable-slim"},        % Base image (default: debian:stable-slim)
    {tag, "myapp:1.0.0"},                      % Image tag - string or list
    % {tag, ["myapp:1.0.0", "myapp:latest"]},  % Use a list for multiple tags
    {workdir, "/app"},                         % Working directory in container
    {env, #{                                   % Environment variables
        ~"LANG" => ~"C.UTF-8"
    }},
    {expose, [8080, 443]},                     % Ports to expose
    {labels, #{                                % Image labels
        ~"org.opencontainers.image.source" => ~"https://github.com/..."
    }},
    {uid, 65534},                              % User ID (optional, defaults to 65534)
    {description, "My application"},           % OCI manifest annotation
    {vcs_annotations, true}                    % Auto VCS annotations (default: true)
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
      base_image: "debian:stable-slim",        # Base image (default: debian:stable-slim)
      tag: "myapp:1.0.0",                      # Optional, defaults to app:version
      # tag: ["myapp:1.0.0", "myapp:latest"],  # Or use a list for multiple tags
      workdir: "/app",                         # Working directory in container
      cmd: "start",                            # Release command (default: start)
      env: %{"LANG" => "C.UTF-8"},             # Environment variables
      expose: [8080, 443],                     # Ports to expose
      labels: %{                               # Image labels
        "org.opencontainers.image.source" => "https://github.com/..."
      },
      uid: 65534,                              # User ID (optional, defaults to 65534)
      description: "My application",           # OCI manifest annotation
      vcs_annotations: true                    # Auto VCS annotations (default: true)
    ]
  ]
end
```

## Environment Variables

### Registry Authentication

```bash
# Push credentials (for pushing to registries)
export OCIBUILD_PUSH_USERNAME="user"
export OCIBUILD_PUSH_PASSWORD="pass"

# Pull credentials (optional, for private base images)
# If not set, anonymous pull is attempted (works for public images)
export OCIBUILD_PULL_USERNAME="user"
export OCIBUILD_PULL_PASSWORD="pass"
```

### Git Timeout

When `ocibuild` retrieves VCS information (source URL, revision) for automatic annotations, it executes git commands with a default timeout of 5 seconds. For slow networks or large repositories, you can increase this:

```bash
# Timeout in milliseconds (default: 5000, max: 300000)
export OCIBUILD_GIT_TIMEOUT=30000  # 30 seconds
```

This timeout applies only to network operations like `git remote get-url`. Local operations use the default timeout.

### Programmatic API

```erlang
%% Read credentials from environment
Auth = #{username => list_to_binary(os:getenv("OCIBUILD_PUSH_USERNAME")),
         password => list_to_binary(os:getenv("OCIBUILD_PUSH_PASSWORD"))}.

%% Push to GHCR, Docker Hub, or any OCI registry
ocibuild:push(Image, ~"ghcr.io", ~"myorg/myapp:latest", Auth).
ocibuild:push(Image, ~"docker.io", ~"myuser/myapp:latest", Auth).
```

## Automatic OCI Annotations

`ocibuild` automatically adds standard OCI annotations to your images:

| Annotation                             | Source          | Description                                       |
|----------------------------------------|-----------------|---------------------------------------------------|
| `org.opencontainers.image.source`      | Git remote URL  | Repository URL (converted to HTTPS)               |
| `org.opencontainers.image.revision`    | Git commit SHA  | Current commit hash                               |
| `org.opencontainers.image.version`     | Release version | Application version from build system             |
| `org.opencontainers.image.created`     | Build time      | ISO 8601 timestamp                                |
| `org.opencontainers.image.base.name`   | Base image      | Base image reference (e.g., `debian:stable-slim`) |
| `org.opencontainers.image.base.digest` | Base manifest   | SHA256 digest of base image manifest              |

### CI Environment Support

In CI environments, VCS information is read from environment variables for reliability:

- **GitHub Actions:** `GITHUB_SERVER_URL`, `GITHUB_REPOSITORY`, `GITHUB_SHA`
- **GitLab CI:** `CI_PROJECT_URL`, `CI_COMMIT_SHA`
- **Azure DevOps:** `BUILD_REPOSITORY_URI`, `BUILD_SOURCEVERSION`

When not in CI, `ocibuild` falls back to `git` commands.

### Disabling Automatic Annotations

To disable automatic VCS annotations, use `--no-vcs-annotations` or set in config:

```erlang
%% rebar.config
{ocibuild, [{vcs_annotations, false}]}.
```

```elixir
# mix.exs
ocibuild: [vcs_annotations: false]
```

The `created` timestamp respects `SOURCE_DATE_EPOCH` for reproducible builds.

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
FROM debian:stable-slim
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

## Multi-Platform Images

Build images for multiple architectures (e.g., `linux/amd64` and `linux/arm64`) from a single command.

### Requirements

Multi-platform builds require:
1. **No bundled ERTS** - Set `include_erts: false` in your release config
2. **Base image with ERTS** - Use `erlang:27-slim`, `elixir:1.17-slim`, or similar

BEAM bytecode is platform-independent, so only the base image layers differ between platforms.

### Configuration

**rebar.config:**
```erlang
{relx, [
    {include_erts, false},
    {system_libs, false}
]}.

{ocibuild, [
    {base_image, "erlang:27-slim"}
]}.
```

**mix.exs:**
```elixir
releases: [
  myapp: [
    include_erts: false
  ]
],
ocibuild: [
  base_image: "elixir:1.17-slim"
]
```

### Usage

```bash
# Build and push multi-platform image
rebar3 ocibuild -t myapp:1.0.0 --push ghcr.io/myorg --platform linux/amd64,linux/arm64
mix ocibuild -t myapp:1.0.0 --push ghcr.io/myorg -P linux/amd64,linux/arm64

# Build multi-platform tarball (OCI image index)
rebar3 ocibuild -t myapp:1.0.0 --platform linux/amd64,linux/arm64
```

### Programmatic API

```erlang
%% Build for multiple platforms
{ok, Platforms} = ocibuild:parse_platforms(~"linux/amd64,linux/arm64"),
{ok, Images} = ocibuild:from(~"erlang:27-slim", #{}, #{
    platforms => Platforms
}),

%% Configure all platform images
Images2 = [ocibuild:entrypoint(I, [~"/app/bin/myapp", ~"foreground"]) || I <- Images],

%% Push multi-platform image with index
Auth = #{username => ..., password => ...},
ok = ocibuild:push_multi(Images2, ~"ghcr.io", ~"myorg/myapp:1.0.0", Auth).
```

### Validation

The build will fail if multi-platform is requested but ERTS is bundled:

```
Error: Multi-platform builds require include_erts set to false.
Found bundled ERTS in release directory.
```

Native code (NIFs) in the release will trigger a warning since `.so` files may not be portable across platforms.

## Image Signing

Sign images with ECDSA P-256 keys (cosign-compatible format) to prove authenticity and enable verification by Kubernetes admission controllers.

> **Note:** Currently only local key-based signing is supported. Keyless signing via Sigstore/Fulcio may be added in a future release.

### Key Generation

Generate a cosign-compatible key pair:

```bash
# Using cosign
cosign generate-key-pair

# Or using openssl
openssl ecparam -genkey -name prime256v1 -noout -out cosign.key
openssl ec -in cosign.key -pubout -out cosign.pub
```

### Usage

```bash
# Sign with key file
rebar3 ocibuild --push ghcr.io/myorg --sign-key cosign.key
mix ocibuild --push ghcr.io/myorg --sign-key cosign.key

# Or via environment variable
OCIBUILD_SIGN_KEY=cosign.key rebar3 ocibuild --push ghcr.io/myorg
```

### Configuration

```erlang
%% rebar.config
{ocibuild, [
    {sign_key, "cosign.key"}
]}.
```

```elixir
# mix.exs
ocibuild: [
  sign_key: "cosign.key"
]
```

### Verification

After pushing, verify the signature with cosign:

```bash
cosign verify --key cosign.pub ghcr.io/myorg/myapp:latest
```

The signature is attached to the image using the OCI Referrers API, making it compatible with standard cosign verification workflows and Kubernetes admission controllers like Kyverno and OPA Gatekeeper.

## Reproducible Builds

`ocibuild` supports reproducible builds via the standard `SOURCE_DATE_EPOCH` environment variable.
When set, all timestamps in the image (config, history, TAR mtimes) use this value instead of current time,
and files are sorted alphabetically for deterministic ordering.

### Usage

```bash
# Set to git commit timestamp for reproducible builds
export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
rebar3 ocibuild -t myapp:1.0.0 --push ghcr.io/myorg

# Verify reproducibility
rebar3 ocibuild -t myapp:1.0.0 -o image1.tar.gz
rebar3 ocibuild -t myapp:1.0.0 -o image2.tar.gz
sha256sum image1.tar.gz image2.tar.gz  # Should match
```

### Benefits

- **Build verification**: Rebuild and verify images produce identical content
- **Security audits**: Confirm published images match source code
- **Registry deduplication**: Identical content = identical digests = deduplication
- **Debugging**: Same input always produces same output

See [reproducible-builds.org](https://reproducible-builds.org/docs/source-date-epoch/) for more information.

## Smart Dependency Layering

`ocibuild` automatically separates your release into multiple OCI layers for optimal caching in CI/CD pipelines. When only your application code changes, registries only need to store and transfer the app layer — dependencies remain cached.

### Layer Structure

| Layer    | Contents                                       | When it changes            |
|----------|------------------------------------------------|----------------------------|
| **Base** | OS files from base image                       | Base image updated         |
| **ERTS** | ERTS + OTP libs (stdlib, kernel, crypto, etc.) | Erlang/OTP version updated |
| **Deps** | Third-party dependencies from lock file        | Dependencies updated       |
| **App**  | Your application code, `bin/`, `releases/`     | Your code changes          |

**Note:** The ERTS layer is only created when `include_erts: true` (the default). When `include_erts: false`, OTP libs are included in the Deps layer since they come from the base image and are stable like dependencies.

### How It Works

`ocibuild` uses your lock file (`rebar.lock` or `mix.lock`) as the source of truth:

1. **App layer**: Files matching your application name
2. **Deps layer**: Files matching packages listed in your lock file
3. **ERTS layer**: Everything else (`erts-*` directory and OTP libraries)

This approach requires no configuration and has zero maintenance overhead — it automatically adapts to your project's dependencies.

### Benefits

```
# First build: all layers uploaded
Layer 1/3 (erts, amd64)         : [==============================] 45 MB
Layer 2/3 (deps, amd64)         : [==============================] 12 MB
Layer 3/3 (app, amd64)          : [==============================]  2 MB
Layer 4/4 (sbom, amd64)         : [==============================] 100% 3.4 KB/3.4 KB


# After code change: only app layer uploaded
Layer 1/3 (erts, amd64)         : exists (skipped)
Layer 2/3 (deps, amd64)         : exists (skipped)
Layer 3/3 (app, amd64)          : [==============================]  2 MB
Layer 4/4 (sbom, amd64)         : [==============================] 100% 3.4 KB/3.4 KB
```

Typical improvements:
- **80-90% smaller uploads** when only app code changes
- **Faster CI/CD pipelines** due to layer caching
- **Reduced registry storage** through layer deduplication

### Reproducible Builds Required for Layer Caching

For layer caching to work across builds, you **must** use reproducible builds by setting `SOURCE_DATE_EPOCH`. Without it, each build produces different layer digests (due to varying timestamps), causing all layers to be re-uploaded.

```bash
# Use the last git commit timestamp (recommended)
export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
rebar3 ocibuild -t myapp:1.0.0 --push ghcr.io/myorg
```

**CI/CD Examples:**

```yaml
# GitHub Actions
- run: |
    export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
    mix ocibuild --push ghcr.io/myorg

# GitLab CI
build:
  script:
    - export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
    - mix ocibuild --push registry.gitlab.com/myorg
```

See the [Reproducible Builds](#reproducible-builds) section for more details.

### Fallback Behavior

Smart layering is automatically enabled when a lock file is present. Without a lock file (or if parsing fails), all files go into a single layer — ensuring backward compatibility.
