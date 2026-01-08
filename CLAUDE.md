# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Summary

**ocibuild** is a pure Erlang library for building OCI container images without Docker. Think of it as the BEAM equivalent of .NET's `Microsoft.NET.Build.Containers`, Google's `ko`, or Java's `jib`.

## Build & Test Commands

```bash
# With rebar3 (Erlang)
rebar3 compile
rebar3 eunit
rebar3 eunit --test=ocibuild_tests:test_name_test  # Single test

# With mix (Elixir)
mix compile
mix test
mix test test/ocibuild_mix_test.exs:42  # Single test by line
```

## Formatting Commands

```bash
# With erlfmt
rebar3 fmt -w
```

## Code Style

- Always use the `~` sigil for binary literals.
- Always prefer markdown style docs for docstrings.
- **Prefer `maybe` expressions** over nested `case` statements for sequential fallible operations.

### Maybe Expressions and eqWalizer

eqWalizer has [limited support for `maybe` expressions](https://github.com/WhatsApp/eqwalizer/issues/55) and produces false-positive type errors. When writing functions that use `maybe`:

1. Add `-feature(maybe_expr, enable).` at the top of the module (after `-module`).
2. Add `-eqwalizer({nowarn_function, function_name/arity}).` for each function using `maybe`.

Example:
```erlang
-module(mymodule).
-feature(maybe_expr, enable).

%% eqWalizer has limited support for maybe expressions
-eqwalizer({nowarn_function, my_function/1}).

-spec my_function(map()) -> {ok, term()} | {error, term()}.
my_function(Input) ->
    maybe
        {ok, A} ?= step_one(Input),
        {ok, B} ?= step_two(A),
        {ok, B}
    end.
```

See `src/oci/ocibuild_layout.erl` for examples of this pattern.

## Architecture

```
src/
├── ocibuild.erl              → Public API (from, copy, push, save, annotation, etc.)
│
├── http/                     → HTTP/Registry operations
│   ├── ocibuild_http.erl         → Public facade for HTTP operations
│   ├── ocibuild_http_sup.erl     → OTP supervisor for HTTP workers
│   ├── ocibuild_http_pool.erl    → Coordinates parallel HTTP operations
│   ├── ocibuild_http_worker.erl  → Single-use worker (owns its httpc)
│   ├── ocibuild_registry.erl     → Registry client (pull/push, retry)
│   └── ocibuild_cache.erl        → Layer caching for base images
│
├── oci/                      → OCI image building
│   ├── ocibuild_layer.erl        → OCI layer creation (tar + gzip + SHA256)
│   ├── ocibuild_manifest.erl     → OCI manifest generation
│   ├── ocibuild_index.erl        → OCI image index for multi-platform
│   ├── ocibuild_layout.erl       → Export to directory/tarball
│   ├── ocibuild_tar.erl          → In-memory TAR builder (POSIX ustar)
│   └── ocibuild_sbom.erl         → SPDX 2.2 SBOM generation
│
├── adapters/                 → Build system adapters
│   ├── ocibuild_adapter.erl      → Behaviour for adapters
│   ├── ocibuild_release.erl      → Shared release handling
│   ├── ocibuild_rebar3.erl       → Rebar3 provider
│   └── ocibuild_mix.erl          → Mix adapter
│
├── vcs/                      → Version control
│   ├── ocibuild_vcs.erl          → VCS behaviour
│   └── ocibuild_vcs_git.erl      → Git adapter
│
└── util/                     → Utilities
    ├── ocibuild_digest.erl       → SHA256 utilities
    ├── ocibuild_json.erl         → JSON encode/decode
    ├── ocibuild_time.erl         → Timestamp utilities
    └── ocibuild_progress.erl     → Progress reporting

lib/
├── mix/tasks/ocibuild.ex     → Mix task (mix ocibuild command)
└── ocibuild/mix_release.ex   → Mix release step integration
```

**Adapter Pattern:**
```
                    ocibuild_release.erl
           (Shared: auth, progress, save, push, etc.)
                          ▲
                          │ uses
    ┌─────────────────────┼─────────────────────┐
    │                     │                     │
ocibuild_rebar3    ocibuild_mix         (Future adapters)
(rebar3 provider)  (Mix integration)    (Gleam, LFE, etc.)
    │                     │
    └──────────┬──────────┘
               │
       ocibuild_adapter (behaviour)
         - get_config/1
         - find_release/2
         - info/2, console/2, error/2
```

**Data Flow:**
```
User API (ocibuild.erl)
    ├─► ocibuild_http.erl ────► Parallel HTTP operations via supervised workers
    │       └─► ocibuild_registry.erl ──► Pull base image manifest + config + layers
    │           └─► ocibuild_cache.erl ──► Cache layers locally
    ├─► ocibuild_layer.erl ─────► Create layers (uses ocibuild_tar + zlib + ocibuild_digest)
    ├─► ocibuild_manifest.erl ──► Generate manifest JSON (with annotations)
    └─► ocibuild_layout.erl ────► Export to directory/tarball
        OR ocibuild_registry.erl ► Push to registry
```

**HTTP Supervision Tree:**
```
ocibuild_http_sup (one_for_one)
├── ocibuild_http_pool (transient) ─── Coordinates parallel operations
└── [Dynamic workers via start_child]
    ocibuild_http_worker (temporary) ── Single-use, owns its httpc profile
```

## Key Design Decisions

1. **In-memory TAR**: `ocibuild_tar.erl` implements POSIX ustar format manually because `:erl_tar` requires file I/O.
2. **Two Digests per Layer**: OCI requires `digest` (compressed) for manifests and `diff_id` (uncompressed) for config.
3. **OTP 27+ Target**: Uses native `json` module with fallback for OTP 25+.
4. **Zero Dependencies**: Only OTP stdlib (crypto, zlib, inets, ssl).
5. **Layer Caching**: Base image layers cached in `_build/ocibuild_cache/` for faster rebuilds.
6. **Adapter Pattern**: Build system integrations implement `ocibuild_adapter` behaviour to onboard new BEAM build systems (Gleam, LFE, etc.) with minimal effort.
7. **OTP-supervised HTTP**: Each HTTP worker owns its own httpc profile for isolation; clean shutdown via OTP supervision cascade.

## Current Status

**Working:** tar creation, layer creation, JSON encoding, image configuration, OCI layout export, tarball export (compatible with `podman load`, skopeo, crane, buildah), registry pull/push (tested with GHCR), manifest annotations, layer caching, progress reporting, chunked uploads for large layers, multi-platform images (OCI image index), reproducible builds (SOURCE_DATE_EPOCH), automatic VCS annotations (Git, GitHub Actions, GitLab CI, Azure DevOps), SBOM generation (SPDX 2.2 embedded in image layer and attached via OCI referrers API).

**Not Implemented:** Resumable uploads, zstd compression.

## CLI Reference

Both `rebar3 ocibuild` and `mix ocibuild` support:

| Option         | Short | Description                                       |
|----------------|-------|---------------------------------------------------|
| `--tag`        | `-t`  | Image tag (repeatable), e.g., `-t myapp:1.0.0 -t myapp:latest` |
| `--output`     | `-o`  | Output tarball path (default: `<tag>.tar.gz`)     |
| `--push`       | `-p`  | Push to registry, e.g., `ghcr.io/myorg`           |
| `--desc`       | `-d`  | Image description (OCI manifest annotation)       |
| `--platform`   | `-P`  | Target platforms, e.g., `linux/amd64,linux/arm64` |
| `--base`       |       | Override base image                               |
| `--release`    |       | Release name (if multiple configured)             |
| `--cmd`        | `-c`  | Release start command (Elixir only)               |
| `--uid`        |       | User ID to run as (default: 65534 for nobody)     |
| `--chunk-size` |       | Chunk size in MB for uploads (default: 5)         |
| `--no-vcs-annotations` | | Disable automatic VCS annotations            |
| `--sbom`       |       | Export SBOM to file path (SBOM always in image)   |

### Multiple Tags

Push the same image with multiple tags in a single command:

```bash
rebar3 ocibuild -t myapp:1.0.0 -t myapp:latest --push ghcr.io/myorg
mix ocibuild -t myapp:1.0.0 -t myapp:latest --push ghcr.io/myorg
```

This is efficient: the first tag does a full upload, additional tags just reference the same manifest.

### Push Existing Tarball

You can push a pre-built OCI tarball without rebuilding by providing a tarball path after `--push`:

```bash
# Push existing tarball (uses embedded tag)
rebar3 ocibuild --push ghcr.io/myorg myimage.tar.gz
mix ocibuild --push ghcr.io/myorg myimage.tar.gz

# Push with multiple tag overrides
rebar3 ocibuild --push ghcr.io/myorg -t myapp:2.0.0 -t myapp:latest myimage.tar.gz
```

This is useful for CI/CD pipelines where build and push are separate steps.

Whenever updating the CLI, remember to update the `src/ocibuild_rebar3.erl`, `lib/ocibuild/mix_release.ex` and `lib/mix/tasks/ocibuild.ex`
files to support the new functionality.

## Configuration

### rebar.config (Erlang)

```erlang
{ocibuild, [
    {base_image, "debian:stable-slim"},
    {tag, "myapp:1.0.0"},                   % String or list; or use CLI -t flags
    {workdir, "/app"},
    {env, #{<<"LANG">> => <<"C.UTF-8">>}},
    {expose, [8080]},
    {labels, #{<<"org.opencontainers.image.source">> => <<"...">>}},
    {description, "My application"},
    {vcs_annotations, true}    % Automatic VCS annotations (default: true)
]}.
```

### mix.exs (Elixir)

```elixir
def project do
  [
    ocibuild: [
      base_image: "debian:stable-slim",
      env: %{"LANG" => "C.UTF-8"},
      expose: [8080],
      description: "My application",
      vcs_annotations: true    # Automatic VCS annotations (default: true)
    ]
  ]
end
```

### Automatic Release Step (Elixir)

```elixir
releases: [
  myapp: [
    steps: [:assemble, &Ocibuild.MixRelease.build_image/1]
  ]
]
```

## Authentication

Environment variables:
- Push: `OCIBUILD_PUSH_USERNAME`/`OCIBUILD_PUSH_PASSWORD` or `OCIBUILD_PUSH_TOKEN`
- Pull (optional): `OCIBUILD_PULL_USERNAME`/`OCIBUILD_PULL_PASSWORD` or `OCIBUILD_PULL_TOKEN`

## Key Types (from ocibuild.erl)

```erlang
-opaque image() :: #{
    base := base_ref() | none,
    base_manifest => map(),
    base_config => map(),
    auth => auth() | #{},
    layers := [layer()],
    config := map(),
    annotations => map()    % OCI manifest annotations
}.

-type layer() :: #{
    media_type := binary(),
    digest := binary(),      % sha256:... of compressed data
    diff_id := binary(),     % sha256:... of uncompressed tar
    size := non_neg_integer(),
    data := binary()
}.
```

## Public API (ocibuild.erl)

| Function | Description |
|----------|-------------|
| `from/1`, `from/2`, `from/3` | Start from base image (with optional platform(s)) |
| `scratch/0` | Start from empty image |
| `add_layer/2` | Add layer with file modes |
| `copy/3` | Copy files to destination |
| `entrypoint/2` | Set entrypoint |
| `cmd/2` | Set CMD |
| `env/2` | Set environment variables |
| `workdir/2` | Set working directory |
| `expose/2` | Expose port |
| `label/3` | Add config label |
| `user/2` | Set user |
| `annotation/3` | Add manifest annotation |
| `push/3`, `push/4` | Push to registry |
| `push_multi/4`, `push_multi/5` | Push multi-platform image with index |
| `save/2`, `save/3` | Save as tarball (supports multi-platform) |
| `export/2` | Export as directory |

## Common Development Tasks

When building new important features or doing major changes to existing functionality,
always update `README.md`, `CLAUDE.md` and `AGENTS.md` to reflect the new reality.

### Add a new image configuration option

1. Add export to `ocibuild.erl`
2. Implement function using `set_config_field/3`
3. Add test to `ocibuild_tests.erl`

### Implement a new build system adapter

1. Create a new module implementing `ocibuild_adapter` behaviour
2. Implement required callbacks:
   - `get_config/1` - Extract configuration from build system state
   - `find_release/2` - Locate the release directory
   - `info/2`, `console/2`, `error/2` - Logging functions
3. Use `ocibuild_release` functions for shared functionality:
   - `get_pull_auth/0`, `get_push_auth/0` - Authentication
   - `make_progress_callback/0`, `clear_progress_line/0` - Progress display
   - `save_image/3`, `push_image/5` - Output operations
   - `parse_tag/1`, `add_description/2` - Utilities

### Debug tar output

```erlang
Tar = ocibuild_tar:create([{<<"/test">>, <<"hello">>, 8#644}]),
file:write_file("/tmp/test.tar", Tar).
% Then: tar -tvf /tmp/test.tar
```

### Test registry integration (interactive)

```erlang
{ok, Image} = ocibuild:from(<<"alpine:3.19">>).
```

## Files to Read First

1. `AGENTS.md` - Comprehensive development guide with OCI spec details
2. `src/ocibuild.erl` - Public API
3. `src/adapters/ocibuild_adapter.erl` - Adapter behaviour definition
4. `src/adapters/ocibuild_release.erl` - Shared release handling functions
5. `src/http/ocibuild_http.erl` - HTTP supervision facade
6. `src/oci/ocibuild_tar.erl` - Core tar implementation
7. `test/ocibuild_tests.erl` - Usage examples
