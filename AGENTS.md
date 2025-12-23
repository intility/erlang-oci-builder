# ocibuild Development Guide

This document provides a comprehensive overview of the `ocibuild` project for continuing development.

## Project Overview

**ocibuild** is a pure Erlang library for building OCI-compliant container images programmatically, without requiring Docker or any container runtime. It's inspired by:

- .NET's `Microsoft.NET.Build.Containers`
- Google's `ko` (for Go)
- Java's `jib`

### Feature Comparison with Similar Tools

| Feature                       | ocibuild           | ko (Go)     | jib (Java)        | .NET Containers |
|-------------------------------|--------------------|-------------|-------------------|-----------------|
| No Docker required            | ✅                 | ✅          | ✅                | ✅              |
| Push to registries            | ✅                 | ✅          | ✅                | ✅              |
| Layer caching                 | ✅                 | ✅          | ✅                | ✅              |
| Tarball export                | ✅                 | ✅          | ✅                | ✅              |
| OCI annotations               | ✅                 | ✅          | ✅                | ✅              |
| Build system integration      | ✅ (rebar3/Mix)    | ✅          | ✅ (Maven/Gradle) | ✅ (MSBuild)    |
| **Multi-platform images**     | ✅                 | ✅          | ✅                | ✅              |
| **Reproducible builds**       | ✅                 | ✅          | ✅                | ✅              |
| **Smart dependency layering** | ⏳ Planned (P3)    | N/A         | ✅                | ✅              |
| **Non-root by default**       | ✅                 | ✅          | ❌                | ✅              |
| **Auto OCI annotations**      | ✅                 | ✅          | ✅                | ✅              |
| **SBOM generation**           | ⏳ Planned (P6)    | ✅ (SPDX)   | ❌                | ✅ (SPDX)       |
| **Image signing**             | ⏳ Planned (P7)    | ✅ (cosign) | ❌                | ❌              |
| Zstd compression              | ❌ Future (OTP28+) | ✅          | ❌                | ❌              |

Legend: ✅ Implemented | ⏳ Planned (P# = Priority) | ❌ Not implemented

**References:**
- [ko: Easy Go Containers](https://ko.build/)
- [jib - Build container images for Java](https://github.com/GoogleContainerTools/jib)
- [.NET SDK container creation](https://learn.microsoft.com/en-us/dotnet/core/containers/overview)

### Goals

1. **Zero dependencies** — Only OTP stdlib modules (crypto, zlib, inets, ssl, json)
2. **BEAM-universal** — Works from Erlang, Elixir, Gleam, LFE via hex.pm
3. **OCI compliant** — Produces standard OCI image layouts
4. **No Docker required** — Builds and pushes images directly to registries

### Target OTP Version

- Primary target: OTP 27+ (has built-in `json` module)

### Code style

- Always prefer using `maybe` instead of deeply nested `case...of`
- Prefer Markdown style comments instead of EDoc (deprecated).

---

## Architecture

### Module Structure

```
src/
├── ocibuild.erl           # Public API - the main interface users interact with
├── ocibuild_adapter.erl   # Behaviour for build system adapters (rebar3, Mix, etc.)
├── ocibuild_rebar3.erl    # Rebar3 provider (implements ocibuild_adapter)
├── ocibuild_mix.erl       # Mix adapter (implements ocibuild_adapter)
├── ocibuild_release.erl   # Shared release handling (configure_release_image is the single
│                          # source of truth for image config: layers, env, ports, labels,
│                          # annotations, uid, etc. Used by both CLI and programmatic API)
├── ocibuild_tar.erl       # In-memory TAR archive builder (POSIX ustar format)
├── ocibuild_layer.erl     # OCI layer creation (tar + gzip + digests)
├── ocibuild_digest.erl    # SHA256 digest utilities
├── ocibuild_json.erl      # JSON encode/decode (OTP 27 native + fallback)
├── ocibuild_manifest.erl  # OCI manifest generation (with annotations support)
├── ocibuild_index.erl     # OCI image index for multi-platform images
├── ocibuild_layout.erl    # OCI image layout export (directory/tarball, multi-platform)
├── ocibuild_registry.erl  # Registry client (pull/push via HTTP with retry logic)
├── ocibuild_cache.erl     # Layer caching for base images
├── ocibuild_time.erl      # Timestamp utilities for reproducible builds (SOURCE_DATE_EPOCH)
├── ocibuild_vcs.erl       # VCS behaviour and detection for auto-annotations
├── ocibuild_vcs_git.erl   # Git adapter (source URL, revision from git or CI env vars)
└── ocibuild.app.src       # OTP application spec

lib/
├── mix/tasks/ocibuild.ex      # Mix task (mix ocibuild command)
└── ocibuild/mix_release.ex    # Mix release step integration
```

**Adapter Pattern:**
```
                    ocibuild_release.erl
           (Shared: auth, progress, save, push, validation)
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

### Data Flow

```
User Code / CLI (rebar3 ocibuild, mix ocibuild)
    │
    ▼
ocibuild_rebar3 / ocibuild_mix (Adapters)
    │
    ├─► ocibuild_adapter ──────► Behaviour interface (get_config, find_release, logging)
    │
    └─► ocibuild_release ──────► Shared release handling
            │
            ├─► ocibuild_time ─────► Timestamps (respects SOURCE_DATE_EPOCH)
            │
            └─► ocibuild.erl (Public API)
                    │
                    ├─► ocibuild_registry ──► Pull base image manifest + config + layers
                    │       │
                    │       └─► ocibuild_cache ──► Cache layers locally in _build/
                    │
                    ├─► ocibuild_layer ─────► Create new layers
                    │       │
                    │       ├─► ocibuild_tar ──► Build tar in memory (sorted, deterministic mtime)
                    │       ├─► zlib:gzip/1 ───► Compress
                    │       └─► ocibuild_digest ► Calculate SHA256
                    │
                    ├─► ocibuild_manifest ──► Generate manifest JSON (with annotations)
                    │
                    ├─► ocibuild_index ─────► Generate OCI image index (multi-platform)
                    │
                    └─► ocibuild_layout ────► Export to directory/tarball
                        OR
                        ocibuild_registry ──► Push to registry (single or multi-platform)
```

---

## Module Details

### ocibuild.erl (Public API)

**Status: ✅ Implemented and tested**

The main public interface. Key types:

```erlang
-opaque image() :: #{
    base := base_ref() | none,
    base_manifest => map(),      % Original manifest from registry
    base_config => map(),        % Original config from registry
    auth => auth() | #{},        % Auth credentials for registry operations
    layers := [layer()],         % New layers added by user
    config := map(),             % Modified config
    annotations => map()         % OCI manifest annotations
}.

-type base_ref() :: {Registry :: binary(), Repo :: binary(), Ref :: binary()}.

-type layer() :: #{
    media_type := binary(),
    digest := binary(),          % sha256:... of compressed data
    diff_id := binary(),         % sha256:... of uncompressed tar
    size := non_neg_integer(),
    data := binary()             % The actual compressed layer data
}.

-type auth() :: #{username := binary(), password := binary()} |
                #{token := binary()}.
```

**Public Functions:**

| Function                       | Description                        | Status                    |
|--------------------------------|------------------------------------|---------------------------|
| `from/1`, `from/2`, `from/3`   | Start from base image              | ✅ Implemented            |
| `scratch/0`                    | Start from empty image             | ✅ Implemented            |
| `add_layer/2`                  | Add layer with file modes          | ✅ Implemented            |
| `copy/3`                       | Copy files to destination          | ✅ Implemented            |
| `entrypoint/2`                 | Set entrypoint                     | ✅ Implemented            |
| `cmd/2`                        | Set CMD                            | ✅ Implemented            |
| `env/2`                        | Set environment variables          | ✅ Implemented            |
| `workdir/2`                    | Set working directory              | ✅ Implemented            |
| `expose/2`                     | Expose port                        | ✅ Implemented            |
| `label/3`                      | Add config label                   | ✅ Implemented            |
| `user/2`                       | Set user (UID or username)         | ✅ Implemented            |
| `annotation/3`                 | Add manifest annotation            | ✅ Implemented            |
| `push/3`, `push/4`             | Push single image to registry      | ✅ Implemented and tested |
| `push_multi/4`, `push_multi/5` | Push multi-platform image w/ index | ✅ Implemented and tested |
| `save/2`, `save/3`             | Save as tarball (multi-platform)   | ✅ Implemented and tested |
| `export/2`                     | Export as directory                | ✅ Implemented and tested |

**Image Reference Parsing:**

The `parse_image_ref/1` function handles various formats:
- `"alpine:3.19"` → `{~"docker.io", ~"library/alpine", ~"3.19"}`
- `"docker.io/library/alpine:3.19"` → same as above
- `"ghcr.io/myorg/myapp:v1"` → `{~"ghcr.io", ~"myorg/myapp", ~"v1"}`
- `"myregistry.com:5000/myapp:latest"` → handles port in registry

---

### ocibuild_release.erl (Shared Release Handling)

**Status: ✅ Implemented and tested**

Provides common functionality for collecting release files and building OCI images. The
`configure_release_image/3` function is the **single source of truth** for image configuration,
used by both the programmatic API (`build_image/3`) and CLI adapters (`run/3`).

**Key Functions:**

```erlang
%% Collect files from release directory (with symlink security)
-spec collect_release_files(ReleasePath) -> {ok, Files} | {error, term()}.

%% Build OCI image from release files (programmatic API)
-spec build_image(BaseImage, Files, Opts) -> {ok, image()} | {error, term()}.

%% Configure image with all settings (internal, single source of truth)
%% Handles: layers, workdir, entrypoint, env, ports, labels, annotations, uid, description
-spec configure_release_image(Image, Files, Opts) -> image().
```

**Unified Code Path:**
```
CLI (rebar3/mix)           Programmatic API
       │                         │
       ▼                         ▼
    run/3                   build_image/3
       │                         │
       └────────┬────────────────┘
                │
                ▼
       configure_release_image/3
       (layers, env, ports, labels,
        annotations, uid, user, etc.)
```

**Security Features:**
- Symlinks pointing outside the release directory are rejected
- Broken symlinks are skipped with a warning
- Path traversal via `..` components is prevented
- Non-root by default (UID 65534)

---

### ocibuild_tar.erl (In-Memory TAR Builder)

**Status: ✅ Implemented and tested**

This is a critical module that builds TAR archives entirely in memory without writing to disk. The standard `:erl_tar` module requires file I/O, so we implement the POSIX ustar format manually.

**TAR Format Basics:**
- 512-byte blocks
- Each file: 512-byte header + content + padding to 512 boundary
- Archive ends with two 512-byte zero blocks

**Key Functions:**

```erlang
%% Create a tar archive in memory
-spec create([{Path :: binary(), Content :: binary(), Mode :: integer()}]) -> binary().

%% Create a gzip-compressed tar archive
-spec create_compressed([{Path :: binary(), Content :: binary(), Mode :: integer()}]) -> binary().
```

**Implementation Notes:**

1. **Path Normalization**: Paths are normalized to start with `./` for tar compatibility
2. **Directory Creation**: Parent directories are automatically created
3. **Long Paths**: Uses ustar prefix field for paths > 100 chars (splits at `/`)
4. **Checksum**: Computed as sum of all header bytes (with checksum field as spaces)

**Potential Issues:**
- Very long paths (>255 chars combined) will be truncated
- No support for symlinks, hard links, or special files (not needed for OCI layers)
- No support for extended attributes

---

### ocibuild_layer.erl (Layer Creation)

**Status: ✅ Implemented and tested**

Creates OCI layers from file lists. An OCI layer has two digests:
- `digest`: SHA256 of **compressed** data (used in manifest, for content addressing)
- `diff_id`: SHA256 of **uncompressed** tar (used in config's rootfs section)

```erlang
-spec create([{Path :: binary(), Content :: binary(), Mode :: integer()}]) -> layer().
```

**Media Types Supported:**
- `application/vnd.oci.image.layer.v1.tar+gzip` (default)
- `application/vnd.oci.image.layer.v1.tar+zstd` (defined but not implemented)
- `application/vnd.oci.image.layer.v1.tar` (uncompressed, defined but not used)

---

### ocibuild_digest.erl (SHA256 Utilities)

**Status: ✅ Implemented and tested**

Simple wrapper around `:crypto` for OCI-style digests.

```erlang
%% Returns ~"sha256:abc123..."
-spec sha256(binary()) -> digest().

%% Extract parts
-spec algorithm(digest()) -> binary().  % ~"sha256"
-spec encoded(digest()) -> binary().    % ~"abc123..."
```

---

### ocibuild_json.erl (JSON Handling)

**Status: ✅ Implemented and tested**

Wraps OTP 27's `json` module with a fallback implementation for older OTP versions.

```erlang
%% Runtime check for OTP 27+ json module
-define(HAS_JSON_MODULE, (erlang:function_exported(json, encode, 1))).
```

**Fallback Implementation:**
- Recursive descent parser for decoding
- Simple encoder supporting: null, booleans, numbers, strings, arrays, maps
- Handles escape sequences: `\"`, `\\`, `\n`, `\r`, `\t`, `\uXXXX`

---

### ocibuild_manifest.erl (Manifest Generation)

**Status: ✅ Implemented and tested**

Generates OCI image manifests with optional annotations.

```erlang
%% Returns {JsonBinary, Digest}
-spec build(ConfigDescriptor :: map(), LayerDescriptors :: [map()]) -> {binary(), binary()}.
-spec build(ConfigDescriptor :: map(), LayerDescriptors :: [map()], Annotations :: map()) -> {binary(), binary()}.
```

**Manifest Structure:**
```json
{
  "schemaVersion": 2,
  "mediaType": "application/vnd.oci.image.manifest.v1+json",
  "config": {
    "mediaType": "application/vnd.oci.image.config.v1+json",
    "digest": "sha256:...",
    "size": 1234
  },
  "layers": [
    {
      "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
      "digest": "sha256:...",
      "size": 5678
    }
  ],
  "annotations": {
    "org.opencontainers.image.description": "My application"
  }
}
```

---

### ocibuild_layout.erl (Export)

**Status: ✅ Implemented and tested**

Exports images in OCI Image Layout format.

**Two Export Modes:**

1. **Directory Export** (`export/2`):
```
myimage/
├── oci-layout           # {"imageLayoutVersion": "1.0.0"}
├── index.json           # Entry point manifest list
└── blobs/
    └── sha256/
        ├── <manifest>   # Manifest JSON
        ├── <config>     # Config JSON
        └── <layers...>  # Layer tarballs
```

2. **Tarball Export** (`save/2`, `save/3`):
   - Same structure as above, but packaged as `.tar.gz`
   - Compatible with `docker load` and `podman load`

---

### ocibuild_registry.erl (Registry Client)

**Status: ✅ Implemented and tested (GHCR integration tests in CI)**

Implements OCI Distribution Specification for pulling/pushing.

**Supported Registries:**
| Registry   | URL                  | Auth Method                       |
|------------|----------------------|-----------------------------------|
| Docker Hub | registry-1.docker.io | Token exchange via auth.docker.io |
| GHCR       | ghcr.io              | Bearer token (GITHUB_TOKEN)       |
| GCR        | gcr.io               | Bearer token                      |
| Quay.io    | quay.io              | Bearer token                      |
| Others     | https://{registry}   | Basic auth or bearer token        |

**Key Functions:**

```erlang
%% Pull manifest and config
-spec pull_manifest(Registry, Repo, Ref) -> {ok, Manifest, Config} | {error, term()}.
-spec pull_manifest(Registry, Repo, Ref, Auth) -> {ok, Manifest, Config} | {error, term()}.

%% Pull a blob (layer data)
-spec pull_blob(Registry, Repo, Digest) -> {ok, binary()} | {error, term()}.

%% Push complete image
-spec push(Image, Registry, Repo, Tag, Auth) -> ok | {error, term()}.

%% Check if blob exists (for layer deduplication)
-spec check_blob_exists(Registry, Repo, Digest, Auth) -> boolean().
```

**Features:**
- Automatic retry with exponential backoff for transient failures
- Progress callback support for download and upload tracking
- Layer existence check before upload (deduplication)
- Chunked uploads for large layers (OCI Distribution Spec compliant)

**Chunked Upload:**

Layers >= 5MB (configurable) are uploaded using OCI chunked upload:

```
1. POST /v2/{repo}/blobs/uploads/  → Get upload session URL
2. PATCH {url} with Content-Range  → Upload chunks (5MB default)
3. PUT {url}?digest={digest}       → Complete upload with final chunk
```

Options:
- `chunk_size`: Size in bytes for chunked uploads (default: 5MB)
- `progress`: Callback function for upload progress updates

Example with chunked upload options:
```erlang
Opts = #{chunk_size => 10 * 1024 * 1024},  % 10MB chunks
ocibuild_registry:push(Image, Registry, Repo, Tag, Auth, Opts).
```

---

### ocibuild_cache.erl (Layer Caching)

**Status: ✅ Implemented and tested**

Caches downloaded base image layers locally to avoid re-downloading on every build.

**Cache Location (in order of precedence):**
1. `OCIBUILD_CACHE_DIR` environment variable
2. Project root detection → `<project>/_build/ocibuild_cache/`
3. Fall back to `./_build/ocibuild_cache/`

**CI Integration:**
```yaml
# GitHub Actions
- uses: actions/cache@v4
  with:
    path: _build/ocibuild_cache
    key: ocibuild-${{ runner.os }}-${{ hashFiles('rebar.lock') }}
```

---

## Testing

### Running Tests

```bash
# Erlang tests
rebar3 eunit

# Elixir tests
mix test

# Single test
rebar3 eunit --test=ocibuild_tests:test_name_test
```

### Test Coverage

| Module            | Status                         |
|-------------------|--------------------------------|
| ocibuild_digest   | ✅ Tested                      |
| ocibuild_json     | ✅ Tested                      |
| ocibuild_tar      | ✅ Tested                      |
| ocibuild_layer    | ✅ Tested                      |
| ocibuild_manifest | ✅ Tested                      |
| ocibuild_layout   | ✅ Tested                      |
| ocibuild_registry | ✅ Tested (unit + integration) |
| ocibuild_cache    | ✅ Tested                      |
| ocibuild_release  | ✅ Tested                      |
| ocibuild_index    | ✅ Tested                      |
| ocibuild_time     | ✅ Tested                      |
| ocibuild_vcs      | ✅ Tested                      |
| ocibuild_vcs_git  | ✅ Tested                      |
| ocibuild (API)    | ✅ Tested                      |

---

## Roadmap (Prioritized)

Always update this file with new status when we have completed a roadmap task.

### Priority 1: Multi-Platform Images ✅ IMPLEMENTED

**Status:** Fully implemented and tested

Supports building `linux/amd64` + `linux/arm64` images. Essential for:
- Kubernetes clusters with mixed node architectures
- Apple Silicon development → Linux deployment
- AWS Graviton / Azure ARM instances

**Approach:** Use base image with ERTS (`include_erts: false`)

BEAM bytecode is platform-independent, but ERTS is native code. For multi-platform builds:
- User sets `include_erts: false` in release config
- Uses base image with ERTS (e.g., `erlang:27-alpine`, `elixir:1.17-alpine`)
- ocibuild pulls platform-specific base image variants
- Application layer is identical across platforms (only base layers differ)

**Validation Checks:**

1. **ERTS Check (Error):** If `--platform` specifies multiple platforms and release contains `erts-*` directory, fail with build-system-specific error:

   For rebar3:
   ```
   Error: Multi-platform builds require include_erts set to false.
   Found bundled ERTS in release directory.

   Fix in rebar.config:
     {relx, [
         {include_erts, false},
         {system_libs, false}
     ]}.

   Then use a base image with ERTS:
     {ocibuild, [{base_image, "erlang:27-alpine"}]}.
   ```

   For Mix:
   ```
   Error: Multi-platform builds require include_erts set to false.
   Found bundled ERTS in release directory.

   Fix in mix.exs:
     releases: [
       myapp: [
         include_erts: false,
         include_src: false
       ]
     ]

   Then use a base image with ERTS:
     ocibuild: [base_image: "elixir:1.17-alpine"]
   ```

2. **NIF Check (Warning):** If `.so` files found in `lib/*/priv/`, warn that native code may cause platform compatibility issues.

```
--platform linux/amd64,linux/arm64
              │
              ▼
    ┌─────────────────────┐
    │ has_bundled_erts?   │
    └─────────────────────┘
        │           │
       yes          no
        │           │
        ▼           ▼
   ❌ ERROR    ┌──────────────┐
               │ has_nifs?    │
               └──────────────┘
                   │       │
                  yes      no
                   │       │
                   ▼       ▼
              ⚠️ WARN    ✅ OK
```

**Implementation Steps:**

1. **Handle Image Index on pull** (`ocibuild_registry.erl`):
   - Detect when base image returns an index vs single manifest
   - Add `Platform` parameter to `pull_manifest/5`
   - Select platform-specific manifest from index

2. **Add platform to image type** (`ocibuild.erl`):
   ```erlang
   -opaque image() :: #{
       ...
       platform => #{os := binary(), architecture := binary()}
   }.
   ```

3. **New module: `ocibuild_index.erl`**:
   - `create/1` - takes list of platform-specific images, returns index
   - `to_json/1` - serialize index to JSON with platform descriptors
   - Media type: `application/vnd.oci.image.index.v1+json`

4. **Validation functions** (`ocibuild_release.erl`):
   - `validate_multiplatform/2` - check for bundled ERTS (error)
   - `check_for_nifs/1` - detect native code in deps (warning)

5. **Extend push for indexes** (`ocibuild_registry.erl`):
   - Push each platform's layers + config + manifest
   - Create and push the index
   - Tag points to index digest

6. **CLI support**:
   - Add `--platform` option (e.g., `--platform linux/amd64,linux/arm64`)
   - Update `ocibuild_rebar3.erl` and `lib/mix/tasks/ocibuild.ex`

**API Design:**

```erlang
%% Build for multiple platforms
{ok, Images} = ocibuild:from(~"alpine:3.19", #{
    platforms => [~"linux/amd64", ~"linux/arm64"]
}),
%% Returns list of images, one per platform
Images2 = [ocibuild:entrypoint(I, [...]) || I <- Images],
ok = ocibuild:push_multi(Images2, Registry, Repo, Tag, Auth).
```

**CLI Usage:**

```bash
rebar3 ocibuild --push ghcr.io/myorg --platform linux/amd64,linux/arm64
mix ocibuild --push ghcr.io/myorg --platform linux/amd64,linux/arm64

# Multi-platform tarball (OCI image index)
rebar3 ocibuild -t myapp:1.0.0 --platform linux/amd64,linux/arm64
```

**Implementation Summary:**

| Component | File | Description |
|-----------|------|-------------|
| Platform types | `ocibuild.erl` | `parse_platform/1`, `parse_platforms/1` |
| OCI Image Index | `ocibuild_index.erl` | `create/1`, `to_json/1`, `select_manifest/2` |
| Validation | `ocibuild_release.erl` | `has_bundled_erts/1`, `check_for_native_code/1`, `validate_multiplatform/2` |
| Registry | `ocibuild_registry.erl` | `pull_manifests_for_platforms/5`, `push_multi/6` |
| Public API | `ocibuild.erl` | Extended `from/3` with `platforms` option, `push_multi/4,5` |
| Layout | `ocibuild_layout.erl` | Multi-platform tarball support with OCI image index |
| CLI | `ocibuild_rebar3.erl`, `lib/mix/tasks/ocibuild.ex` | `--platform/-P` option |

### Priority 2: Reproducible Builds ✅ IMPLEMENTED

**Status:** Fully implemented and tested

**Impact:** Build verification, security audits, registry deduplication

**Approach:** Support `SOURCE_DATE_EPOCH` environment variable ([spec](https://reproducible-builds.org/docs/source-date-epoch/))

```bash
# Set to git commit timestamp for reproducible builds
export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
rebar3 ocibuild --push ghcr.io/myorg
```

No CLI flag - environment variable only (it's the standard).

**Implementation Summary:**

| Component | File | Description |
|-----------|------|-------------|
| Timestamp utilities | `ocibuild_time.erl` | `get_timestamp/0`, `get_iso8601/0`, `unix_to_iso8601/1` |
| TAR creation | `ocibuild_tar.erl` | `create/2` with `mtime` option, alphabetical file sorting |
| Layer creation | `ocibuild_layer.erl` | `create/2` passes mtime through to TAR |
| Config timestamps | `ocibuild.erl` | `iso8601_now/0` delegates to `ocibuild_time` |

**Sources of Non-Determinism (all fixed):**

| Source | Fix |
|--------|-----|
| Config `created` timestamp | ✅ Uses `SOURCE_DATE_EPOCH` via `ocibuild_time` |
| History `created` timestamps | ✅ Uses `SOURCE_DATE_EPOCH` via `ocibuild_time` |
| TAR file `mtime` headers | ✅ Uses `SOURCE_DATE_EPOCH` for all files |
| File ordering in TAR | ✅ Sorted alphabetically by path in `ocibuild_tar` |
| Gzip MTIME header | ✅ Already zero (Erlang's `zlib:gzip/1` sets MTIME=0) |

### Priority 3: Smart Dependency Layering

**Impact:** Faster CI/CD, smaller uploads

jib separates Java apps into distinct layers (dependencies vs application code). When only app code changes, registries only store/transfer the app layer.

**Layer Structure:**

With ERTS (`include_erts: true`, single-platform):
```
Base image (e.g., debian:stable-slim)
  └── Layer 1: ERTS + system libs (erts-*, lib/stdlib-*, lib/kernel-*)
        └── Layer 2: Dependencies (lib/* from lock file)
              └── Layer 3: Application code (lib/myapp-*, bin/, releases/)
```

Without ERTS (`include_erts: false`, multi-platform):
```
Base image (e.g., erlang:27-alpine, provides ERTS)
  └── Layer 1: Dependencies (lib/* from lock file)
        └── Layer 2: Application code (lib/myapp-*, bin/, releases/)
```

This is the default behavior, not an option. Layer count is determined by whether ERTS is present.

**Implementation Steps:**

1. **New adapter callback** (`ocibuild_adapter.erl`):
   ```erlang
   -callback get_dependencies(State :: term()) ->
       {ok, [#{name := binary(), version := binary(), source := binary()}]} |
       {error, term()}.
   %% Returns full dependency info from lock file
   %% e.g., [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}, ...]
   ```

   This keeps build system logic in adapters, enabling future Gleam/LFE support.
   Same callback is reused for SBOM generation (Priority 6).

2. **Classify lib directories** (`ocibuild_release.erl`):
   ```erlang
   classify_libs(ReleasePath, Deps) ->
       LibPath = filename:join(ReleasePath, "lib"),
       {ok, Dirs} = file:list_dir(LibPath),
       DepNames = [maps:get(name, D) || D <- Deps],

       lists:partition(
           fun(Dir) ->
               %% "cowboy-2.10.0" -> "cowboy"
               AppName = extract_app_name(Dir),
               lists:member(AppName, DepNames)
           end,
           Dirs
       ).
       %% Returns {DepDirs, AppDirs}
   ```

3. **Build layers based on ERTS presence** (`ocibuild_release.erl`):
   ```erlang
   build_layers(ReleasePath, Deps) ->
       {DepDirs, AppDirs} = classify_libs(ReleasePath, Deps),

       case has_bundled_erts(ReleasePath) of
           true ->
               [
                   build_erts_layer(ReleasePath),
                   build_deps_layer(ReleasePath, DepDirs),
                   build_app_layer(ReleasePath, AppDirs)
               ];
           false ->
               [
                   build_deps_layer(ReleasePath, DepDirs),
                   build_app_layer(ReleasePath, AppDirs)
               ]
       end.
   ```

4. **Implement adapter callbacks:**

   For rebar3 (`ocibuild_rebar3.erl`):
   - Parse `rebar.lock` to extract dependency names

   For Mix (`ocibuild_mix.erl`):
   - Parse `mix.lock` to extract dependency names

**Lock File Parsing:**

rebar.lock format:
```erlang
{~"cowboy", {pkg, ~"cowboy", ~"2.10.0"}, 0}.
```

mix.lock format:
```elixir
%{"cowboy": {:hex, :cowboy, "2.10.0", ...}}
```

### Priority 4: Non-Root by Default ✅ IMPLEMENTED

**Status:** Fully implemented and tested

**Impact:** Security best practice

.NET containers and ko default to non-root for security. Running as root inside containers is a security risk.

**Approach:** Single `--uid` option, defaults to 65534 (nobody)

```bash
# Default: runs as nobody (65534)
rebar3 ocibuild --push ghcr.io/myorg

# Explicit non-root UID
rebar3 ocibuild --push ghcr.io/myorg --uid 1000

# Run as root (UID 0)
rebar3 ocibuild --push ghcr.io/myorg --uid 0
```

**Implementation Summary:**

| Component | File | Description |
|-----------|------|-------------|
| CLI option | `ocibuild_rebar3.erl` | `--uid` option, `get_uid/2` helper |
| CLI option | `lib/mix/tasks/ocibuild.ex` | `uid: :integer` switch |
| Config type | `ocibuild_adapter.erl` | `uid => non_neg_integer() \| undefined` |
| Defaults | `ocibuild_mix.erl` | `uid => undefined` in defaults |
| Application | `ocibuild_release.erl` | Applies user in `configure_release_image/3` |

**Behavior:**
- **Default (undefined):** UID 65534 (nobody) - containers run as non-root
- **Custom UID:** `--uid 1000` - run as specified user
- **Root (UID 0):** `--uid 0` - explicitly run as root (`User` set to `0`)

**Kubernetes/OpenShift Compatibility:**
- Works with any Kubernetes distribution including OpenShift
- OpenShift's SCC overrides the `User` field with a random UID at runtime
- Setting `User: 65534` signals "designed for non-root" which is the expected pattern
- BEAM releases are UID-agnostic and work with any assigned UID

### Priority 5: Auto-Populate OCI Annotations ✅ IMPLEMENTED

**Status:** Fully implemented and tested

**Impact:** Image provenance, debugging

.NET and ko add useful labels automatically from build context. The OCI spec defines standard annotation keys that are VCS-agnostic.

**Annotations populated:**

| Annotation | Source |
|------------|--------|
| `org.opencontainers.image.source` | VCS remote URL (Git) |
| `org.opencontainers.image.revision` | VCS revision (commit SHA) |
| `org.opencontainers.image.version` | App version from build system |
| `org.opencontainers.image.created` | Build timestamp (respects SOURCE_DATE_EPOCH) |
| `org.opencontainers.image.base.name` | Base image reference |
| `org.opencontainers.image.base.digest` | Base image digest |

**Implementation Summary:**

| Component | File | Description |
|-----------|------|-------------|
| VCS behaviour | `ocibuild_vcs.erl` | `detect/1`, `get_annotations/2` |
| Git adapter | `ocibuild_vcs_git.erl` | CI env vars + git commands via ports |
| Adapter callback | `ocibuild_adapter.erl` | Optional `get_app_version/1` callback |
| Auto-annotations | `ocibuild_release.erl` | `build_auto_annotations/3` |
| CLI option | `ocibuild_rebar3.erl`, `lib/mix/tasks/ocibuild.ex` | `--no-vcs-annotations` flag |

**VCS Behaviour:**

```erlang
%% ocibuild_vcs.erl
-callback detect(Path :: file:filename()) -> boolean().
-callback get_source_url(Path :: file:filename()) -> {ok, binary()} | {error, term()}.
-callback get_revision(Path :: file:filename()) -> {ok, binary()} | {error, term()}.
```

**Git Adapter Features:**

- Walks up directory tree to find `.git/`
- CI environment variable support (checked first for reliability):
  - GitHub Actions: `GITHUB_SERVER_URL`, `GITHUB_REPOSITORY`, `GITHUB_SHA`
  - GitLab CI: `CI_PROJECT_URL`, `CI_COMMIT_SHA`
  - Azure DevOps: `BUILD_REPOSITORY_URI`, `BUILD_SOURCEVERSION`
- Falls back to git commands via Erlang ports (not `os:cmd`)
- SSH to HTTPS URL conversion for public visibility

**CLI Usage:**

```bash
# Annotations enabled by default
rebar3 ocibuild --push ghcr.io/myorg

# Disable VCS annotations
rebar3 ocibuild --push ghcr.io/myorg --no-vcs-annotations
```

**Configuration:**

```erlang
%% rebar.config
{ocibuild, [
    {vcs_annotations, false}  % Disable VCS annotations
]}.
```

```elixir
# mix.exs
ocibuild: [
  vcs_annotations: false  # Disable VCS annotations
]
```

**Future VCS adapters:**
- `ocibuild_vcs_hg.erl` - Mercurial
- `ocibuild_vcs_svn.erl` - Subversion
- `ocibuild_vcs_fossil.erl` - Fossil

### Priority 6: SBOM Generation

**Impact:** Supply chain security, compliance

Generate Software Bill of Materials from lock files. GitHub, Microsoft, and ko all use SPDX format.

**Format:** SPDX 2.2 (ISO/IEC 5962:2021) - no CycloneDX, keep it simple.

**SBOM Contents:**
- Application name + version
- All dependencies from `rebar.lock` / `mix.lock` (name, version, source)
- ERTS version (if included)
- OTP version
- Base image reference + digest

**Default Behavior:**

SBOM is always generated, embedded, and attached. No flags needed.

| Output | Behavior |
|--------|----------|
| Embed in image | Always (at `/sbom.spdx.json`) |
| Attach as OCI artifact | Always (via referrers API) |
| Export to file | Optional: `--sbom <path>` |

```bash
# SBOM embedded + attached automatically
rebar3 ocibuild --push ghcr.io/myorg

# Also export to file
rebar3 ocibuild --push ghcr.io/myorg --sbom myapp.spdx.json
```

**Implementation Steps:**

1. **New module** (`ocibuild_sbom.erl`):
   ```erlang
   -spec generate(Deps, AppInfo, Opts) -> {ok, binary()} | {error, term()}.
   %% Generates SPDX 2.2 JSON
   ```

2. **Lock file parsing via adapter** (`ocibuild_adapter.erl`):

   Reuse the `get_dependencies/1` callback (consolidates with Priority 2's `get_dependency_apps/1`):
   ```erlang
   -callback get_dependencies(State :: term()) ->
       {ok, [#{name := binary(), version := binary(), source := binary()}]} |
       {error, term()}.
   ```
   - rebar3: parse `rebar.lock`
   - Mix: parse `mix.lock`

   This single callback serves both:
   - **Smart layering** (Priority 2): extract app names to classify lib dirs
   - **SBOM generation** (Priority 6): full dependency info for SPDX output

3. **Separate file output**: Write JSON to specified path

4. **Embed in image**: Add layer containing `/sbom.spdx.json`

5. **OCI artifact attachment** (`ocibuild_registry.erl`):
   - Push SBOM as separate blob
   - Create referrer manifest with `artifactType: application/spdx+json`
   - Link via `subject` field to image manifest
   - Uses [OCI Referrers API](https://github.com/opencontainers/distribution-spec/blob/main/spec.md#listing-referrers)

**SPDX 2.2 Structure:**
```json
{
  "spdxVersion": "SPDX-2.2",
  "SPDXID": "SPDXRef-DOCUMENT",
  "name": "myapp-1.0.0",
  "packages": [
    {
      "SPDXID": "SPDXRef-Package-myapp",
      "name": "myapp",
      "versionInfo": "1.0.0",
      "downloadLocation": "https://github.com/myorg/myapp"
    },
    {
      "SPDXID": "SPDXRef-Package-cowboy",
      "name": "cowboy",
      "versionInfo": "2.10.0",
      "externalRefs": [{
        "referenceType": "purl",
        "referenceLocator": "pkg:hex/cowboy@2.10.0"
      }]
    }
  ]
}
```

### Priority 7: Image Signing

**Impact:** Supply chain security, compliance

Sign images to prove authenticity and enable verification by Kubernetes admission controllers (Kyverno, OPA Gatekeeper).

**Approach:** Native key-based signing using Erlang's `crypto` module

Zero external dependencies. Uses same OCI artifact push mechanism as SBOM (Priority 6).

```bash
# Sign with key file
rebar3 ocibuild --push ghcr.io/myorg --sign-key cosign.key

# Or via environment variable
OCIBUILD_SIGN_KEY=/path/to/key.pem rebar3 ocibuild --push ghcr.io/myorg
```

**How It Works:**

```
┌─────────────────┐
│  Image Manifest │
│  sha256:abc123  │
└────────┬────────┘
         │ sign with private key (ECDSA P-256)
         ▼
┌─────────────────┐
│   Signature     │──► Push as OCI artifact (referrer)
└─────────────────┘
```

**Implementation Steps:**

1. **Key loading** (`ocibuild_sign.erl`):
   ```erlang
   -spec load_private_key(Path :: file:filename()) ->
       {ok, crypto:key()} | {error, term()}.
   %% Support PEM-encoded ECDSA P-256 keys (cosign default)
   ```

2. **Signature generation**:
   ```erlang
   -spec sign(ManifestDigest :: binary(), PrivateKey :: crypto:key()) ->
       {ok, binary()} | {error, term()}.
   %% Uses crypto:sign/4 with ECDSA P-256 + SHA256
   ```

3. **Create signature payload** (cosign-compatible format):
   ```json
   {
     "critical": {
       "identity": {"docker-reference": "ghcr.io/myorg/myapp"},
       "image": {"docker-manifest-digest": "sha256:abc123..."},
       "type": "cosign container image signature"
     },
     "optional": {}
   }
   ```

4. **Push as OCI artifact** (`ocibuild_registry.erl`):
   - Reuse referrer push from SBOM
   - `artifactType: application/vnd.dev.cosign.simplesigning.v1+json`
   - Signature in layer, payload in config

5. **CLI options**:
   - `--sign-key <path>` - path to private key file
   - Config: `{sign_key, "/path/to/key.pem"}` / `sign_key: "/path/to/key.pem"`
   - Environment: `OCIBUILD_SIGN_KEY`

**Key Generation** (for documentation):
```bash
# Generate cosign-compatible key pair
cosign generate-key-pair
# Or with openssl
openssl ecparam -genkey -name prime256v1 -noout -out cosign.key
openssl ec -in cosign.key -pubout -out cosign.pub
```

**Verification** (user runs separately):
```bash
cosign verify --key cosign.pub ghcr.io/myorg/myapp:latest
```

**Future Enhancement:** Keyless signing via Sigstore/Fulcio (would require HTTP calls to Sigstore services, could shell out to cosign).

### Future Considerations

**Resumable Uploads:**
- Chunked uploads are implemented but resume capability is not
- If upload fails mid-way, must restart from beginning
- Would need session persistence for true resumability

**Zstd Compression:**
- Defined in media types but not implemented
- Would need zstd NIF or pure Erlang implementation

**Layer Squashing:**
- Combine multiple layers into one for smaller images

---

## OCI Specifications Reference

- [OCI Image Format Spec](https://github.com/opencontainers/image-spec)
- [OCI Distribution Spec](https://github.com/opencontainers/distribution-spec)
- [OCI Image Layout](https://github.com/opencontainers/image-spec/blob/main/image-layout.md)

### Key Media Types

```
application/vnd.oci.image.manifest.v1+json
application/vnd.oci.image.config.v1+json
application/vnd.oci.image.layer.v1.tar+gzip
application/vnd.oci.image.index.v1+json
```

### Config JSON Structure

```json
{
  "architecture": "amd64",
  "os": "linux",
  "config": {
    "Env": ["PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"],
    "Entrypoint": ["/app/bin/myapp"],
    "Cmd": ["foreground"],
    "WorkingDir": "/app",
    "ExposedPorts": {"8080/tcp": {}},
    "Labels": {"version": "1.0"},
    "User": "65534"
  },
  "rootfs": {
    "type": "layers",
    "diff_ids": [
      "sha256:...",
      "sha256:..."
    ]
  },
  "history": [
    {"created": "2024-01-01T00:00:00Z", "created_by": "ocibuild"}
  ]
}
```

Note: `User` defaults to `"65534"` (nobody) for non-root security. Override with `--uid`.

---

## Development Workflow

### Adding a New Feature

1. Write tests first in `test/ocibuild_tests.erl`
2. Implement in appropriate module
3. Update public API in `ocibuild.erl` if needed
4. Update this document

### Debugging Tips

```erlang
% Pretty print JSON
io:format("~s~n", [ocibuild_json:encode(Map)]).

% Inspect a layer
Layer = ocibuild_layer:create([{~"/test", ~"hello", 8#644}]),
io:format("Digest: ~s~n", [maps:get(digest, Layer)]).

% Test tar creation
Tar = ocibuild_tar:create([{~"/test.txt", ~"content", 8#644}]),
file:write_file("/tmp/test.tar", Tar).
% Then: tar -tvf /tmp/test.tar
```

### Publishing to Hex.pm

1. Ensure all tests pass
2. Update version in `src/ocibuild.app.src` and `mix.exs`
3. Run: `rebar3 hex publish`

---

## Example: Building a Complete Image

```erlang
%% Build from a base image
{ok, Image0} = ocibuild:from(~"alpine:3.19"),

%% Add application layer
{ok, AppBin} = file:read_file("myapp"),
Image1 = ocibuild:add_layer(Image0, [
    {~"/app/myapp", AppBin, 8#755},
    {~"/app/config.json", ~"{\"port\": 8080}", 8#644}
]),

%% Configure
Image2 = ocibuild:entrypoint(Image1, [~"/app/myapp"]),
Image3 = ocibuild:env(Image2, #{
    ~"PORT" => ~"8080",
    ~"ENV" => ~"production"
}),
Image4 = ocibuild:expose(Image3, 8080),
Image5 = ocibuild:workdir(Image4, ~"/app"),
Image6 = ocibuild:label(Image5, ~"org.opencontainers.image.version", ~"1.0.0"),

%% Set user (default is 65534/nobody when using CLI, explicit here for API)
Image7 = ocibuild:user(Image6, ~"65534"),

%% Add manifest annotation (displayed on registry UI)
Image8 = ocibuild:annotation(Image7, ~"org.opencontainers.image.description", ~"My app"),

%% Export
ok = ocibuild:save(Image8, "myapp.tar.gz").

%% Load with: podman load < myapp.tar.gz
```

**Reproducible builds:** Set `SOURCE_DATE_EPOCH` before building for identical images:
```bash
export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
```
