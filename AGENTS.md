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
| **Non-root by default**       | ⏳ Planned (P4)    | ✅          | ❌                | ✅              |
| **Auto OCI annotations**      | ⏳ Planned (P5)    | ✅          | ✅                | ✅              |
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
├── ocibuild_rebar3.erl    # Rebar3 provider (rebar3 ocibuild command)
├── ocibuild_release.erl   # Shared release handling for rebar3/Mix integrations
├── ocibuild_tar.erl       # In-memory TAR archive builder (POSIX ustar format)
├── ocibuild_layer.erl     # OCI layer creation (tar + gzip + digests)
├── ocibuild_digest.erl    # SHA256 digest utilities
├── ocibuild_json.erl      # JSON encode/decode (OTP 27 native + fallback)
├── ocibuild_manifest.erl  # OCI manifest generation (with annotations support)
├── ocibuild_layout.erl    # OCI image layout export (directory/tarball)
├── ocibuild_registry.erl  # Registry client (pull/push via HTTP with retry logic)
├── ocibuild_cache.erl     # Layer caching for base images
├── ocibuild_time.erl      # Timestamp utilities for reproducible builds
└── ocibuild.app.src       # OTP application spec

lib/
├── mix/tasks/ocibuild.ex      # Mix task (mix ocibuild command)
└── ocibuild/mix_release.ex    # Mix release step integration
```

### Data Flow

```
User Code
    │
    ▼
ocibuild.erl (Public API)
    │
    ├─► ocibuild_registry.erl ──► Pull base image manifest + config + layers
    │       │
    │       └─► ocibuild_cache.erl ──► Cache layers locally in _build/
    │
    ├─► ocibuild_layer.erl ─────► Create new layers
    │       │
    │       └─► ocibuild_tar.erl ──► Build tar in memory
    │       └─► zlib:gzip/1 ───────► Compress
    │       └─► ocibuild_digest.erl ► Calculate SHA256
    │
    ├─► ocibuild_manifest.erl ──► Generate manifest JSON (with annotations)
    │
    └─► ocibuild_layout.erl ────► Export to directory/tarball
        OR
        ocibuild_registry.erl ──► Push to registry
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

| Function                     | Description               | Status                    |
|------------------------------|---------------------------|---------------------------|
| `from/1`, `from/2`, `from/3` | Start from base image     | ✅ Implemented            |
| `scratch/0`                  | Start from empty image    | ✅ Implemented            |
| `add_layer/2`                | Add layer with file modes | ✅ Implemented            |
| `copy/3`                     | Copy files to destination | ✅ Implemented            |
| `entrypoint/2`               | Set entrypoint            | ✅ Implemented            |
| `cmd/2`                      | Set CMD                   | ✅ Implemented            |
| `env/2`                      | Set environment variables | ✅ Implemented            |
| `workdir/2`                  | Set working directory     | ✅ Implemented            |
| `expose/2`                   | Expose port               | ✅ Implemented            |
| `label/3`                    | Add config label          | ✅ Implemented            |
| `user/2`                     | Set user                  | ✅ Implemented            |
| `annotation/3`               | Add manifest annotation   | ✅ Implemented            |
| `push/3`, `push/4`           | Push to registry          | ✅ Implemented and tested |
| `save/2`, `save/3`           | Save as tarball           | ✅ Implemented and tested |
| `export/2`                   | Export as directory       | ✅ Implemented and tested |

**Image Reference Parsing:**

The `parse_image_ref/1` function handles various formats:
- `"alpine:3.19"` → `{<<"docker.io">>, <<"library/alpine">>, <<"3.19">>}`
- `"docker.io/library/alpine:3.19"` → same as above
- `"ghcr.io/myorg/myapp:v1"` → `{<<"ghcr.io">>, <<"myorg/myapp">>, <<"v1">>}`
- `"myregistry.com:5000/myapp:latest"` → handles port in registry

---

### ocibuild_release.erl (Shared Release Handling)

**Status: ✅ Implemented and tested**

Provides common functionality for collecting release files and building OCI images, used by both rebar3 and Mix integrations.

**Key Functions:**

```erlang
%% Collect files from release directory (with symlink security)
-spec collect_release_files(ReleasePath) -> {ok, Files} | {error, term()}.

%% Build OCI image from release files
-spec build_image(BaseImage, Files, ReleaseName, Workdir, EnvMap, ExposePorts, Labels, Cmd, Opts) ->
    {ok, image()} | {error, term()}.
```

**Security Features:**
- Symlinks pointing outside the release directory are rejected
- Broken symlinks are skipped with a warning
- Path traversal via `..` components is prevented

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
%% Returns <<"sha256:abc123...">>
-spec sha256(binary()) -> digest().

%% Extract parts
-spec algorithm(digest()) -> binary().  % <<"sha256">>
-spec encoded(digest()) -> binary().    % <<"abc123...">>
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
{ok, Images} = ocibuild:from(<<"alpine:3.19">>, #{
    platforms => [<<"linux/amd64">>, <<"linux/arm64">>]
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

### Priority 2: Reproducible Builds

**Impact:** Build verification, security audits

ko and jib produce identical images from identical inputs. Currently ocibuild has non-deterministic timestamps and file ordering.

**Approach:** Support `SOURCE_DATE_EPOCH` environment variable ([spec](https://reproducible-builds.org/docs/source-date-epoch/))

```bash
# Set to git commit timestamp for reproducible builds
export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
rebar3 ocibuild --push ghcr.io/myorg
```

No CLI flag - environment variable only (it's the standard).

**Sources of Non-Determinism:**

| Source | Fix |
|--------|-----|
| Config `created` timestamp | Use `SOURCE_DATE_EPOCH` |
| History `created` timestamps | Use `SOURCE_DATE_EPOCH` |
| TAR file `mtime` headers | Use `SOURCE_DATE_EPOCH` for all files |
| File ordering in TAR | Sort alphabetically by path |

**Implementation Steps:**

1. **Read SOURCE_DATE_EPOCH** (`ocibuild_release.erl` or new utility):
   ```erlang
   get_source_date() ->
       case os:getenv("SOURCE_DATE_EPOCH") of
           false ->
               erlang:system_time(second);
           Epoch ->
               list_to_integer(Epoch)
       end.
   ```

2. **Update TAR creation** (`ocibuild_tar.erl`):
   - Accept optional timestamp parameter
   - Sort files alphabetically before building archive
   - Use provided timestamp for all mtime headers
   ```erlang
   -spec create(Files, Opts) -> binary() when
       Files :: [{Path, Content, Mode}],
       Opts :: #{mtime => non_neg_integer()}.
   ```

3. **Update config timestamps** (`ocibuild.erl` / `ocibuild_manifest.erl`):
   - Use `SOURCE_DATE_EPOCH` for `created` field in config
   - Use `SOURCE_DATE_EPOCH` for history entries

4. **Consistent file ordering** (`ocibuild_release.erl`):
   ```erlang
   collect_release_files(ReleasePath) ->
       Files = collect_files_recursive(ReleasePath),
       %% Sort for reproducibility
       lists:sort(fun({PathA, _, _}, {PathB, _, _}) -> PathA =< PathB end, Files).
   ```

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
   %% e.g., [#{name => <<"cowboy">>, version => <<"2.10.0">>, source => <<"hex">>}, ...]
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
{<<"cowboy">>, {pkg, <<"cowboy">>, <<"2.10.0">>}, 0}.
```

mix.lock format:
```elixir
%{"cowboy": {:hex, :cowboy, "2.10.0", ...}}
```

### Priority 4: Non-Root by Default

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

**Implementation Steps:**

1. **Add `--uid` CLI option** (default: 65534):
   - Update `ocibuild_rebar3.erl` and `lib/mix/tasks/ocibuild.ex`
   - Config option: `{uid, 65534}` / `uid: 65534`

2. **Set User in image config** (`ocibuild.erl`):
   ```erlang
   %% When building release image, apply UID
   Image = ocibuild:user(Image0, integer_to_binary(Uid))
   ```

   Resulting config:
   ```json
   {
     "config": {
       "User": "65534"
     }
   }
   ```

3. **File permissions**: Keep current behavior (root owns files, world-readable). BEAM files only need to be readable, not writable. If the app needs writable directories (logs, mnesia), users should either:
   - Mount a volume at runtime
   - Use a base image with appropriate directory permissions

### Priority 5: Auto-Populate OCI Annotations

**Impact:** Image provenance, debugging

.NET and ko add useful labels automatically from build context. The OCI spec defines standard annotation keys that are VCS-agnostic.

**Annotations to populate:**

| Annotation | Source |
|------------|--------|
| `org.opencontainers.image.source` | VCS remote URL |
| `org.opencontainers.image.revision` | VCS revision (commit SHA, SVN rev, etc.) |
| `org.opencontainers.image.version` | App version from build system |
| `org.opencontainers.image.created` | Build timestamp |
| `org.opencontainers.image.base.name` | Base image reference |
| `org.opencontainers.image.base.digest` | Base image digest |

**Approach:** VCS behaviour with pluggable adapters

The OCI spec is VCS-agnostic. Use a behaviour to support Git now, others later:

```erlang
%% ocibuild_vcs.erl
-callback detect(Path :: file:filename()) -> boolean().
-callback get_source_url(Path :: file:filename()) -> {ok, binary()} | {error, term()}.
-callback get_revision(Path :: file:filename()) -> {ok, binary()} | {error, term()}.
```

**Implementation Steps:**

1. **New behaviour** (`ocibuild_vcs.erl`):
   ```erlang
   -spec detect(Path) -> {ok, module()} | not_found.
   detect(Path) ->
       Adapters = [ocibuild_vcs_git], % Add more later
       find_vcs(Path, Adapters).
   ```

2. **Git adapter** (`ocibuild_vcs_git.erl`):
   - `detect/1` - check for `.git/` directory
   - `get_source_url/1` - try CI env vars first (`GITHUB_SERVER_URL`, `GITHUB_REPOSITORY`), fall back to `git remote get-url origin`
   - `get_revision/1` - try CI env vars first (`GITHUB_SHA`, `CI_COMMIT_SHA`), fall back to `git rev-parse HEAD`

3. **Version from adapter** (`ocibuild_adapter.erl`):
   ```erlang
   -callback get_app_version(State :: term()) -> {ok, binary()} | {error, term()}.
   ```
   - rebar3: parse `.app.src` for `{vsn, "1.2.3"}`
   - Mix: get from `Mix.Project.config()[:version]`

4. **Base image info** (`ocibuild.erl`):
   - Already have base image reference and digest after `from/1`
   - Store in image record, add as annotations

5. **CLI opt-out**:
   - `--no-vcs-annotations` flag to disable VCS detection
   - Config option: `{vcs_annotations, false}` / `vcs_annotations: false`

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
    "Entrypoint": ["/app/myapp"],
    "Cmd": ["--help"],
    "WorkingDir": "/app",
    "ExposedPorts": {"8080/tcp": {}},
    "Labels": {"version": "1.0"},
    "User": "nobody"
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
Layer = ocibuild_layer:create([{<<"/test">>, <<"hello">>, 8#644}]),
io:format("Digest: ~s~n", [maps:get(digest, Layer)]).

% Test tar creation
Tar = ocibuild_tar:create([{<<"/test.txt">>, <<"content">>, 8#644}]),
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
{ok, Image0} = ocibuild:from(<<"alpine:3.19">>),

%% Add application layer
{ok, AppBin} = file:read_file("myapp"),
Image1 = ocibuild:add_layer(Image0, [
    {<<"/app/myapp">>, AppBin, 8#755},
    {<<"/app/config.json">>, <<"{\"port\": 8080}">>, 8#644}
]),

%% Configure
Image2 = ocibuild:entrypoint(Image1, [<<"/app/myapp">>]),
Image3 = ocibuild:env(Image2, #{
    <<"PORT">> => <<"8080">>,
    <<"ENV">> => <<"production">>
}),
Image4 = ocibuild:expose(Image3, 8080),
Image5 = ocibuild:workdir(Image4, <<"/app">>),
Image6 = ocibuild:label(Image5, <<"org.opencontainers.image.version">>, <<"1.0.0">>),

%% Add manifest annotation (displayed on registry UI)
Image7 = ocibuild:annotation(Image6, <<"org.opencontainers.image.description">>, <<"My app">>),

%% Export
ok = ocibuild:save(Image7, "myapp.tar.gz").

%% Load with: podman load < myapp.tar.gz
```
