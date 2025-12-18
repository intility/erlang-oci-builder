# ocibuild Development Guide

This document provides a comprehensive overview of the `ocibuild` project for continuing development.

## Project Overview

**ocibuild** is a pure Erlang library for building OCI-compliant container images programmatically, without requiring Docker or any container runtime. It's inspired by:

- .NET's `Microsoft.NET.Build.Containers`
- Google's `ko` (for Go)
- Java's `jib`

### Goals

1. **Zero dependencies** — Only OTP stdlib modules (crypto, zlib, inets, ssl, json)
2. **BEAM-universal** — Works from Erlang, Elixir, Gleam, LFE via hex.pm
3. **OCI compliant** — Produces standard OCI image layouts
4. **No Docker required** — Builds and pushes images directly to registries

### Target OTP Version

- Primary target: OTP 27+ (has built-in `json` module)

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

| Function | Description | Status |
|----------|-------------|--------|
| `from/1`, `from/2`, `from/3` | Start from base image | ✅ Implemented |
| `scratch/0` | Start from empty image | ✅ Implemented |
| `add_layer/2` | Add layer with file modes | ✅ Implemented |
| `copy/3` | Copy files to destination | ✅ Implemented |
| `entrypoint/2` | Set entrypoint | ✅ Implemented |
| `cmd/2` | Set CMD | ✅ Implemented |
| `env/2` | Set environment variables | ✅ Implemented |
| `workdir/2` | Set working directory | ✅ Implemented |
| `expose/2` | Expose port | ✅ Implemented |
| `label/3` | Add config label | ✅ Implemented |
| `user/2` | Set user | ✅ Implemented |
| `annotation/3` | Add manifest annotation | ✅ Implemented |
| `push/3`, `push/4` | Push to registry | ✅ Implemented and tested |
| `save/2`, `save/3` | Save as tarball | ✅ Implemented and tested |
| `export/2` | Export as directory | ✅ Implemented and tested |

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
| Registry | URL | Auth Method |
|----------|-----|-------------|
| Docker Hub | registry-1.docker.io | Token exchange via auth.docker.io |
| GHCR | ghcr.io | Bearer token (GITHUB_TOKEN) |
| GCR | gcr.io | Bearer token |
| Quay.io | quay.io | Bearer token |
| Others | https://{registry} | Basic auth or bearer token |

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

| Module | Status |
|--------|--------|
| ocibuild_digest | ✅ Tested |
| ocibuild_json | ✅ Tested |
| ocibuild_tar | ✅ Tested |
| ocibuild_layer | ✅ Tested |
| ocibuild_manifest | ✅ Tested |
| ocibuild_layout | ✅ Tested |
| ocibuild_registry | ✅ Tested (unit + integration) |
| ocibuild_cache | ✅ Tested |
| ocibuild_release | ✅ Tested |
| ocibuild (API) | ✅ Tested |

**Total: 182 Erlang tests + 11 Elixir tests**

---

## What's NOT Implemented (TODO)

### High Priority

1. **Multi-Platform Images**
   - No support for manifest lists / image indexes
   - Can't build multi-arch images (amd64 + arm64)

### Medium Priority

2. **Resumable Uploads**
   - Chunked uploads are implemented but resume capability is not
   - If upload fails mid-way, must restart from beginning
   - Would need session persistence for true resumability

3. **Better Error Messages**
   - Some registry errors could be more descriptive

### Low Priority

4. **Zstd Compression**
   - Defined in media types but not implemented
   - Would need zstd NIF or pure Erlang implementation

5. **Image Signing**
   - Cosign/Notary support

6. **Squashing Layers**
   - Combine multiple layers into one

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
