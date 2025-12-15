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
- Fallback: OTP 25+ (includes custom JSON encoder/decoder)

---

## Architecture

### Module Structure

```
src/
├── ocibuild.erl           # Public API - the main interface users interact with
├── ocibuild_tar.erl       # In-memory TAR archive builder (POSIX ustar format)
├── ocibuild_layer.erl     # OCI layer creation (tar + gzip + digests)
├── ocibuild_digest.erl    # SHA256 digest utilities
├── ocibuild_json.erl      # JSON encode/decode (OTP 27 native + fallback)
├── ocibuild_manifest.erl  # OCI manifest generation
├── ocibuild_layout.erl    # OCI image layout export (directory/tarball)
├── ocibuild_registry.erl  # Registry client (pull/push via HTTP)
└── ocibuild.app.src       # OTP application spec
```

### Data Flow

```
User Code
    │
    ▼
ocibuild.erl (Public API)
    │
    ├─► ocibuild_registry.erl ──► Pull base image manifest + config
    │
    ├─► ocibuild_layer.erl ─────► Create new layers
    │       │
    │       └─► ocibuild_tar.erl ──► Build tar in memory
    │       └─► zlib:gzip/1 ───────► Compress
    │       └─► ocibuild_digest.erl ► Calculate SHA256
    │
    ├─► ocibuild_manifest.erl ──► Generate manifest JSON
    │
    └─► ocibuild_layout.erl ────► Export to directory/tarball
        OR
        ocibuild_registry.erl ──► Push to registry
```

---

## Module Details

### ocibuild.erl (Public API)

**Status: ✅ Implemented, needs testing with real registries**

The main public interface. Key types:

```erlang
-opaque image() :: #{
    base := base_ref() | none,
    base_manifest => map(),      % Original manifest from registry
    base_config => map(),        % Original config from registry
    layers := [layer()],         % New layers added by user
    config := map()              % Modified config
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
| `from/1`, `from/2` | Start from base image | ✅ Implemented |
| `scratch/0` | Start from empty image | ✅ Implemented |
| `add_layer/2` | Add layer with file modes | ✅ Implemented |
| `copy/3` | Copy files to destination | ✅ Implemented |
| `entrypoint/2` | Set entrypoint | ✅ Implemented |
| `cmd/2` | Set CMD | ✅ Implemented |
| `env/2` | Set environment variables | ✅ Implemented |
| `workdir/2` | Set working directory | ✅ Implemented |
| `expose/2` | Expose port | ✅ Implemented |
| `label/3` | Add label | ✅ Implemented |
| `user/2` | Set user | ✅ Implemented |
| `push/3`, `push/4` | Push to registry | ✅ Implemented, ⚠️ untested |
| `save/2` | Save as tarball | ✅ Implemented, ✅ tested |
| `export/2` | Export as directory | ✅ Implemented, ✅ tested |

**Image Reference Parsing:**

The `parse_image_ref/1` function handles various formats:
- `"alpine:3.19"` → `{<<"docker.io">>, <<"library/alpine">>, <<"3.19">>}`
- `"docker.io/library/alpine:3.19"` → same as above
- `"ghcr.io/myorg/myapp:v1"` → `{<<"ghcr.io">>, <<"myorg/myapp">>, <<"v1">>}`
- `"myregistry.com:5000/myapp:latest"` → handles port in registry

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

**Status: ✅ Implemented**

Generates OCI image manifests.

```erlang
%% Returns {JsonBinary, Digest}
-spec build(ConfigDescriptor :: map(), LayerDescriptors :: [map()]) -> {binary(), binary()}.
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
  ]
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

2. **Tarball Export** (`save/2`):
   - Same structure as above, but packaged as `.tar.gz`
   - Compatible with `docker load` and `podman load`

**Current Limitation:**
- When building from a base image, only NEW layers are included in the export
- Base image layers are referenced but not downloaded/included
- This means exported tarballs from `from/1` won't work standalone (yet)

---

### ocibuild_registry.erl (Registry Client)

**Status: ⚠️ Implemented but UNTESTED with real registries**

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

**Push Flow:**
1. Get auth token
2. For each layer: check if exists, upload if not
3. Upload config blob
4. Upload manifest with tag

**Known Issues:**
- Uses deprecated `http_uri:encode/1` → should use `uri_string:quote/1` (fixed in code but untested)
- Error handling is basic
- No retry logic
- No chunked uploads for large layers
- Docker Hub token exchange needs real-world testing

---

## Testing

### Running Tests

```bash
cd ocibuild

# Compile
erlc -o _build +debug_info -I include src/*.erl test/*.erl

# Run tests
cd _build
erl -noshell -pa . -eval 'eunit:test(ocibuild_tests, [verbose])' -s init stop
```

### Test Coverage

| Module | Tests | Status |
|--------|-------|--------|
| ocibuild_digest | 2 | ✅ Passing |
| ocibuild_json | 7 | ✅ Passing |
| ocibuild_tar | 2 | ✅ Passing |
| ocibuild_layer | 1 | ✅ Passing |
| ocibuild (API) | 3 | ✅ Passing |
| ocibuild_layout | 2 | ✅ Passing |
| ocibuild_registry | 0 | ❌ No tests |

**Total: 17 tests passing**

---

## What's NOT Implemented (TODO)

### High Priority

1. **Registry Integration Testing**
   - Test `pull_manifest` with Docker Hub, GHCR
   - Test `push` flow end-to-end
   - Handle various error conditions

2. **Base Image Layer Handling**
   - Currently only pulls manifest/config, not actual layer data
   - For `save/2` to work with base images, need to either:
     - Download and include base layers, OR
     - Document that this only works for `scratch()` images

3. **Multi-Platform Images**
   - No support for manifest lists / image indexes
   - Can't build multi-arch images (amd64 + arm64)

### Medium Priority

4. **Layer Caching**
   - Cache downloaded base image layers
   - Skip re-uploading unchanged layers

5. **Streaming/Chunked Uploads**
   - Large layers should use chunked upload API
   - Current implementation loads entire layer in memory

6. **Better Error Messages**
   - Registry errors should be more descriptive
   - HTTP status codes should map to meaningful errors

7. **Progress Reporting**
   - Callback mechanism for upload/download progress

### Low Priority

8. **Zstd Compression**
   - Defined in media types but not implemented
   - Would need zstd NIF or pure Erlang implementation

9. **Image Signing**
   - Cosign/Notary support

10. **Squashing Layers**
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
      "sha256:...",  // Uncompressed layer digests
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
2. Update version in `src/ocibuild.app.src`
3. Run: `rebar3 hex publish`

---

## Example: Building a Complete Image

```erlang
%% Build from scratch (most reliable currently)
{ok, Image0} = ocibuild:scratch(),

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

%% Export
ok = ocibuild:save(Image6, "myapp.tar.gz").

%% Load with: docker load < myapp.tar.gz
```

---

## Contact / Questions

This project was initially developed through a conversation with Claude. The conversation covered:

1. Analysis of how .NET achieves fast Docker builds
2. Survey of existing BEAM ecosystem tools (none equivalent)
3. Architecture design decisions
4. Implementation of all core modules
5. Testing and bug fixes

Key design decisions documented in conversation:
- Why in-memory tar (avoiding erl_tar's file dependency)
- Why OTP 27+ target (native json module)
- Registry authentication strategies
- OCI layout format for docker/podman compatibility
