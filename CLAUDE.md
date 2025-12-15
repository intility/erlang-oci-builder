# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Summary

**ocibuild** is a pure Erlang library for building OCI container images without Docker. Think of it as the BEAM equivalent of .NET's `Microsoft.NET.Build.Containers`, Google's `ko`, or Java's `jib`.

## Build & Test Commands

```bash
# Compile (without rebar3)
erlc -o _build +debug_info -I include src/*.erl test/*.erl

# Run all tests
cd _build && erl -noshell -pa . -eval 'eunit:test(ocibuild_tests, [verbose])' -s init stop

# Run a single test (replace test_name with actual test function name)
cd _build && erl -noshell -pa . -eval 'eunit:test({ocibuild_tests, test_name})' -s init stop

# With rebar3
rebar3 compile
rebar3 eunit
rebar3 eunit --test=ocibuild_tests:test_name_test  # Single test
```

## Architecture

```
ocibuild.erl          → Public API (from, copy, push, save, etc.)
ocibuild_tar.erl      → In-memory TAR builder (POSIX ustar, custom implementation)
ocibuild_layer.erl    → Creates OCI layers (tar + gzip + SHA256)
ocibuild_digest.erl   → SHA256 digest utilities
ocibuild_json.erl     → JSON encode/decode (OTP 27 native + fallback for OTP 25+)
ocibuild_manifest.erl → OCI manifest generation
ocibuild_layout.erl   → Export to directory or tarball
ocibuild_registry.erl → Registry HTTP client (pull/push)
```

**Data Flow:**
```
User API (ocibuild.erl)
    ├─► ocibuild_registry.erl ──► Pull base image manifest + config
    ├─► ocibuild_layer.erl ─────► Create layers (uses ocibuild_tar + zlib + ocibuild_digest)
    ├─► ocibuild_manifest.erl ──► Generate manifest JSON
    └─► ocibuild_layout.erl ────► Export to directory/tarball
        OR ocibuild_registry.erl ► Push to registry
```

## Key Design Decisions

1. **In-memory TAR**: `ocibuild_tar.erl` implements POSIX ustar format manually because `:erl_tar` requires file I/O.
2. **Two Digests per Layer**: OCI requires `digest` (compressed) for manifests and `diff_id` (uncompressed) for config.
3. **OTP 27+ Target**: Uses native `json` module with fallback for OTP 25+.
4. **Zero Dependencies**: Only OTP stdlib (crypto, zlib, inets, ssl).

## Current Status

**Working:** tar creation, layer creation, JSON encoding, image configuration, OCI layout export, tarball export (docker load compatible). 17 unit tests passing.

**Untested:** Registry pull/push, Docker Hub token auth, GHCR/other registry auth.

**Not Implemented:** Multi-platform images, layer caching, downloading base image layers (only metadata), chunked uploads, zstd compression.

## Common Development Tasks

### Add a new image configuration option

1. Add export to `ocibuild.erl`
2. Implement function using `set_config_field/3`
3. Add test to `ocibuild_tests.erl`

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

## Key Types (from ocibuild.erl)

```erlang
-opaque image() :: #{
    base := base_ref() | none,
    base_manifest => map(),
    base_config => map(),
    layers := [layer()],
    config := map()
}.

-type layer() :: #{
    media_type := binary(),
    digest := binary(),      % sha256:... of compressed data
    diff_id := binary(),     % sha256:... of uncompressed tar
    size := non_neg_integer(),
    data := binary()
}.
```

## Files to Read First

1. `DEVELOPMENT.md` - Comprehensive development guide with OCI spec details
2. `src/ocibuild.erl` - Public API
3. `src/ocibuild_tar.erl` - Core tar implementation
4. `test/ocibuild_tests.erl` - Usage examples
