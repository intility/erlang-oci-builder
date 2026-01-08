# Changelog

## 0.7.1 - Unreleased

### Features

- **Multiple tag support** ([#30](https://github.com/intility/erlang-oci-builder/issues/30)):
  - Push the same image with multiple tags in a single command: `-t myapp:1.0.0 -t myapp:latest`
  - Efficient: first tag does full upload, additional tags just reference the same manifest
  - Works with both build-and-push and push-tarball modes
  - Works with both single-platform and multi-platform images
  - All tags report the same digest in output

### New Functions

- `ocibuild_registry:tag_from_digest/5` - Tag an existing manifest with a new tag (no blob re-upload)

## 0.7.0 - 2026-01-08

### Features

- **Push existing OCI tarballs**:
  - Push pre-built OCI tarballs to registries without rebuilding
  - Works with both single-platform and multi-platform images
  - Tag extracted from `org.opencontainers.image.ref.name` annotation by default
  - Use `--tag` to override; manifest annotation is updated to match
  - Hybrid loading: small images (<100MB) loaded to memory, larger images extracted to temp directory

### Security

- **Tarball security validation**: Comprehensive validation of OCI tarballs to prevent malicious input:
  - Path traversal protection: Reject entries with `../` components or absolute paths
  - Symlink rejection: Prevent symlink escape attacks that could read arbitrary files
  - Hardlink rejection: Prevent hardlink-based attacks
  - Unknown entry type rejection: Only allow regular files and directories (reject devices, FIFOs, sockets)
  - Null byte detection: Defense-in-depth against null byte injection
  - Validation applies to both memory mode and disk mode loading
  - OCI index schema validation: Reject tarballs with invalid `schemaVersion` or empty manifests

### New Functions

- `ocibuild_layout:load_tarball_for_push/1,2` - Load OCI tarball for pushing without rebuilding
- `ocibuild_registry:push_blobs/6` - Push pre-loaded blobs to registry
- `ocibuild_registry:push_blobs_multi/6` - Push pre-loaded multi-platform blobs with index

## 0.6.1 - 2026-01-05

### Bugfixes

- **Fixed rebar3 plugin usage** ([#28](https://github.com/intility/erlang-oci-builder/pull/28)): The rebar3 plugin now works correctly when used as a project plugin.
  - Added `init/1` dispatch function to `ocibuild.erl` for proper plugin initialization
  - Fixed `get_base_image/2` to handle both binary and list base image values from config
  - Updated README to recommend `{project_plugins, [{ocibuild, "~> 0.6"}]}` instead of `{deps, ...}`

### Improvements

- **Improved HTTP shutdown reliability**: Extended graceful shutdown timeout and added explicit pool termination to prevent VM hangs on exit.
  - Pool is now explicitly stopped with `gen_server:stop/3` before supervisor shutdown
  - Graceful shutdown timeout increased from 1 second to 3 seconds
  - Prevents the ~10 minute delay seen in CI when HTTP connections weren't cleaning up properly

## 0.6.0 - 2026-01-02

### Breaking Changes

- **Push functions now return digest**: `ocibuild:push/3,4,5` and `ocibuild:push_multi/4,5` now return `{ok, Digest}` instead of `ok` on success. This enables CI/CD integration with attestation workflows (e.g., `actions/attest-build-provenance`).
  - Before: `ok = ocibuild:push(Image, Registry, RepoTag)`
  - After: `{ok, Digest} = ocibuild:push(Image, Registry, RepoTag)`

### Features

- **Digest output after push**: After a successful push, the full image reference with digest is printed to stdout:
  ```
  Pushed: ghcr.io/org/repo:tag@sha256:abc123...
  ```
  This format is machine-parseable for CI/CD pipelines that need the digest for signing, attestation, or verification.

### Improvements

- **OTP-supervised HTTP client** ([#23](https://github.com/intility/erlang-oci-builder/issues/23)): Refactored HTTP operations to use proper OTP supervision instead of `stand_alone` mode. This fixes CI hangs where the VM can't exit due to open http connections. 
  - New supervisor tree: `ocibuild_http_sup` → `ocibuild_http_pool` → `ocibuild_http_worker`
  - Each HTTP worker owns its own httpc profile for clean isolation
  - Clean shutdown via OTP supervision cascade (no more force-kill workarounds)
  - Removed `persistent_term` tracking and manual process cleanup
- **Domain-based source organization**: Source files reorganized into logical subdirectories:
  - `src/http/` - HTTP and registry operations
  - `src/oci/` - OCI image building modules
  - `src/adapters/` - Build system adapters
  - `src/vcs/` - Version control adapters
  - `src/util/` - Utility modules
- **Test suite reorganization**: Tests reorganized to mirror source directory structure with proper separation between adapter tests and shared release API tests

### New Modules

- `ocibuild_http` - Public api for HTTP operations with `start/0`, `stop/0`, `pmap/2,3`
- `ocibuild_http_sup` - OTP supervisor for HTTP workers
- `ocibuild_http_pool` - Coordinates parallel HTTP operations with bounded concurrency
- `ocibuild_http_worker` - Single-use worker that owns its httpc profile


## 0.5.1 - 2025-12-29

### Bugfixes

- **Fixed long filename truncation** ([#21](https://github.com/intility/erlang-oci-builder/issues/21)): Files with names exceeding 100 bytes (the ustar limit) were being silently truncated, causing missing modules at runtime. This commonly affected Elixir projects using libraries like AshAuthentication which generate long module names (e.g., `Elixir.AshAuthentication.Strategy.Password.Authentication.Strategies.Password.Resettable.Options.beam` at 101 bytes).
  - Implemented PAX extended headers (POSIX.1-2001) for paths that don't fit in ustar format
  - PAX headers support arbitrary path lengths with no practical limit
  - Backwards compatible: ustar is still used when paths fit, PAX only when needed

### Security

- **Null byte injection protection**: Paths containing null bytes (`\0`) are now rejected. Null bytes could truncate filenames when extracted by C-based tools, potentially allowing path manipulation.
- **Empty path validation**: Empty paths are now rejected with a clear error message.
- **Mode validation**: File modes outside the valid range (0-7777 octal) are now rejected. Previously, invalid modes could set unintended permissions like setuid/setgid.
- **Overflow protection**: Numeric fields (size, mtime, mode) that exceed their octal field capacity now raise errors instead of being silently truncated, which could corrupt archives.
- **Duplicate path detection**: Archives with duplicate paths now raise an error, preventing undefined extraction behavior. Detection works correctly when paths use different formats (e.g., `/app/file` and `app/file` both normalize to `./app/file`).

### Improvements

- **Clearer error messages**: All validation errors now include the offending value for easier debugging:
  - `{null_byte, Path}` - Path contains null byte
  - `{empty_path, <<>>}` - Empty path provided
  - `{path_traversal, Path}` - Path contains `..`
  - `{invalid_mode, Mode}` - Mode outside valid range
  - `{duplicate_paths, [Path]}` - Duplicate paths in file list
  - `{octal_overflow, N, Width}` - Value too large for field
- **Iteration guard**: PAX length calculation now has a maximum iteration limit to prevent theoretical infinite loops.
- **Code cleanup**: Reorganized TAR header field macros with unused offset definitions preserved as documentation comments.

## 0.5.0 - 2025-12-25

### Features

- **SBOM generation**: Automatic SPDX 2.2 Software Bill of Materials generation for supply chain security
  - Embedded in every image at `/sbom.spdx.json`
  - Attached as OCI referrer artifact when pushing to registries that support the referrers API
  - Includes application dependencies, ERTS version, OTP version, and base image reference
  - Package URLs (PURLs) generated for all dependencies (hex, github, gitlab, bitbucket, generic)
- **New `--sbom` CLI flag**: Export SBOM to a local file path (e.g., `--sbom myapp.spdx.json`)
- **New `ocibuild_sbom` module**: Public API for SBOM generation
  - `generate/2` - Generate SPDX 2.2 JSON from dependencies and options
  - `media_type/0` - Returns `application/spdx+json`
  - `to_purl/1` - Convert dependency to Package URL
  - `build_referrer_manifest/4` - Build OCI referrer manifest for artifact attachment
- **OCI Referrers API support**: New `ocibuild_registry:push_referrer/7,8` for attaching artifacts to images

### Improvements

- **VCS-agnostic URL handling**: SBOM source URLs support multiple VCS suffixes (`.git`, `.hg`, `.svn`)
- **SPDX ID sanitization**: Package names are sanitized to comply with SPDX ID format (`[a-zA-Z0-9.-]+`)
- **URI encoding for namespaces**: Document namespace URLs are properly percent-encoded per RFC 3986
- **Graceful SBOM failures**: SBOM generation errors are logged as warnings without failing the build
- **Silent referrer skip**: Registries without referrer support are handled gracefully (no errors)
- **Reusable manifest utilities**: `ocibuild_layout` now exports `build_config_blob/1` and `build_layer_descriptors/1`

### Documentation

- Updated README with SBOM feature description
- Updated CLAUDE.md with `--sbom` CLI option and `ocibuild_sbom` module
- Updated AGENTS.md to mark SBOM generation as implemented

## 0.4.0 - 2025-12-25

### Features

- **Smart dependency layering**: Releases are now automatically split into multiple OCI layers for optimal caching
  - **ERTS layer**: Contains ERTS and OTP libraries (when `include_erts: true`)
  - **Deps layer**: Contains third-party dependencies from lock file
  - **App layer**: Contains your application code, `bin/`, and `releases/`
  - Uses lock file (`rebar.lock` / `mix.lock`) as source of truth for classification
  - Falls back to single layer when lock file is unavailable (backward compatible)
  - Typical improvement: 80-90% smaller uploads when only app code changes
- **New `Ocibuild.Lock` module**: Shared Elixir module for parsing `mix.lock` files
  - Uses `Mix.Dep.Lock.read/1` for safe lockfile parsing (no `Code.eval_string`)
  - Supports hex dependencies (7 and 8-element tuples) and git dependencies
- **New `get_dependencies/1` adapter callback**: Optional callback for adapters to provide dependency info for smart layering
- **Configurable git timeout**: New `OCIBUILD_GIT_TIMEOUT` environment variable
  - Timeout in milliseconds for git network operations (default: 5000, max: 300000)
  - Useful for slow networks or large repositories

### Security

- **Port message flushing**: Fixed potential race condition where stale port messages could pollute the mailbox after git command timeout
- **URL sanitization**: CI environment variables are now sanitized to prevent URL injection attacks
  - Strips credentials, query params, fragments, and control characters from URLs
- **Timeout upper bound**: Git timeout is now capped at 5 minutes to prevent indefinite hangs

### Improvements

- **Layer partitioning functions**: New public API in `ocibuild_release`:
  - `partition_files_by_layer/5` - Split files into ERTS, deps, and app layers
  - `classify_file_layer/5` - Classify a single file path
  - `build_release_layers/5` - Build layers with smart partitioning
- **Dependency parsing for rebar3**: `ocibuild_rebar3:parse_rebar_lock/1` parses both old and new lock file formats
- **Consolidated test helpers**: Elixir tests now use shared `Ocibuild.TestHelpers` module

### Documentation

- Added "Environment Variables" section to README documenting `OCIBUILD_GIT_TIMEOUT`
- Added "Smart Dependency Layering" section to README explaining layer structure and benefits
- Documented reproducible builds requirement for layer caching to work across builds

## 0.3.0 - 2025-12-24

### Features

- **Automatic OCI annotations from VCS**: Images are now automatically annotated with version control information
  - `org.opencontainers.image.source` - Repository URL (from VCS remote or CI environment)
  - `org.opencontainers.image.revision` - Commit SHA
  - `org.opencontainers.image.version` - Application version from build system
  - `org.opencontainers.image.created` - Build timestamp (respects `SOURCE_DATE_EPOCH`)
  - `org.opencontainers.image.base.name` - Base image reference
  - `org.opencontainers.image.base.digest` - Base image digest
- **CI environment variable support**: VCS information is automatically detected from CI systems
  - GitHub Actions: `GITHUB_SERVER_URL`, `GITHUB_REPOSITORY`, `GITHUB_SHA`
  - GitLab CI: `CI_PROJECT_URL`, `CI_COMMIT_SHA`
  - Azure DevOps: `BUILD_REPOSITORY_URI`, `BUILD_SOURCEVERSION`
- **New `ocibuild_vcs` behaviour**: Pluggable VCS adapter system for future support of Mercurial, SVN, etc.
- **New `ocibuild_vcs_git` module**: Git adapter for VCS annotations based on Git repositories.
- **New `--no-vcs-annotations` CLI flag**: Disable automatic VCS annotations when not desired
- **New `vcs_annotations` config option**: Set to `false` in rebar.config or mix.exs to disable by default
- **Non-root by default**: Containers now run as UID 65534 (nobody) by default for improved security
  - Add `--uid` CLI option to override (e.g., `--uid 1000` for custom user, `--uid 0` for root)
  - Configurable via `uid` option in rebar.config or mix.exs
- **Reproducible builds**: Support for `SOURCE_DATE_EPOCH` environment variable to produce identical images from identical inputs
  - All timestamps (config `created`, history entries, TAR file mtimes) use the epoch value when set
  - Files are sorted alphabetically for deterministic layer ordering
  - Enables build verification, security audits, and registry deduplication
- **New `ocibuild_time` module**: Centralized timestamp utilities with `get_timestamp/0`, `get_iso8601/0`, and `unix_to_iso8601/1`
- **New `ocibuild_tar:create/2`**: TAR creation now accepts options map with `mtime` parameter for reproducible archives
- **New `ocibuild_layer:create/2`**: Layer creation now accepts options map for passing mtime through to TAR

### Improvements

- **Adapter `get_app_version/1` callback**: Optional callback for adapters to provide application version for annotations
- **SSH to HTTPS URL conversion**: Git SSH URLs (e.g., `git@github.com:org/repo.git`) are automatically converted to HTTPS for public visibility
- **Unified image configuration**: `configure_release_image/3` is now the single source of truth for image configuration, used by both CLI adapters and the programmatic API (`build_image/3`)
- **`build_image/3` now supports all options**: The programmatic API now supports `uid`, `annotations`, and properly clears inherited `Cmd` from base images

## 0.2.0 - 2025-12-22

### Features

- **Multi-platform image support**: Build and push images for multiple platforms (e.g., `linux/amd64,linux/arm64`) with OCI image index
- **Parallel layer downloads**: Base image layers are now downloaded in parallel with bounded concurrency
- **Parallel layer uploads**: Application and base layers are uploaded in parallel during push operations
- **Parallel multi-platform push**: When pushing multi-platform images, all platform images are pushed simultaneously
- **Multi-line progress display**: New progress bar system shows all concurrent operations with real-time updates
- **Layer caching during push**: Layers cached during save are reused during push, avoiding redundant downloads
- **Auto-detect platform**: When `--platform` is omitted, automatically detects and uses the current system platform

### Bugfixes

- **Fixed `--platform` flag being ignored**: Single platform builds now correctly use the specified platform instead of auto-detecting
- **Fixed progress bar duplication**: Progress bars are now properly cleared between save and push operations
- **Fixed GHCR authentication**: GitHub Container Registry now uses proper OAuth2 token exchange instead of Basic Auth
- **Fixed push destination parsing**: `--push ghcr.io/org` now correctly separates registry host from namespace

## 0.1.0 - 2025-12-19

### Initial Release

- Publish the initial release to [hex.pm](https://hex.pm/packages/ocibuild).
