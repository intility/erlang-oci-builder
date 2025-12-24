# Changelog

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
