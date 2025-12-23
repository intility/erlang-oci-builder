# Changelog

## 0.3.0 - 2025-12-23

### Features

- **Reproducible builds**: Support for `SOURCE_DATE_EPOCH` environment variable to produce identical images from identical inputs
  - All timestamps (config `created`, history entries, TAR file mtimes) use the epoch value when set
  - Files are sorted alphabetically for deterministic layer ordering
  - Enables build verification, security audits, and registry deduplication
- **New `ocibuild_time` module**: Centralized timestamp utilities with `get_timestamp/0`, `get_iso8601/0`, and `unix_to_iso8601/1`
- **New `ocibuild_tar:create/2`**: TAR creation now accepts options map with `mtime` parameter for reproducible archives
- **New `ocibuild_layer:create/2`**: Layer creation now accepts options map for passing mtime through to TAR

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
