%%%-------------------------------------------------------------------
-module(ocibuild_layer).
-moduledoc """
OCI image layer creation.

An OCI layer is a compressed tar archive with two digests:
- `digest`: SHA256 of the compressed data (used for transfer/manifest)
- `diff_id`: SHA256 of the uncompressed tar (used in config rootfs)

## Compression

Layers support multiple compression algorithms:
- `gzip`: Always available (default on OTP 27)
- `zstd`: Available on OTP 28+ (default when available)
- `auto`: Automatically selects best available compression

Use `auto` (the default) to get zstd on OTP 28+ with automatic fallback
to gzip on OTP 27.
""".

-export([create/1, create/2]).

-type layer() ::
    #{
        media_type := binary(),
        digest := binary(),
        diff_id := binary(),
        size := non_neg_integer(),
        data := binary()
    }.

-export_type([layer/0]).

-doc """
Create a layer from a list of files.

Files are specified as `{Path, Content, Mode}` tuples:
```
{ok, Layer} = ocibuild_layer:create([
    {~"/app/myapp", AppBinary, 8#755},
    {~"/app/config.json", ConfigJson, 8#644}
]).
```

Returns `{ok, Layer}` where Layer contains:
- `media_type`: The OCI media type for the layer
- `digest`: SHA256 digest of compressed data (for content addressing)
- `diff_id`: SHA256 digest of uncompressed tar (for config rootfs)
- `size`: Size in bytes of compressed data
- `data`: The compressed layer data

Returns `{error, Reason}` if compression fails (e.g., zstd requested on OTP 27).
""".
-spec create([{Path :: binary(), Content :: binary(), Mode :: integer()}]) ->
    {ok, layer()} | {error, term()}.
create(Files) ->
    create(Files, #{}).

-doc """
Create a layer from a list of files with options.

Options:
- `mtime`: Unix timestamp for file modification times (for reproducible builds)
- `layer_type`: Type of layer content (erts, deps, app) for progress display
- `compression`: Compression algorithm (`gzip`, `zstd`, or `auto`). Default: `auto`

```
%% With fixed mtime for reproducible builds
Layer = ocibuild_layer:create(Files, #{mtime => 1700000000}).

%% With layer type for labeled progress
Layer = ocibuild_layer:create(Files, #{layer_type => app}).

%% With explicit compression (zstd requires OTP 28+)
{ok, Layer} = ocibuild_layer:create(Files, #{compression => zstd}).

%% Auto-select best available (default)
{ok, Layer} = ocibuild_layer:create(Files, #{compression => auto}).
```
""".
-spec create(Files, Opts) -> {ok, layer()} | {error, term()} when
    Files :: [{Path :: binary(), Content :: binary(), Mode :: integer()}],
    Opts :: #{
        mtime => non_neg_integer(),
        layer_type => atom(),
        compression => ocibuild_compress:compression()
    }.
create(Files, Opts) ->
    %% Create uncompressed tar (with options for reproducibility)
    Tar = ocibuild_tar:create(Files, Opts),

    %% Calculate diff_id from uncompressed tar
    DiffId = ocibuild_digest:sha256(Tar),

    %% Get compression type (default: auto)
    Compression = maps:get(compression, Opts, auto),

    %% Compress with selected algorithm
    case ocibuild_compress:compress(Tar, Compression) of
        {ok, Compressed} ->
            %% Resolve actual compression used (auto -> gzip or zstd)
            ResolvedCompression = ocibuild_compress:resolve(Compression),

            %% Calculate digest from compressed data
            Digest = ocibuild_digest:sha256(Compressed),

            Layer = #{
                media_type => ocibuild_manifest:layer_media_type(ResolvedCompression),
                digest => Digest,
                diff_id => DiffId,
                size => byte_size(Compressed),
                data => Compressed
            },
            %% Add layer_type if provided
            FinalLayer =
                case maps:get(layer_type, Opts, undefined) of
                    undefined -> Layer;
                    Type -> Layer#{layer_type => Type}
                end,
            {ok, FinalLayer};
        {error, _Reason} = Error ->
            Error
    end.
