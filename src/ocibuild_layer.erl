%%%-------------------------------------------------------------------
-module(ocibuild_layer).
-moduledoc """
OCI image layer creation.

An OCI layer is a gzip-compressed tar archive with two digests:
- `digest`: SHA256 of the compressed data (used for transfer/manifest)
- `diff_id`: SHA256 of the uncompressed tar (used in config rootfs)
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
Layer = ocibuild_layer:create([
    {~"/app/myapp", AppBinary, 8#755},
    {~"/app/config.json", ConfigJson, 8#644}
]).
```

Returns a layer map containing:
- `media_type`: The OCI media type for the layer
- `digest`: SHA256 digest of compressed data (for content addressing)
- `diff_id`: SHA256 digest of uncompressed tar (for config rootfs)
- `size`: Size in bytes of compressed data
- `data`: The compressed layer data
""".
-spec create([{Path :: binary(), Content :: binary(), Mode :: integer()}]) -> layer().
create(Files) ->
    create(Files, #{}).

-doc """
Create a layer from a list of files with options.

Options:
- `mtime`: Unix timestamp for file modification times (for reproducible builds)
- `layer_type`: Type of layer content (erts, deps, app) for progress display

```
%% With fixed mtime for reproducible builds
Layer = ocibuild_layer:create(Files, #{mtime => 1700000000}).

%% With layer type for labeled progress
Layer = ocibuild_layer:create(Files, #{layer_type => app}).
```
""".
-spec create(Files, Opts) -> layer() when
    Files :: [{Path :: binary(), Content :: binary(), Mode :: integer()}],
    Opts :: #{mtime => non_neg_integer(), layer_type => atom()}.
create(Files, Opts) ->
    %% Create uncompressed tar (with options for reproducibility)
    Tar = ocibuild_tar:create(Files, Opts),

    %% Calculate diff_id from uncompressed tar
    DiffId = ocibuild_digest:sha256(Tar),

    %% Compress
    Compressed = zlib:gzip(Tar),

    %% Calculate digest from compressed data
    Digest = ocibuild_digest:sha256(Compressed),

    Layer = #{
        media_type => ocibuild_manifest:layer_media_type(),
        digest => Digest,
        diff_id => DiffId,
        size => byte_size(Compressed),
        data => Compressed
    },
    %% Add layer_type if provided
    case maps:get(layer_type, Opts, undefined) of
        undefined -> Layer;
        Type -> Layer#{layer_type => Type}
    end.
