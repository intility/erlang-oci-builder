%%%-------------------------------------------------------------------
%%% @doc
%%% OCI image layer creation.
%%%
%%% An OCI layer is a gzip-compressed tar archive with two digests:
%%% - `digest': SHA256 of the compressed data (used for transfer)
%%% - `diff_id': SHA256 of the uncompressed tar (used in config)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ocibuild_layer).

-export([
    create/1,
    media_type/0,
    media_type/1
]).

-type layer() :: #{
    media_type := binary(),
    digest := binary(),
    diff_id := binary(),
    size := non_neg_integer(),
    data := binary()
}.

-export_type([layer/0]).

%% @doc Create a layer from a list of files.
%%
%% Files are specified as `{Path, Content, Mode}' tuples:
%% ```
%% Layer = ocibuild_layer:create([
%%     {<<"/app/myapp">>, AppBinary, 8#755},
%%     {<<"/app/config.json">>, ConfigJson, 8#644}
%% ]).
%% '''
%%
%% Returns a layer map containing:
%% - `media_type': The OCI media type for the layer
%% - `digest': SHA256 digest of compressed data (for content addressing)
%% - `diff_id': SHA256 digest of uncompressed tar (for config rootfs)
%% - `size': Size in bytes of compressed data
%% - `data': The compressed layer data
-spec create([{Path :: binary(), Content :: binary(), Mode :: integer()}]) -> layer().
create(Files) ->
    %% Create uncompressed tar
    Tar = ocibuild_tar:create(Files),
    
    %% Calculate diff_id from uncompressed tar
    DiffId = ocibuild_digest:sha256(Tar),
    
    %% Compress
    Compressed = zlib:gzip(Tar),
    
    %% Calculate digest from compressed data
    Digest = ocibuild_digest:sha256(Compressed),
    
    #{
        media_type => media_type(),
        digest => Digest,
        diff_id => DiffId,
        size => byte_size(Compressed),
        data => Compressed
    }.

%% @doc Return the default OCI media type for layers.
-spec media_type() -> binary().
media_type() ->
    <<"application/vnd.oci.image.layer.v1.tar+gzip">>.

%% @doc Return the media type for a specific compression format.
-spec media_type(gzip | zstd | none) -> binary().
media_type(gzip) ->
    <<"application/vnd.oci.image.layer.v1.tar+gzip">>;
media_type(zstd) ->
    <<"application/vnd.oci.image.layer.v1.tar+zstd">>;
media_type(none) ->
    <<"application/vnd.oci.image.layer.v1.tar">>.
