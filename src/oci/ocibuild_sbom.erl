%%%-------------------------------------------------------------------
-module(ocibuild_sbom).
-moduledoc """
SPDX 2.2 Software Bill of Materials (SBOM) generation.

Generates SPDX 2.2 (ISO/IEC 5962:2021) compliant SBOMs containing:
- Application name and version
- All dependencies from lock files (via adapter)
- ERTS version (if bundled)
- OTP version
- Base image reference and digest

## Usage

```erlang
Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],
Opts = #{
    app_name => ~"myapp",
    app_version => ~"1.0.0",
    base_image => {~"ghcr.io", ~"myorg/base", ~"v1"},
    base_digest => ~"sha256:abc123...",
    erts_version => ~"14.2",
    otp_version => ~"27.0"
},
{ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts).
```

## SPDX Specification

This module generates SPDX 2.2 JSON format as defined in:
https://spdx.github.io/spdx-spec/v2.2.2/

Package URLs (PURLs) follow the specification:
https://github.com/package-url/purl-spec
""".

-export([generate/2, media_type/0, to_purl/1]).
-export([build_referrer_manifest/4]).

%%%===================================================================
%%% Types
%%%===================================================================

-type sbom_opts() :: #{
    app_name := binary(),
    app_version => binary() | undefined,
    release_name => binary() | undefined,
    source_url => binary() | undefined,
    base_image => {binary(), binary(), binary()} | none | undefined,
    base_digest => binary() | undefined,
    erts_version => binary() | undefined,
    otp_version => binary() | undefined
}.

-type dependency() :: #{
    name := binary(),
    version := binary(),
    source := binary()
}.

-export_type([sbom_opts/0, dependency/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

-doc """
Get the media type for SPDX SBOM.

Returns the IANA registered media type for SPDX JSON format.
""".
-spec media_type() -> binary().
media_type() ->
    ~"application/spdx+json".

-doc """
Build an OCI referrer manifest for attaching the SBOM as an artifact.

Creates a manifest following the OCI referrers API specification that
links the SBOM artifact to a subject image manifest.

The manifest uses:
- Empty config blob (OCI spec requirement for artifacts)
- SBOM layer with `application/spdx+json` media type
- Subject reference pointing to the image manifest

Example:
```erlang
Manifest = ocibuild_sbom:build_referrer_manifest(
    SbomDigest, SbomSize, ImageManifestDigest, ImageManifestSize
).
```
""".
-spec build_referrer_manifest(
    ArtifactDigest :: binary(),
    ArtifactSize :: non_neg_integer(),
    SubjectDigest :: binary(),
    SubjectSize :: non_neg_integer()
) -> map().
build_referrer_manifest(ArtifactDigest, ArtifactSize, SubjectDigest, SubjectSize) ->
    %% The empty config blob - required by OCI spec for artifact manifests
    EmptyConfigBlob = ~"{}",
    EmptyConfigDigest = ocibuild_digest:sha256(EmptyConfigBlob),
    EmptyConfigSize = byte_size(EmptyConfigBlob),

    #{
        ~"schemaVersion" => 2,
        ~"mediaType" => ~"application/vnd.oci.image.manifest.v1+json",
        ~"artifactType" => media_type(),
        ~"config" => #{
            ~"mediaType" => ~"application/vnd.oci.empty.v1+json",
            ~"digest" => EmptyConfigDigest,
            ~"size" => EmptyConfigSize
        },
        ~"layers" => [
            #{
                ~"mediaType" => media_type(),
                ~"digest" => ArtifactDigest,
                ~"size" => ArtifactSize,
                ~"annotations" => #{
                    ~"org.opencontainers.image.title" => ~"sbom.spdx.json"
                }
            }
        ],
        ~"subject" => #{
            ~"mediaType" => ~"application/vnd.oci.image.manifest.v1+json",
            ~"digest" => SubjectDigest,
            ~"size" => SubjectSize
        },
        ~"annotations" => #{
            ~"org.opencontainers.image.created" => ocibuild_time:get_iso8601()
        }
    }.

-doc """
Generate an SPDX 2.2 SBOM from dependencies and options.

The SBOM includes:
- Document metadata (name, namespace, creation info)
- Main application package
- Dependency packages with PURLs
- ERTS package (if erts_version provided)
- Base image package (if base_image provided)
- Relationships between packages

Example:
```erlang
Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],
Opts = #{app_name => ~"myapp", app_version => ~"1.0.0"},
{ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts).
```
""".
-spec generate([dependency()], sbom_opts()) -> {ok, binary()}.
generate(Dependencies, #{app_name := AppName} = Opts) ->
    ReleaseName = maps:get(release_name, Opts, AppName),
    %% Get version, treating undefined as "unknown"
    VersionStr =
        case maps:get(app_version, Opts, undefined) of
            undefined -> ~"unknown";
            V -> V
        end,

    %% Document name: includes both app and release name if different
    DocName =
        case ReleaseName of
            AppName -> <<AppName/binary, "-", VersionStr/binary>>;
            _ -> <<AppName/binary, "-", ReleaseName/binary, "-", VersionStr/binary>>
        end,

    %% Generate deterministic namespace based on document content hash
    Namespace = generate_namespace(DocName),

    %% Build packages list
    MainPackage = build_package(AppName, VersionStr, Opts),
    DepPackages = [build_dependency_package(D) || D <- Dependencies],

    %% Add ERTS package if version provided
    ErtsPackages =
        case maps:get(erts_version, Opts, undefined) of
            undefined -> [];
            ErtsVer -> [build_erts_package(ErtsVer, maps:get(otp_version, Opts, undefined))]
        end,

    %% Add base image package if provided
    BasePackages =
        case maps:get(base_image, Opts, undefined) of
            undefined ->
                [];
            none ->
                [];
            BaseImage ->
                [build_base_image_package(BaseImage, maps:get(base_digest, Opts, undefined))]
        end,

    AllPackages = [MainPackage] ++ DepPackages ++ ErtsPackages ++ BasePackages,

    %% Build relationships
    SafeAppName = sanitize_spdx_id(AppName),
    Relationships = build_relationships(SafeAppName, AllPackages),

    %% Build the SPDX document
    Sbom = #{
        ~"spdxVersion" => ~"SPDX-2.2",
        ~"SPDXID" => ~"SPDXRef-DOCUMENT",
        ~"name" => DocName,
        ~"dataLicense" => ~"CC0-1.0",
        ~"creationInfo" => #{
            ~"created" => ocibuild_time:get_iso8601(),
            ~"creators" => [<<"Tool: ocibuild-", (get_ocibuild_version())/binary>>]
        },
        ~"documentNamespace" => Namespace,
        ~"documentDescribes" => [<<"SPDXRef-Package-", SafeAppName/binary>>],
        ~"packages" => AllPackages,
        ~"relationships" => Relationships
    },

    {ok, ocibuild_json:encode(Sbom)}.

-doc """
Generate a Package URL (PURL) for a dependency.

PURL format depends on the source:
- Hex packages: `pkg:hex/name@version`
- Git repos: `pkg:github/owner/repo@version` or `pkg:generic/name@version`
- Unknown: `pkg:generic/name@version`

Example:
```erlang
Dep = #{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"},
~"pkg:hex/cowboy@2.10.0" = ocibuild_sbom:to_purl(Dep).
```
""".
-spec to_purl(dependency()) -> binary().
to_purl(#{name := Name, version := Version, source := Source}) when
    Source =:= ~"hex"; Source =:= ~"hexpm"
->
    %% Hex packages (rebar3 uses "hex", Mix uses "hexpm")
    <<"pkg:hex/", Name/binary, "@", Version/binary>>;
to_purl(#{name := Name, version := Version, source := Source}) when is_binary(Source) ->
    case parse_git_url(Source) of
        {ok, {~"github.com", Repo}} ->
            <<"pkg:github/", Repo/binary, "@", Version/binary>>;
        {ok, {~"gitlab.com", Repo}} ->
            <<"pkg:gitlab/", Repo/binary, "@", Version/binary>>;
        {ok, {~"bitbucket.org", Repo}} ->
            <<"pkg:bitbucket/", Repo/binary, "@", Version/binary>>;
        {ok, {_Host, _Repo}} ->
            %% Generic git URL
            <<"pkg:generic/", Name/binary, "@", Version/binary>>;
        error ->
            <<"pkg:generic/", Name/binary, "@", Version/binary>>
    end;
to_purl(#{name := Name, version := Version}) ->
    <<"pkg:generic/", Name/binary, "@", Version/binary>>.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Generate SPDX document namespace
%% Uses a deterministic hash based on document name and timestamp for reproducibility.
%% The timestamp comes from SOURCE_DATE_EPOCH when set, ensuring reproducible builds.
-spec generate_namespace(binary()) -> binary().
generate_namespace(DocName) ->
    %% Include timestamp for uniqueness across builds (respects SOURCE_DATE_EPOCH)
    Timestamp = ocibuild_time:get_iso8601(),
    Data = <<DocName/binary, "-", Timestamp/binary>>,
    %% Use first 16 chars of SHA256 hex for a compact but unique identifier
    HashHex = ocibuild_digest:sha256(Data),
    %% Extract just the hex part after "sha256:"
    <<"sha256:", Hex/binary>> = HashHex,
    ShortHash = binary:part(Hex, 0, 16),
    %% URL-encode the document name to ensure a valid URI
    EncodedDocName = uri_encode(DocName),
    <<"https://spdx.org/spdxdocs/", EncodedDocName/binary, "-", ShortHash/binary>>.

%% @private Build the main application package
-spec build_package(binary(), binary(), sbom_opts()) -> map().
build_package(AppName, Version, Opts) ->
    SourceUrl = maps:get(source_url, Opts, undefined),
    DownloadLocation =
        case SourceUrl of
            undefined -> ~"NOASSERTION";
            Url -> Url
        end,
    SafeName = sanitize_spdx_id(AppName),
    #{
        ~"SPDXID" => <<"SPDXRef-Package-", SafeName/binary>>,
        ~"name" => AppName,
        ~"versionInfo" => Version,
        ~"downloadLocation" => DownloadLocation,
        %% filesAnalyzed: false indicates we did not analyze individual files
        %% for license/copyright info. This is standard for dependency SBOMs.
        ~"filesAnalyzed" => false,
        ~"supplier" => ~"NOASSERTION",
        ~"primaryPackagePurpose" => ~"APPLICATION"
    }.

%% @private Build a dependency package
-spec build_dependency_package(dependency()) -> map().
build_dependency_package(#{name := Name, version := Version} = Dep) ->
    PURL = to_purl(Dep),
    DownloadLocation = build_download_location(Dep),
    SafeName = sanitize_spdx_id(Name),
    #{
        ~"SPDXID" => <<"SPDXRef-Package-", SafeName/binary>>,
        ~"name" => Name,
        ~"versionInfo" => Version,
        ~"downloadLocation" => DownloadLocation,
        ~"filesAnalyzed" => false,
        ~"supplier" => ~"NOASSERTION",
        ~"externalRefs" => [
            #{
                ~"referenceCategory" => ~"PACKAGE_MANAGER",
                ~"referenceType" => ~"purl",
                ~"referenceLocator" => PURL
            }
        ]
    }.

%% @private Build download location URL from dependency
-spec build_download_location(dependency()) -> binary().
build_download_location(#{name := Name, version := Version, source := Source}) ->
    case Source of
        S when S =:= ~"hex"; S =:= ~"hexpm" ->
            %% Hex packages (rebar3 uses "hex", Mix uses "hexpm")
            <<"https://hex.pm/packages/", Name/binary, "/", Version/binary>>;
        Url when is_binary(Url) ->
            %% Git URL or other source
            Url;
        _ ->
            ~"NOASSERTION"
    end;
build_download_location(_) ->
    ~"NOASSERTION".

%% @private Build ERTS package
-spec build_erts_package(binary(), binary() | undefined) -> map().
build_erts_package(ErtsVersion, OtpVersion) ->
    Version =
        case OtpVersion of
            undefined -> ErtsVersion;
            OTP -> <<ErtsVersion/binary, " (OTP ", OTP/binary, ")">>
        end,
    PURL =
        case OtpVersion of
            undefined -> <<"pkg:generic/erlang-erts@", ErtsVersion/binary>>;
            V -> <<"pkg:generic/erlang-otp@", V/binary>>
        end,
    #{
        ~"SPDXID" => ~"SPDXRef-Package-erts",
        ~"name" => ~"erts",
        ~"versionInfo" => Version,
        ~"downloadLocation" => ~"https://github.com/erlang/otp",
        ~"filesAnalyzed" => false,
        ~"supplier" => ~"Organization: Erlang/OTP Team",
        ~"externalRefs" => [
            #{
                ~"referenceCategory" => ~"PACKAGE_MANAGER",
                ~"referenceType" => ~"purl",
                ~"referenceLocator" => PURL
            }
        ]
    }.

%% @private Build base image package
-spec build_base_image_package({binary(), binary(), binary()}, binary() | undefined) -> map().
build_base_image_package({Registry, Repo, Tag}, Digest) ->
    %% Extract image name from repo (last component)
    ImageName =
        case binary:split(Repo, ~"/", [global]) of
            [Name] -> Name;
            Parts -> lists:last(Parts)
        end,

    DownloadLocation = <<"https://", Registry/binary, "/", Repo/binary>>,

    PURL =
        case Digest of
            undefined ->
                <<"pkg:oci/", ImageName/binary, "@", Tag/binary, "?repository_url=",
                    Registry/binary>>;
            D ->
                <<"pkg:oci/", ImageName/binary, "@", D/binary, "?repository_url=", Registry/binary>>
        end,

    #{
        ~"SPDXID" => ~"SPDXRef-Package-base-image",
        ~"name" => ImageName,
        ~"versionInfo" => Tag,
        ~"downloadLocation" => DownloadLocation,
        ~"filesAnalyzed" => false,
        ~"supplier" => ~"NOASSERTION",
        ~"externalRefs" => [
            #{
                ~"referenceCategory" => ~"PACKAGE_MANAGER",
                ~"referenceType" => ~"purl",
                ~"referenceLocator" => PURL
            }
        ]
    }.

%% @private Build SPDX relationships
-spec build_relationships(binary(), [map()]) -> [map()].
build_relationships(AppName, Packages) ->
    AppRef = <<"SPDXRef-Package-", AppName/binary>>,

    %% DESCRIBES relationship from document
    DescribesRel = #{
        ~"spdxElementId" => ~"SPDXRef-DOCUMENT",
        ~"relationshipType" => ~"DESCRIBES",
        ~"relatedSpdxElement" => AppRef
    },

    %% DEPENDS_ON relationships for dependencies
    DependsOnRels = lists:filtermap(
        fun(Pkg) ->
            PkgId = maps:get(~"SPDXID", Pkg),
            %% Skip the main app package, ERTS, and base-image
            case PkgId of
                <<"SPDXRef-Package-", AppName/binary>> ->
                    false;
                ~"SPDXRef-Package-erts" ->
                    false;
                ~"SPDXRef-Package-base-image" ->
                    false;
                _ ->
                    {true, #{
                        ~"spdxElementId" => AppRef,
                        ~"relationshipType" => ~"DEPENDS_ON",
                        ~"relatedSpdxElement" => PkgId
                    }}
            end
        end,
        Packages
    ),

    %% RUNTIME_DEPENDENCY_OF for ERTS (if present)
    ErtsRels =
        case has_package_id(~"SPDXRef-Package-erts", Packages) of
            false ->
                [];
            true ->
                [
                    #{
                        ~"spdxElementId" => ~"SPDXRef-Package-erts",
                        ~"relationshipType" => ~"RUNTIME_DEPENDENCY_OF",
                        ~"relatedSpdxElement" => AppRef
                    }
                ]
        end,

    %% BUILD_TOOL_OF for base image (if present)
    BaseRels =
        case has_package_id(~"SPDXRef-Package-base-image", Packages) of
            false ->
                [];
            true ->
                [
                    #{
                        ~"spdxElementId" => ~"SPDXRef-Package-base-image",
                        ~"relationshipType" => ~"BUILD_TOOL_OF",
                        ~"relatedSpdxElement" => AppRef
                    }
                ]
        end,

    [DescribesRel] ++ DependsOnRels ++ ErtsRels ++ BaseRels.

%% @private Check if a package with the given SPDX ID exists in the packages list
-spec has_package_id(binary(), [map()]) -> boolean().
has_package_id(Id, Packages) ->
    lists:any(fun(P) -> maps:get(~"SPDXID", P) =:= Id end, Packages).

%% @private Parse git URL to extract host and repo
-spec parse_git_url(binary()) -> {ok, {binary(), binary()}} | error.
parse_git_url(Url) ->
    %% Handle various git URL formats:
    %% - https://github.com/owner/repo.git
    %% - git@github.com:owner/repo.git
    %% - https://github.com/owner/repo
    try
        case Url of
            <<"https://", Rest/binary>> ->
                parse_https_url(Rest);
            <<"http://", Rest/binary>> ->
                parse_https_url(Rest);
            <<"git@", Rest/binary>> ->
                parse_ssh_url(Rest);
            _ ->
                error
        end
    catch
        _:_ -> error
    end.

%% @private Parse HTTPS git URL
%% Handles URLs like "github.com/owner/repo.git"
-spec parse_https_url(binary()) -> {ok, {binary(), binary()}} | error.
parse_https_url(Rest) ->
    %% Split only on first "/" to separate host from path
    %% binary:split/2 without [global] splits at most once
    case binary:split(Rest, ~"/") of
        [Host, RepoPath] when byte_size(Host) > 0, byte_size(RepoPath) > 0 ->
            Repo = strip_vcs_suffix(RepoPath),
            {ok, {Host, Repo}};
        _ ->
            error
    end.

%% @private Parse SSH git URL (git@host:owner/repo.git)
-spec parse_ssh_url(binary()) -> {ok, {binary(), binary()}} | error.
parse_ssh_url(Rest) ->
    case binary:split(Rest, ~":") of
        [Host, RepoPath] ->
            Repo = strip_vcs_suffix(RepoPath),
            {ok, {Host, Repo}};
        _ ->
            error
    end.

%% @private Strip common VCS suffixes from repo path (.git, .hg, .svn)
-spec strip_vcs_suffix(binary()) -> binary().
strip_vcs_suffix(RepoPath) ->
    %% Check for common VCS suffixes at the end of the path
    case RepoPath of
        <<Prefix:(byte_size(RepoPath) - 4)/binary, ".git">> -> Prefix;
        <<Prefix:(byte_size(RepoPath) - 3)/binary, ".hg">> -> Prefix;
        <<Prefix:(byte_size(RepoPath) - 4)/binary, ".svn">> -> Prefix;
        _ -> RepoPath
    end.

%% @private Get ocibuild version from application metadata
-spec get_ocibuild_version() -> binary().
get_ocibuild_version() ->
    %% Ensure app is loaded so we can read version
    _ = application:load(ocibuild),
    case application:get_key(ocibuild, vsn) of
        {ok, Vsn} when is_list(Vsn) -> list_to_binary(Vsn);
        {ok, Vsn} when is_binary(Vsn) -> Vsn;
        _ -> ~"unknown"
    end.

%% @private Sanitize a string for use in SPDX IDs.
%% SPDX IDs must match: [a-zA-Z0-9.-]+
%% Invalid characters are replaced with hyphens.
-spec sanitize_spdx_id(binary()) -> binary().
sanitize_spdx_id(Name) ->
    <<<<(sanitize_spdx_char(C))/binary>> || <<C>> <= Name>>.

%% @private Convert a character to a valid SPDX ID character
-spec sanitize_spdx_char(byte()) -> binary().
sanitize_spdx_char(C) when C >= $a, C =< $z -> <<C>>;
sanitize_spdx_char(C) when C >= $A, C =< $Z -> <<C>>;
sanitize_spdx_char(C) when C >= $0, C =< $9 -> <<C>>;
sanitize_spdx_char($.) -> ~".";
sanitize_spdx_char($-) -> ~"-";
%% Common in Erlang names
sanitize_spdx_char($_) -> ~"-";
sanitize_spdx_char(_) -> ~"-".

%% @private URI-encode a binary string for use in URLs.
%% Encodes characters that are not unreserved per RFC 3986.
-spec uri_encode(binary()) -> binary().
uri_encode(Bin) ->
    <<<<(uri_encode_char(C))/binary>> || <<C>> <= Bin>>.

%% @private Encode a single character for URI
-spec uri_encode_char(byte()) -> binary().
uri_encode_char(C) when C >= $a, C =< $z -> <<C>>;
uri_encode_char(C) when C >= $A, C =< $Z -> <<C>>;
uri_encode_char(C) when C >= $0, C =< $9 -> <<C>>;
uri_encode_char($-) ->
    ~"-";
uri_encode_char($.) ->
    ~".";
uri_encode_char($_) ->
    ~"_";
uri_encode_char($~) ->
    ~"~";
uri_encode_char(C) ->
    %% Percent-encode all other characters
    High = C bsr 4,
    Low = C band 16#0F,
    <<"%", (hex_digit(High)), (hex_digit(Low))>>.

%% @private Convert a nibble (0-15) to a hex digit
-spec hex_digit(0..15) -> byte().
hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $A + N - 10.
