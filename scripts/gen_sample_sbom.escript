#!/usr/bin/env escript
%% Generates a representative SBOM so CI can run SPDX validators against it.
%% Covers a hex dependency graph, a git dependency, ERTS and a base image.
%%
%% Usage (after `rebar3 compile`): escript scripts/gen_sample_sbom.escript <output-file>

main([Out]) ->
    true = code:add_patha("_build/default/lib/ocibuild/ebin"),
    Deps = [
        #{
            name => ~"plug_cowboy",
            version => ~"2.7.0",
            source => ~"hex",
            deps => [~"cowboy", ~"plug"],
            direct => true
        },
        #{
            name => ~"cowboy",
            version => ~"2.10.0",
            source => ~"hex",
            deps => [~"ranch", ~"cowlib"],
            direct => false
        },
        #{name => ~"plug", version => ~"1.14.0", source => ~"hex", deps => [], direct => false},
        #{name => ~"ranch", version => ~"1.8.0", source => ~"hex", deps => [], direct => false},
        #{name => ~"cowlib", version => ~"2.12.0", source => ~"hex", deps => [], direct => false},
        #{
            name => ~"my_git_dep",
            version => ~"v1.0.0",
            source => ~"https://github.com/example/my_git_dep.git",
            direct => true
        }
    ],
    Opts = #{
        app_name => ~"myapp",
        app_version => ~"1.0.0",
        source_url => ~"https://github.com/example/myapp",
        base_image => {~"docker.io", ~"library/debian", ~"stable-slim"},
        base_digest => ~"sha256:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
        erts_version => ~"15.2",
        otp_version => ~"27.2"
    },
    {ok, Json} = ocibuild_sbom:generate(Deps, Opts),
    ok = file:write_file(Out, Json);
main(_) ->
    io:format(standard_error, "usage: gen_sample_sbom.escript <output-file>~n", []),
    halt(1).
