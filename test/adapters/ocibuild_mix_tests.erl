%%%-------------------------------------------------------------------
-module(ocibuild_mix_tests).
-moduledoc "Tests for the Mix/Elixir adapter (ocibuild_mix)".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% get_config tests
%%%===================================================================

get_config_empty_state_test() ->
    Config = ocibuild_mix:get_config(#{}),
    %% Should have defaults
    ?assertEqual(~"debian:stable-slim", maps:get(base_image, Config)),
    ?assertEqual(~"/app", maps:get(workdir, Config)),
    ?assertEqual(#{}, maps:get(env, Config)),
    ?assertEqual([], maps:get(expose, Config)),
    ?assertEqual(#{}, maps:get(labels, Config)),
    ?assertEqual(~"start", maps:get(cmd, Config)),
    ?assertEqual(undefined, maps:get(description, Config)),
    ?assertEqual(undefined, maps:get(tag, Config)),
    ?assertEqual(undefined, maps:get(output, Config)),
    ?assertEqual(undefined, maps:get(push, Config)),
    ?assertEqual(undefined, maps:get(chunk_size, Config)),
    ?assertEqual(undefined, maps:get(uid, Config)),
    ?assertEqual(true, maps:get(vcs_annotations, Config)).

get_config_with_overrides_test() ->
    State = #{
        base_image => ~"alpine:3.19",
        workdir => ~"/opt/app",
        env => #{~"LANG" => ~"en_US.UTF-8"},
        expose => [4000, 4001],
        cmd => ~"daemon",
        tag => ~"myapp:1.0.0"
    },
    Config = ocibuild_mix:get_config(State),
    ?assertEqual(~"alpine:3.19", maps:get(base_image, Config)),
    ?assertEqual(~"/opt/app", maps:get(workdir, Config)),
    ?assertEqual(#{~"LANG" => ~"en_US.UTF-8"}, maps:get(env, Config)),
    ?assertEqual([4000, 4001], maps:get(expose, Config)),
    ?assertEqual(~"daemon", maps:get(cmd, Config)),
    ?assertEqual(~"myapp:1.0.0", maps:get(tag, Config)),
    %% Non-overridden defaults should still be present
    ?assertEqual(#{}, maps:get(labels, Config)).

get_config_mix_specific_fields_test() ->
    State = #{
        release_name => myapp,
        release_path => ~"/path/to/release",
        app_version => ~"1.2.3",
        app_name => ~"myapp"
    },
    Config = ocibuild_mix:get_config(State),
    ?assertEqual(myapp, maps:get(release_name, Config)),
    ?assertEqual(~"/path/to/release", maps:get(release_path, Config)),
    ?assertEqual(~"1.2.3", maps:get(app_version, Config)),
    ?assertEqual(~"myapp", maps:get(app_name, Config)).

%%%===================================================================
%%% find_release tests
%%%===================================================================

find_release_atom_name_test() ->
    State = #{
        release_name => myapp,
        release_path => ~"/path/to/release"
    },
    Result = ocibuild_mix:find_release(State, #{}),
    ?assertEqual({ok, ~"myapp", ~"/path/to/release"}, Result).

find_release_binary_name_test() ->
    State = #{
        release_name => ~"myapp",
        release_path => ~"/path/to/release"
    },
    Result = ocibuild_mix:find_release(State, #{}),
    ?assertEqual({ok, ~"myapp", ~"/path/to/release"}, Result).

find_release_list_name_test() ->
    State = #{
        release_name => "myapp",
        release_path => ~"/path/to/release"
    },
    Result = ocibuild_mix:find_release(State, #{}),
    ?assertEqual({ok, ~"myapp", ~"/path/to/release"}, Result).

find_release_missing_name_test() ->
    State = #{
        release_path => ~"/path/to/release"
    },
    Result = ocibuild_mix:find_release(State, #{}),
    ?assertEqual({error, {missing_config, release_name}}, Result).

find_release_missing_path_test() ->
    State = #{
        release_name => myapp
    },
    Result = ocibuild_mix:find_release(State, #{}),
    ?assertEqual({error, {missing_config, release_path}}, Result).

find_release_both_missing_test() ->
    State = #{},
    Result = ocibuild_mix:find_release(State, #{}),
    %% Should fail on release_name first
    ?assertEqual({error, {missing_config, release_name}}, Result).

%%%===================================================================
%%% get_app_version tests
%%%===================================================================

get_app_version_binary_test() ->
    State = #{app_version => ~"1.2.3"},
    ?assertEqual(~"1.2.3", ocibuild_mix:get_app_version(State)).

get_app_version_list_test() ->
    State = #{app_version => "1.2.3"},
    ?assertEqual(~"1.2.3", ocibuild_mix:get_app_version(State)).

get_app_version_undefined_test() ->
    State = #{},
    ?assertEqual(undefined, ocibuild_mix:get_app_version(State)).

get_app_version_explicit_undefined_test() ->
    State = #{app_version => undefined},
    ?assertEqual(undefined, ocibuild_mix:get_app_version(State)).

get_app_version_invalid_type_test() ->
    %% Non-string/binary types should return undefined
    State = #{app_version => 123},
    ?assertEqual(undefined, ocibuild_mix:get_app_version(State)).

%%%===================================================================
%%% get_dependencies tests
%%%===================================================================

get_dependencies_empty_list_test() ->
    State = #{dependencies => []},
    ?assertEqual({ok, []}, ocibuild_mix:get_dependencies(State)).

get_dependencies_with_deps_test() ->
    State = #{
        dependencies => [
            #{name => ~"phoenix", version => ~"1.7.0", source => ~"hex"},
            #{name => ~"ecto", version => ~"3.10.0", source => ~"hex"}
        ]
    },
    {ok, Deps} = ocibuild_mix:get_dependencies(State),
    ?assertEqual(2, length(Deps)),
    [Phoenix, Ecto] = Deps,
    ?assertEqual(~"phoenix", maps:get(name, Phoenix)),
    ?assertEqual(~"1.7.0", maps:get(version, Phoenix)),
    ?assertEqual(~"hex", maps:get(source, Phoenix)),
    ?assertEqual(~"ecto", maps:get(name, Ecto)).

get_dependencies_with_atom_keys_test() ->
    %% Elixir might pass maps with atom keys
    State = #{
        dependencies => [
            #{name => phoenix, version => "1.7.0", source => hex}
        ]
    },
    {ok, Deps} = ocibuild_mix:get_dependencies(State),
    ?assertEqual(1, length(Deps)),
    [Phoenix] = Deps,
    ?assertEqual(~"phoenix", maps:get(name, Phoenix)),
    ?assertEqual(~"1.7.0", maps:get(version, Phoenix)),
    ?assertEqual(~"hex", maps:get(source, Phoenix)).

get_dependencies_undefined_test() ->
    State = #{},
    ?assertEqual({error, not_available}, ocibuild_mix:get_dependencies(State)).

get_dependencies_invalid_format_test() ->
    %% Note: strings in Erlang are lists, so we use an atom to test invalid format
    State = #{dependencies => not_a_list},
    ?assertEqual({error, invalid_format}, ocibuild_mix:get_dependencies(State)).

get_dependencies_git_source_test() ->
    State = #{
        dependencies => [
            #{
                name => ~"my_lib",
                version => ~"abc123",
                source => ~"https://github.com/org/my_lib.git"
            }
        ]
    },
    {ok, Deps} = ocibuild_mix:get_dependencies(State),
    ?assertEqual(1, length(Deps)),
    [MyLib] = Deps,
    ?assertEqual(~"my_lib", maps:get(name, MyLib)),
    ?assertEqual(~"abc123", maps:get(version, MyLib)),
    ?assertEqual(~"https://github.com/org/my_lib.git", maps:get(source, MyLib)).

