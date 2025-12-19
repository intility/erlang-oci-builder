%%%-------------------------------------------------------------------
-module(ocibuild_test_helpers).
-moduledoc """
Shared test helper functions for ocibuild tests.
""".

-export([temp_dir/0, make_temp_dir/1, make_temp_file/2, cleanup_temp_dir/1]).

%% @doc Get the system temp directory in a cross-platform way
-spec temp_dir() -> string().
temp_dir() ->
    case os:type() of
        {win32, _} ->
            case os:getenv("TEMP") of
                false ->
                    case os:getenv("TMP") of
                        false ->
                            "C:\\Temp";
                        TmpDir ->
                            TmpDir
                    end;
                TempDir ->
                    TempDir
            end;
        _ ->
            "/tmp"
    end.

%% @doc Create a unique temporary directory
-spec make_temp_dir(string()) -> string().
make_temp_dir(Prefix) ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    DirName = Prefix ++ "_" ++ Unique,
    TmpDir = filename:join(temp_dir(), DirName),
    ok =
        filelib:ensure_dir(
            filename:join(TmpDir, "placeholder")
        ),
    case file:make_dir(TmpDir) of
        ok ->
            TmpDir;
        {error, eexist} ->
            %% Clean up existing directory and recreate it
            case cleanup_temp_dir(TmpDir) of
                ok ->
                    case file:make_dir(TmpDir) of
                        ok ->
                            TmpDir;
                        {error, Reason2} ->
                            erlang:error({temp_dir_create_failed, TmpDir, Reason2})
                    end;
                {error, Reason1} ->
                    erlang:error({temp_dir_cleanup_failed, TmpDir, Reason1})
            end
    end.

%% @doc Create a unique temporary file path
-spec make_temp_file(string(), string()) -> string().
make_temp_file(Prefix, Extension) ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    FileName = Prefix ++ "_" ++ Unique ++ Extension,
    filename:join(temp_dir(), FileName).

%% @doc Recursively delete a directory (cross-platform)
-spec cleanup_temp_dir(string()) -> ok | {error, term()}.
cleanup_temp_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Files} = file:list_dir(Dir),
            lists:foreach(
                fun(File) ->
                    Path = filename:join(Dir, File),
                    case filelib:is_dir(Path) of
                        true ->
                            cleanup_temp_dir(Path);
                        false ->
                            file:delete(Path)
                    end
                end,
                Files
            ),
            file:del_dir(Dir);
        false ->
            ok
    end.
