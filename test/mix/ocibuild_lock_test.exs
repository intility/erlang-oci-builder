defmodule Ocibuild.LockTest do
  use ExUnit.Case, async: true

  alias Ocibuild.Lock
  alias Ocibuild.TestHelpers

  describe "get_dependencies/0" do
    test "returns empty list when mix.lock doesn't exist" do
      # Run from a temp directory without mix.lock
      tmp_dir = TestHelpers.make_temp_dir("no_lock")
      on_exit(fn -> TestHelpers.cleanup_temp_dir(tmp_dir) end)

      File.cd!(tmp_dir, fn ->
        assert Lock.get_dependencies() == []
      end)
    end
  end

  describe "get_dependencies/1" do
    setup do
      tmp_dir = TestHelpers.make_temp_dir("lock_test")
      on_exit(fn -> TestHelpers.cleanup_temp_dir(tmp_dir) end)
      %{tmp_dir: tmp_dir, lock_path: Path.join(tmp_dir, "mix.lock")}
    end

    test "parses hex dependencies with 8-element tuple", %{lock_path: lock_path} do
      lock_content = """
      %{
        "jason": {:hex, :jason, "1.4.0", "e855647bc964a44e2f67df589ccf49105ae039d4179db7f6271dfd3843dc27d6", [:mix], [], "hexpm", "79a3791085b2a0f743ca04cec0f7be26443738779d09302e01318f97bdb82121"},
        "plug": {:hex, :plug, "1.14.0", "ba4f558468f69cbd9f6b356d25443d0b796fbdc887e03e021f379e0956ca5f79", [:mix], [{:mime, "~> 1.0 or ~> 2.0", [hex: :mime, repo: "hexpm", optional: false]}, {:plug_crypto, "~> 1.1.1 or ~> 1.2", [hex: :plug_crypto, repo: "hexpm", optional: false]}, {:telemetry, "~> 0.4.3 or ~> 1.0", [hex: :telemetry, repo: "hexpm", optional: false]}], "hexpm", "bf020432a5ed32da209e16b566f7e9e8e99d3c7c5a764654529920c00d4f1e8e"}
      }
      """

      File.write!(lock_path, lock_content)
      deps = Lock.get_dependencies(lock_path)

      assert length(deps) == 2

      jason = Enum.find(deps, &(&1.name == "jason"))
      assert jason.version == "1.4.0"
      assert jason.source == "hex"

      plug = Enum.find(deps, &(&1.name == "plug"))
      assert plug.version == "1.14.0"
      assert plug.source == "hex"
    end

    test "parses hex dependencies with 7-element tuple", %{lock_path: lock_path} do
      # Some older lock files use 7-element tuples
      lock_content = """
      %{
        "cowboy": {:hex, :cowboy, "2.10.0", "ff9ffeff91dae4ae270dd975642997afe2a1179d94b1887863e43f681a203e26", [:rebar3], [{:cowlib, "2.12.1", [hex: :cowlib, repo: "hexpm", optional: false]}], "hexpm"}
      }
      """

      File.write!(lock_path, lock_content)
      deps = Lock.get_dependencies(lock_path)

      assert length(deps) == 1

      cowboy = Enum.find(deps, &(&1.name == "cowboy"))
      assert cowboy.version == "2.10.0"
      assert cowboy.source == "hex"
    end

    test "parses git dependencies", %{lock_path: lock_path} do
      lock_content = """
      %{
        "my_dep": {:git, "https://github.com/example/my_dep.git", "abc123def456", [tag: "v1.0.0"]}
      }
      """

      File.write!(lock_path, lock_content)
      deps = Lock.get_dependencies(lock_path)

      assert length(deps) == 1

      my_dep = Enum.find(deps, &(&1.name == "my_dep"))
      assert my_dep.version == "abc123def456"
      assert my_dep.source == "https://github.com/example/my_dep.git"
    end

    test "handles unknown dependency format", %{lock_path: lock_path} do
      lock_content = """
      %{
        "unknown_dep": {:path, "../unknown_dep", []}
      }
      """

      File.write!(lock_path, lock_content)
      deps = Lock.get_dependencies(lock_path)

      assert length(deps) == 1

      unknown = Enum.find(deps, &(&1.name == "unknown_dep"))
      assert unknown.version == "unknown"
      assert unknown.source == "unknown"
    end

    test "parses mixed dependency types", %{lock_path: lock_path} do
      lock_content = """
      %{
        "jason": {:hex, :jason, "1.4.0", "hash1", [:mix], [], "hexpm", "hash2"},
        "my_git_dep": {:git, "https://github.com/example/dep.git", "deadbeef", []},
        "local_dep": {:path, "../local", []}
      }
      """

      File.write!(lock_path, lock_content)
      deps = Lock.get_dependencies(lock_path)

      assert length(deps) == 3

      jason = Enum.find(deps, &(&1.name == "jason"))
      assert jason.source == "hex"

      git_dep = Enum.find(deps, &(&1.name == "my_git_dep"))
      assert git_dep.source == "https://github.com/example/dep.git"

      local = Enum.find(deps, &(&1.name == "local_dep"))
      assert local.source == "unknown"
    end

    test "returns empty list for missing lock file" do
      deps = Lock.get_dependencies("/nonexistent/path/mix.lock")
      assert deps == []
    end

    test "returns empty list for empty lock file", %{lock_path: lock_path} do
      File.write!(lock_path, "%{}")
      deps = Lock.get_dependencies(lock_path)
      assert deps == []
    end

    test "handles lock file with many dependencies", %{lock_path: lock_path} do
      # Create a lock file with 50 dependencies
      deps_list =
        for i <- 1..50 do
          ~s("dep_#{i}": {:hex, :dep_#{i}, "#{i}.0.0", "hash#{i}", [:mix], [], "hexpm", "hash#{i}2"})
        end
        |> Enum.join(",\n  ")

      lock_content = "%{\n  #{deps_list}\n}"

      File.write!(lock_path, lock_content)
      deps = Lock.get_dependencies(lock_path)

      assert length(deps) == 50
      assert Enum.all?(deps, &(&1.source == "hex"))
    end

    test "handles dependencies with quoted atom keys", %{lock_path: lock_path} do
      # This is common when dependency names have special characters
      lock_content = """
      %{
        "plug_crypto": {:hex, :plug_crypto, "2.0.0", "hash1", [:mix], [], "hexpm", "hash2"}
      }
      """

      File.write!(lock_path, lock_content)
      deps = Lock.get_dependencies(lock_path)

      assert length(deps) == 1
      plug_crypto = hd(deps)
      assert plug_crypto.name == "plug_crypto"
      assert plug_crypto.version == "2.0.0"
    end
  end
end
