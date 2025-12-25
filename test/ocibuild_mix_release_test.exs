defmodule Ocibuild.MixReleaseTest do
  use ExUnit.Case, async: true

  # Test helper module that replicates private function logic for testing
  defmodule TestHelpers do
    @moduledoc false

    def format_error({:release_not_found, reason}),
      do: "Failed to find release: #{inspect(reason)}"

    def format_error({:collect_failed, reason}),
      do: "Failed to collect release files: #{inspect(reason)}"

    def format_error({:build_failed, reason}), do: "Failed to build image: #{inspect(reason)}"
    def format_error({:save_failed, reason}), do: "Failed to save image: #{inspect(reason)}"
    def format_error({:push_failed, reason}), do: "Failed to push image: #{inspect(reason)}"

    def format_error({:bundled_erts, message}),
      do: "Multi-platform build failed: #{message}"

    def format_error({:nif_warning, files}),
      do: "Warning: Native code detected that may not be portable: #{inspect(files)}"

    def format_error(reason), do: "OCI build error: #{inspect(reason)}"

    def to_erlang_map(map) when is_map(map) do
      Map.new(map, fn {k, v} -> {to_binary(k), to_binary(v)} end)
    end

    def to_binary(value) when is_binary(value), do: value
    def to_binary(value) when is_atom(value), do: Atom.to_string(value)
    def to_binary(value) when is_list(value), do: to_string(value)
    def to_binary(value), do: to_string(value)

    def get_tag(ocibuild_config, release_name, version) do
      case Keyword.get(ocibuild_config, :tag) do
        nil -> "#{release_name}:#{version}"
        tag -> tag
      end
    end

    def get_description(ocibuild_config) do
      case Keyword.get(ocibuild_config, :description) do
        nil -> :undefined
        desc -> to_binary(desc)
      end
    end

    def get_push(ocibuild_config) do
      case Keyword.get(ocibuild_config, :push) do
        nil -> nil
        registry -> to_binary(registry)
      end
    end

    def get_platform(ocibuild_config) do
      case Keyword.get(ocibuild_config, :platform) do
        nil -> nil
        platform when is_binary(platform) -> platform
        platform when is_list(platform) -> to_binary(platform)
      end
    end

    def get_chunk_size(ocibuild_config) do
      case Keyword.get(ocibuild_config, :chunk_size) do
        nil -> nil
        size when is_integer(size) and size >= 1 and size <= 100 -> size * 1024 * 1024
        _size -> nil
      end
    end

    # Simulate building state from a release struct and config
    def build_state(release_name, release_version, release_path, app_name, ocibuild_config) do
      %{
        release_name: release_name,
        app_name: app_name,
        release_path: to_charlist(release_path),
        base_image: Keyword.get(ocibuild_config, :base_image, "debian:stable-slim") |> to_binary(),
        workdir: Keyword.get(ocibuild_config, :workdir, "/app") |> to_binary(),
        env: Keyword.get(ocibuild_config, :env, %{}) |> to_erlang_map(),
        expose: Keyword.get(ocibuild_config, :expose, []),
        labels: Keyword.get(ocibuild_config, :labels, %{}) |> to_erlang_map(),
        cmd: Keyword.get(ocibuild_config, :cmd, "start") |> to_binary(),
        description: get_description(ocibuild_config),
        tag: get_tag(ocibuild_config, release_name, release_version) |> to_binary(),
        output: nil,
        push: get_push(ocibuild_config),
        chunk_size: get_chunk_size(ocibuild_config),
        platform: get_platform(ocibuild_config),
        app_version: to_binary(release_version),
        uid: Keyword.get(ocibuild_config, :uid),
        vcs_annotations: Keyword.get(ocibuild_config, :vcs_annotations, true)
      }
    end
  end

  describe "format_error/1" do
    test "formats release_not_found" do
      error = TestHelpers.format_error({:release_not_found, :enoent})
      assert error == "Failed to find release: :enoent"
    end

    test "formats collect_failed" do
      error = TestHelpers.format_error({:collect_failed, {:permission_denied, "/path"}})
      assert error =~ "Failed to collect release files"
    end

    test "formats build_failed" do
      error = TestHelpers.format_error({:build_failed, {:network_error, :timeout}})
      assert error =~ "Failed to build image"
    end

    test "formats save_failed" do
      error = TestHelpers.format_error({:save_failed, :enospc})
      assert error == "Failed to save image: :enospc"
    end

    test "formats push_failed" do
      error = TestHelpers.format_error({:push_failed, {:http_error, 401}})
      assert error =~ "Failed to push image"
    end

    test "formats bundled_erts" do
      error = TestHelpers.format_error({:bundled_erts, "Cannot build multi-platform with ERTS"})
      assert error == "Multi-platform build failed: Cannot build multi-platform with ERTS"
    end

    test "formats nif_warning" do
      error = TestHelpers.format_error({:nif_warning, ["nif.so"]})
      assert error =~ "Native code detected"
    end

    test "formats generic error" do
      error = TestHelpers.format_error({:unexpected, :error})
      assert error =~ "OCI build error"
    end
  end

  describe "get_tag/3" do
    test "uses config tag when provided" do
      config = [tag: "custom:tag"]
      assert TestHelpers.get_tag(config, :myapp, "1.0.0") == "custom:tag"
    end

    test "generates default tag from release name and version" do
      assert TestHelpers.get_tag([], :myapp, "1.0.0") == "myapp:1.0.0"
    end

    test "handles string release name" do
      assert TestHelpers.get_tag([], "myapp", "2.0.0") == "myapp:2.0.0"
    end
  end

  describe "get_description/1" do
    test "returns description from config" do
      config = [description: "My awesome app"]
      assert TestHelpers.get_description(config) == "My awesome app"
    end

    test "returns :undefined when not set" do
      assert TestHelpers.get_description([]) == :undefined
    end

    test "converts atom description to binary" do
      config = [description: :my_description]
      assert TestHelpers.get_description(config) == "my_description"
    end
  end

  describe "get_push/1" do
    test "returns registry from config" do
      config = [push: "ghcr.io/myorg"]
      assert TestHelpers.get_push(config) == "ghcr.io/myorg"
    end

    test "returns nil when not set" do
      assert TestHelpers.get_push([]) == nil
    end
  end

  describe "get_platform/1" do
    test "returns platform string from config" do
      config = [platform: "linux/amd64"]
      assert TestHelpers.get_platform(config) == "linux/amd64"
    end

    test "converts charlist platform to binary" do
      config = [platform: ~c"linux/arm64"]
      assert TestHelpers.get_platform(config) == "linux/arm64"
    end

    test "returns nil when not set" do
      assert TestHelpers.get_platform([]) == nil
    end
  end

  describe "get_chunk_size/1" do
    test "returns nil when not set" do
      assert TestHelpers.get_chunk_size([]) == nil
    end

    test "converts MB to bytes" do
      config = [chunk_size: 5]
      assert TestHelpers.get_chunk_size(config) == 5 * 1024 * 1024
    end

    test "accepts boundary values" do
      assert TestHelpers.get_chunk_size([chunk_size: 1]) == 1 * 1024 * 1024
      assert TestHelpers.get_chunk_size([chunk_size: 100]) == 100 * 1024 * 1024
    end

    test "returns nil for invalid values" do
      assert TestHelpers.get_chunk_size([chunk_size: 0]) == nil
      assert TestHelpers.get_chunk_size([chunk_size: 101]) == nil
    end
  end

  describe "build_state/5" do
    test "builds state with defaults" do
      state = TestHelpers.build_state(:myapp, "1.0.0", "/path/to/release", "myapp", [])

      assert state.release_name == :myapp
      assert state.app_name == "myapp"
      assert state.release_path == ~c"/path/to/release"
      assert state.base_image == "debian:stable-slim"
      assert state.workdir == "/app"
      assert state.env == %{}
      assert state.expose == []
      assert state.labels == %{}
      assert state.cmd == "start"
      assert state.description == :undefined
      assert state.tag == "myapp:1.0.0"
      assert state.output == nil
      assert state.push == nil
      assert state.chunk_size == nil
      assert state.platform == nil
      assert state.app_version == "1.0.0"
      assert state.uid == nil
      assert state.vcs_annotations == true
    end

    test "builds state with custom config" do
      config = [
        base_image: "alpine:3.19",
        workdir: "/opt/app",
        env: %{"LANG" => "en_US.UTF-8", "PORT" => "4000"},
        expose: [4000, 4001],
        labels: %{"version" => "1.0"},
        cmd: "daemon",
        description: "Test application",
        tag: "custom:latest",
        push: "ghcr.io/test",
        chunk_size: 10,
        platform: "linux/arm64",
        uid: 1000,
        vcs_annotations: false
      ]

      state = TestHelpers.build_state(:myapp, "1.0.0", "/path", "myapp", config)

      assert state.base_image == "alpine:3.19"
      assert state.workdir == "/opt/app"
      assert state.env == %{"LANG" => "en_US.UTF-8", "PORT" => "4000"}
      assert state.expose == [4000, 4001]
      assert state.labels == %{"version" => "1.0"}
      assert state.cmd == "daemon"
      assert state.description == "Test application"
      assert state.tag == "custom:latest"
      assert state.push == "ghcr.io/test"
      assert state.chunk_size == 10 * 1024 * 1024
      assert state.platform == "linux/arm64"
      assert state.uid == 1000
      assert state.vcs_annotations == false
    end

    test "handles nil app_name" do
      state = TestHelpers.build_state(:myapp, "1.0.0", "/path", nil, [])
      assert state.app_name == nil
    end

    test "handles different release and app names" do
      # Common case: app: :indicator_sync, release: :server
      state = TestHelpers.build_state(:server, "1.0.0", "/path", "indicator_sync", [])
      assert state.release_name == :server
      assert state.app_name == "indicator_sync"
      assert state.tag == "server:1.0.0"
    end
  end

  describe "to_erlang_map/1" do
    test "converts Elixir map to Erlang-compatible format" do
      input = %{foo: "bar", baz: :qux}
      result = TestHelpers.to_erlang_map(input)
      assert result == %{"foo" => "bar", "baz" => "qux"}
    end

    test "handles already-binary keys" do
      input = %{"already" => "binary"}
      assert TestHelpers.to_erlang_map(input) == %{"already" => "binary"}
    end

    test "handles empty map" do
      assert TestHelpers.to_erlang_map(%{}) == %{}
    end
  end

  describe "to_binary/1" do
    test "handles various types" do
      assert TestHelpers.to_binary("string") == "string"
      assert TestHelpers.to_binary(:atom) == "atom"
      assert TestHelpers.to_binary(~c"charlist") == "charlist"
      assert TestHelpers.to_binary(42) == "42"
      assert TestHelpers.to_binary(3.14) == "3.14"
    end
  end
end
