defmodule Ocibuild.MixReleaseTest do
  use ExUnit.Case, async: true

  # Use shared test helpers
  alias Ocibuild.TestHelpers

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

  describe "get_tag_from_config/3" do
    test "uses config tag when provided" do
      config = [tag: "custom:tag"]
      assert TestHelpers.get_tag_from_config(config, :myapp, "1.0.0") == "custom:tag"
    end

    test "generates default tag from release name and version" do
      assert TestHelpers.get_tag_from_config([], :myapp, "1.0.0") == "myapp:1.0.0"
    end

    test "handles string release name" do
      assert TestHelpers.get_tag_from_config([], "myapp", "2.0.0") == "myapp:2.0.0"
    end
  end

  describe "get_description_from_config/1" do
    test "returns description from config" do
      config = [description: "My awesome app"]
      assert TestHelpers.get_description_from_config(config) == "My awesome app"
    end

    test "returns :undefined when not set" do
      assert TestHelpers.get_description_from_config([]) == :undefined
    end

    test "converts atom description to binary" do
      config = [description: :my_description]
      assert TestHelpers.get_description_from_config(config) == "my_description"
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

  describe "get_platform_from_config/1" do
    test "returns platform string from config" do
      config = [platform: "linux/amd64"]
      assert TestHelpers.get_platform_from_config(config) == "linux/amd64"
    end

    test "converts charlist platform to binary" do
      config = [platform: ~c"linux/arm64"]
      assert TestHelpers.get_platform_from_config(config) == "linux/arm64"
    end

    test "returns nil when not set" do
      assert TestHelpers.get_platform_from_config([]) == nil
    end
  end

  describe "get_chunk_size_from_config/1" do
    test "returns nil when not set" do
      assert TestHelpers.get_chunk_size_from_config([]) == nil
    end

    test "converts MB to bytes" do
      config = [chunk_size: 5]
      assert TestHelpers.get_chunk_size_from_config(config) == 5 * 1024 * 1024
    end

    test "accepts boundary values" do
      assert TestHelpers.get_chunk_size_from_config(chunk_size: 1) == 1 * 1024 * 1024
      assert TestHelpers.get_chunk_size_from_config(chunk_size: 100) == 100 * 1024 * 1024
    end

    test "returns nil for invalid values" do
      assert TestHelpers.get_chunk_size_from_config(chunk_size: 0) == nil
      assert TestHelpers.get_chunk_size_from_config(chunk_size: 101) == nil
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
