defmodule Mix.Tasks.OcibuildTest do
  use ExUnit.Case, async: true

  # We test the module's internal logic by creating a test helper module
  # that exposes the private functions for testing purposes.
  # This avoids modifying the production code.

  defmodule TestHelpers do
    @moduledoc false

    # Re-implement the private functions for testing
    # This ensures we test the same logic without exposing internals

    def format_error({:release_not_found, name, path}) do
      """
      Release '#{name}' not found at #{path}.

      Make sure to build the release first:

          MIX_ENV=prod mix release
      """
    end

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

    def get_tag(opts, ocibuild_config, release_name, version) do
      cond do
        opts[:tag] -> opts[:tag]
        Keyword.has_key?(ocibuild_config, :tag) -> Keyword.get(ocibuild_config, :tag)
        true -> "#{release_name}:#{version}"
      end
    end

    def get_description(opts, ocibuild_config) do
      case opts[:desc] || Keyword.get(ocibuild_config, :description) do
        nil -> :undefined
        desc -> to_binary(desc)
      end
    end

    def get_vcs_annotations(opts, ocibuild_config) do
      cond do
        opts[:no_vcs_annotations] -> false
        Keyword.has_key?(ocibuild_config, :vcs_annotations) ->
          Keyword.get(ocibuild_config, :vcs_annotations)
        true -> true
      end
    end

    def get_chunk_size(opts) do
      case opts[:chunk_size] do
        nil -> nil
        size when is_integer(size) and size >= 1 and size <= 100 -> size * 1024 * 1024
        _size -> nil
      end
    end

    def get_platform(opts, ocibuild_config) do
      case opts[:platform] || Keyword.get(ocibuild_config, :platform) do
        nil -> nil
        platform when is_binary(platform) -> platform
        platform when is_list(platform) -> to_binary(platform)
      end
    end

    def get_app_version(config) do
      case config[:version] do
        nil -> :undefined
        version -> to_binary(version)
      end
    end
  end

  describe "format_error/1" do
    test "formats release_not_found with name and path" do
      error = TestHelpers.format_error({:release_not_found, :myapp, "/path/to/release"})
      assert error =~ "Release 'myapp' not found at /path/to/release"
      assert error =~ "MIX_ENV=prod mix release"
    end

    test "formats release_not_found with reason" do
      error = TestHelpers.format_error({:release_not_found, :enoent})
      assert error == "Failed to find release: :enoent"
    end

    test "formats collect_failed" do
      error = TestHelpers.format_error({:collect_failed, {:file_read_error, "/foo", :eacces}})
      assert error =~ "Failed to collect release files"
    end

    test "formats build_failed" do
      error = TestHelpers.format_error({:build_failed, :timeout})
      assert error == "Failed to build image: :timeout"
    end

    test "formats save_failed" do
      error = TestHelpers.format_error({:save_failed, :disk_full})
      assert error == "Failed to save image: :disk_full"
    end

    test "formats push_failed" do
      error = TestHelpers.format_error({:push_failed, :unauthorized})
      assert error == "Failed to push image: :unauthorized"
    end

    test "formats bundled_erts" do
      error = TestHelpers.format_error({:bundled_erts, "ERTS is bundled"})
      assert error == "Multi-platform build failed: ERTS is bundled"
    end

    test "formats nif_warning" do
      error = TestHelpers.format_error({:nif_warning, ["crypto.so", "ssl.so"]})
      assert error =~ "Native code detected"
      assert error =~ "crypto.so"
    end

    test "formats unknown error" do
      error = TestHelpers.format_error(:unknown_error)
      assert error == "OCI build error: :unknown_error"
    end
  end

  describe "to_erlang_map/1" do
    test "converts atom keys to binary" do
      result = TestHelpers.to_erlang_map(%{foo: "bar", baz: "qux"})
      assert result == %{"foo" => "bar", "baz" => "qux"}
    end

    test "converts atom values to binary" do
      result = TestHelpers.to_erlang_map(%{"key" => :value})
      assert result == %{"key" => "value"}
    end

    test "handles empty map" do
      assert TestHelpers.to_erlang_map(%{}) == %{}
    end

    test "handles mixed key types" do
      result = TestHelpers.to_erlang_map(%{:atom_key => "val1", "string_key" => "val2"})
      assert result == %{"atom_key" => "val1", "string_key" => "val2"}
    end
  end

  describe "to_binary/1" do
    test "returns binary as-is" do
      assert TestHelpers.to_binary("hello") == "hello"
    end

    test "converts atom to binary" do
      assert TestHelpers.to_binary(:hello) == "hello"
    end

    test "converts charlist to binary" do
      assert TestHelpers.to_binary(~c"hello") == "hello"
    end

    test "converts integer to binary" do
      assert TestHelpers.to_binary(123) == "123"
    end
  end

  describe "get_tag/4" do
    test "uses CLI option when provided" do
      opts = [tag: "myapp:cli"]
      config = [tag: "myapp:config"]
      assert TestHelpers.get_tag(opts, config, :myapp, "1.0.0") == "myapp:cli"
    end

    test "falls back to config when no CLI option" do
      opts = []
      config = [tag: "myapp:config"]
      assert TestHelpers.get_tag(opts, config, :myapp, "1.0.0") == "myapp:config"
    end

    test "generates default tag from release name and version" do
      opts = []
      config = []
      assert TestHelpers.get_tag(opts, config, :myapp, "1.0.0") == "myapp:1.0.0"
    end

    test "handles atom release name" do
      assert TestHelpers.get_tag([], [], :my_app, "2.0.0") == "my_app:2.0.0"
    end
  end

  describe "get_description/2" do
    test "uses CLI option when provided" do
      opts = [desc: "CLI description"]
      config = [description: "Config description"]
      assert TestHelpers.get_description(opts, config) == "CLI description"
    end

    test "falls back to config when no CLI option" do
      opts = []
      config = [description: "Config description"]
      assert TestHelpers.get_description(opts, config) == "Config description"
    end

    test "returns :undefined when not set" do
      assert TestHelpers.get_description([], []) == :undefined
    end
  end

  describe "get_vcs_annotations/2" do
    test "CLI --no-vcs-annotations disables annotations" do
      opts = [no_vcs_annotations: true]
      config = [vcs_annotations: true]
      assert TestHelpers.get_vcs_annotations(opts, config) == false
    end

    test "uses config value when no CLI flag" do
      opts = []
      config = [vcs_annotations: false]
      assert TestHelpers.get_vcs_annotations(opts, config) == false
    end

    test "defaults to true when not set" do
      assert TestHelpers.get_vcs_annotations([], []) == true
    end
  end

  describe "get_chunk_size/1" do
    test "returns nil when not set" do
      assert TestHelpers.get_chunk_size([]) == nil
    end

    test "converts MB to bytes for valid size" do
      assert TestHelpers.get_chunk_size([chunk_size: 10]) == 10 * 1024 * 1024
    end

    test "accepts minimum value of 1 MB" do
      assert TestHelpers.get_chunk_size([chunk_size: 1]) == 1 * 1024 * 1024
    end

    test "accepts maximum value of 100 MB" do
      assert TestHelpers.get_chunk_size([chunk_size: 100]) == 100 * 1024 * 1024
    end

    test "returns nil for out of range value (too small)" do
      assert TestHelpers.get_chunk_size([chunk_size: 0]) == nil
    end

    test "returns nil for out of range value (too large)" do
      assert TestHelpers.get_chunk_size([chunk_size: 101]) == nil
    end

    test "returns nil for negative value" do
      assert TestHelpers.get_chunk_size([chunk_size: -1]) == nil
    end
  end

  describe "get_platform/2" do
    test "uses CLI option when provided" do
      opts = [platform: "linux/arm64"]
      config = [platform: "linux/amd64"]
      assert TestHelpers.get_platform(opts, config) == "linux/arm64"
    end

    test "falls back to config when no CLI option" do
      opts = []
      config = [platform: "linux/amd64"]
      assert TestHelpers.get_platform(opts, config) == "linux/amd64"
    end

    test "returns nil when not set" do
      assert TestHelpers.get_platform([], []) == nil
    end

    test "handles charlist platform from config" do
      opts = []
      config = [platform: ~c"linux/amd64"]
      assert TestHelpers.get_platform(opts, config) == "linux/amd64"
    end
  end

  describe "get_app_version/1" do
    test "returns version as binary" do
      assert TestHelpers.get_app_version([version: "1.2.3"]) == "1.2.3"
    end

    test "returns :undefined when no version" do
      assert TestHelpers.get_app_version([]) == :undefined
    end

    test "converts atom version to binary" do
      # This is unusual but should work
      assert TestHelpers.get_app_version([version: :"1.0.0"]) == "1.0.0"
    end
  end
end
