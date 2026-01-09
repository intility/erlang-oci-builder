ExUnit.start()

defmodule Ocibuild.TestHelpers do
  @moduledoc """
  Shared test helper functions for ocibuild Elixir tests.

  These helpers replicate private function logic from the production modules
  for testing purposes, avoiding the need to expose internals.
  """

  # ============================================================================
  # Error Formatting
  # ============================================================================

  @doc """
  Format error tuples into human-readable messages.

  Replicates the format_error/1 logic from Mix.Tasks.Ocibuild and Ocibuild.MixRelease.
  """
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

  # ============================================================================
  # Type Conversion
  # ============================================================================

  @doc """
  Convert Elixir map to Erlang-compatible format with binary keys and values.
  """
  def to_erlang_map(map) when is_map(map) do
    Map.new(map, fn {k, v} -> {to_binary(k), to_binary(v)} end)
  end

  @doc """
  Convert various types to binary string.
  """
  def to_binary(value) when is_binary(value), do: value
  def to_binary(value) when is_atom(value), do: Atom.to_string(value)
  def to_binary(value) when is_list(value), do: to_string(value)
  def to_binary(value), do: to_string(value)

  # ============================================================================
  # Mix Task Config Helpers (CLI opts + config)
  # ============================================================================

  @doc """
  Get tag from CLI options or config, with fallback to release_name:version.

  Used by Mix.Tasks.Ocibuild where CLI options take precedence.
  """
  def get_tag(opts, ocibuild_config, release_name, version) do
    cond do
      opts[:tag] -> opts[:tag]
      Keyword.has_key?(ocibuild_config, :tag) -> Keyword.get(ocibuild_config, :tag)
      true -> "#{release_name}:#{version}"
    end
  end

  @doc """
  Get tags from CLI options or config, with semicolon expansion support.

  Supports docker/metadata-action style semicolon-separated tags.
  Used by Mix.Tasks.Ocibuild where CLI options take precedence.
  """
  def get_tags(opts, ocibuild_config, release_name, version) do
    # Collect all tag values from CLI, expanding semicolon-separated values
    cli_tags =
      Keyword.get_values(opts, :tag)
      |> Enum.flat_map(&String.split(&1, ";"))
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&to_binary/1)

    cond do
      # CLI tags take precedence
      cli_tags != [] ->
        cli_tags

      # Check config for tag (may be string or list)
      Keyword.has_key?(ocibuild_config, :tag) ->
        case Keyword.get(ocibuild_config, :tag) do
          # List of tags (but not a charlist - charlists are lists of integers)
          tags when is_list(tags) and (tags == [] or not is_integer(hd(tags))) ->
            Enum.map(tags, &to_binary/1)

          # Single tag (binary, charlist, or other)
          tag ->
            [to_binary(tag)]
        end

      # Default to release_name:version
      true ->
        [to_binary("#{release_name}:#{version}")]
    end
  end

  @doc """
  Get description from CLI options or config.

  Used by Mix.Tasks.Ocibuild where CLI options take precedence.
  """
  def get_description(opts, ocibuild_config) do
    case opts[:desc] || Keyword.get(ocibuild_config, :description) do
      nil -> :undefined
      desc -> to_binary(desc)
    end
  end

  @doc """
  Get VCS annotations setting from CLI options or config.

  CLI --no-vcs-annotations takes precedence, then config, then defaults to true.
  """
  def get_vcs_annotations(opts, ocibuild_config) do
    cond do
      opts[:no_vcs_annotations] ->
        false

      Keyword.has_key?(ocibuild_config, :vcs_annotations) ->
        Keyword.get(ocibuild_config, :vcs_annotations)

      true ->
        true
    end
  end

  @doc """
  Get platform from CLI options or config.

  Used by Mix.Tasks.Ocibuild where CLI options take precedence.
  """
  def get_platform(opts, ocibuild_config) do
    case opts[:platform] || Keyword.get(ocibuild_config, :platform) do
      nil -> nil
      platform when is_binary(platform) -> platform
      platform when is_list(platform) -> to_binary(platform)
    end
  end

  @doc """
  Get chunk size from CLI options, converting MB to bytes.

  Valid range: 1-100 MB. Returns nil for invalid or unset values.
  """
  def get_chunk_size(opts) do
    case opts[:chunk_size] do
      nil -> nil
      size when is_integer(size) and size >= 1 and size <= 100 -> size * 1024 * 1024
      _size -> nil
    end
  end

  @doc """
  Get app version from config.
  """
  def get_app_version(config) do
    case config[:version] do
      nil -> :undefined
      version -> to_binary(version)
    end
  end

  # ============================================================================
  # Mix Release Config Helpers (config only, no CLI)
  # ============================================================================

  @doc """
  Get tag from config only, with fallback to release_name:version.

  Used by Ocibuild.MixRelease where there are no CLI options.
  """
  def get_tag_from_config(ocibuild_config, release_name, version) do
    case Keyword.get(ocibuild_config, :tag) do
      nil -> "#{release_name}:#{version}"
      tag -> tag
    end
  end

  @doc """
  Get description from config only.

  Used by Ocibuild.MixRelease where there are no CLI options.
  """
  def get_description_from_config(ocibuild_config) do
    case Keyword.get(ocibuild_config, :description) do
      nil -> :undefined
      desc -> to_binary(desc)
    end
  end

  @doc """
  Get push registry from config.
  """
  def get_push(ocibuild_config) do
    case Keyword.get(ocibuild_config, :push) do
      nil -> nil
      registry -> to_binary(registry)
    end
  end

  @doc """
  Get platform from config only.

  Used by Ocibuild.MixRelease where there are no CLI options.
  """
  def get_platform_from_config(ocibuild_config) do
    case Keyword.get(ocibuild_config, :platform) do
      nil -> nil
      platform when is_binary(platform) -> platform
      platform when is_list(platform) -> to_binary(platform)
    end
  end

  @doc """
  Get chunk size from config, converting MB to bytes.

  Valid range: 1-100 MB. Returns nil for invalid or unset values.
  """
  def get_chunk_size_from_config(ocibuild_config) do
    case Keyword.get(ocibuild_config, :chunk_size) do
      nil -> nil
      size when is_integer(size) and size >= 1 and size <= 100 -> size * 1024 * 1024
      _size -> nil
    end
  end

  # ============================================================================
  # State Building
  # ============================================================================

  @doc """
  Build a state map simulating what Ocibuild.MixRelease.build_state/2 produces.

  Used for testing state construction logic without needing a real Mix.Release struct.
  """
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
      description: get_description_from_config(ocibuild_config),
      tag: get_tag_from_config(ocibuild_config, release_name, release_version) |> to_binary(),
      output: nil,
      push: get_push(ocibuild_config),
      chunk_size: get_chunk_size_from_config(ocibuild_config),
      platform: get_platform_from_config(ocibuild_config),
      app_version: to_binary(release_version),
      uid: Keyword.get(ocibuild_config, :uid),
      vcs_annotations: Keyword.get(ocibuild_config, :vcs_annotations, true)
    }
  end

  # ============================================================================
  # Temp Directory Helpers
  # ============================================================================

  @doc """
  Get the system temp directory.
  """
  def temp_dir do
    case :os.type() do
      {:win32, _} ->
        System.get_env("TEMP") || System.get_env("TMP") || "C:\\Temp"

      _ ->
        "/tmp"
    end
  end

  @doc """
  Create a unique temporary directory with the given prefix.
  """
  def make_temp_dir(prefix) do
    unique = :erlang.unique_integer([:positive])
    dir_name = "ocibuild_test_#{prefix}_#{unique}"
    tmp_dir = Path.join(temp_dir(), dir_name)
    File.mkdir_p!(tmp_dir)
    tmp_dir
  end

  @doc """
  Clean up a temporary directory.
  """
  def cleanup_temp_dir(dir) do
    File.rm_rf!(dir)
  end
end
