defmodule Mix.Tasks.Ocibuild do
  @shortdoc "Build OCI container image from Mix release"
  @moduledoc """
  Builds an OCI container image from a Mix release.

  ## Usage

      MIX_ENV=prod mix release
      MIX_ENV=prod mix ocibuild -t myapp:1.0.0

  Or push directly to a registry:

      MIX_ENV=prod mix ocibuild -t myapp:1.0.0 --push ghcr.io/myorg

  ## Options

    * `-t, --tag` - Image tag (e.g., myapp:1.0.0). Defaults to release_name:version
    * `-o, --output` - Output tarball path (default: <tag>.tar.gz)
    * `-c, --cmd` - Release start command (default: "start"). Use "daemon" for background
    * `-d, --desc` - Image description (OCI manifest annotation)
    * `-p, --push` - Push to registry (e.g., ghcr.io/myorg)
    * `-P, --platform` - Target platforms (e.g., linux/amd64 or linux/amd64,linux/arm64)
    * `--base` - Override base image
    * `--release` - Release name (if multiple configured)
    * `--chunk-size` - Chunk size in MB for uploads (default: 5)
    * `--uid` - User ID to run as (default: 65534 for nobody)
    * `--no-vcs-annotations` - Disable automatic VCS annotations

  ## Configuration

  Add to your `mix.exs`:

      def project do
        [
          # ...
          ocibuild: [
            base_image: "debian:stable-slim",
            workdir: "/app",
            env: %{"LANG" => "C.UTF-8"},
            expose: [8080],
            labels: %{},
            cmd: "start",  # Release command: "start" (Elixir default), "daemon", etc.
            description: "My awesome application"  # OCI manifest annotation
          ]
        ]
      end

  ## Authentication

  Set environment variables for registry authentication:

  For pushing to registry:

      export OCIBUILD_PUSH_USERNAME="user"
      export OCIBUILD_PUSH_PASSWORD="pass"

  For pulling private base images (optional, anonymous pull used if unset):

      export OCIBUILD_PULL_USERNAME="user"
      export OCIBUILD_PULL_PASSWORD="pass"
  """

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    {opts, _remaining, _invalid} =
      OptionParser.parse(args,
        aliases: [t: :tag, p: :push, o: :output, c: :cmd, d: :desc, P: :platform],
        switches: [
          tag: :string,
          output: :string,
          push: :string,
          base: :string,
          release: :string,
          cmd: :string,
          desc: :string,
          chunk_size: :integer,
          platform: :string,
          uid: :integer,
          no_vcs_annotations: :boolean
        ]
      )

    # Ensure the project is compiled
    Mix.Task.run("compile", [])

    config = Mix.Project.config()
    ocibuild_config = config[:ocibuild] || []

    # Find release
    case find_release(config, opts) do
      {:ok, release_name, release_path} ->
        # Build state map for the adapter
        state = build_state(release_name, release_path, opts, ocibuild_config, config)

        # Delegate to ocibuild_release:run/3
        case :ocibuild_release.run(:ocibuild_mix, state, %{}) do
          {:ok, _state} ->
            :ok

          {:error, reason} ->
            Mix.raise(format_error(reason))
        end

      {:error, reason} ->
        Mix.raise(format_error(reason))
    end
  end

  defp find_release(config, opts) do
    release_name = get_release_name(config, opts)
    build_path = Mix.Project.build_path(config)
    release_path = Path.join([build_path, "rel", to_string(release_name)])

    if File.dir?(release_path) do
      {:ok, release_name, release_path}
    else
      {:error, {:release_not_found, release_name, release_path}}
    end
  end

  defp get_release_name(config, opts) do
    cond do
      opts[:release] ->
        String.to_atom(opts[:release])

      config[:releases] && config[:releases] != [] ->
        [{name, _opts} | _] = config[:releases]
        name

      true ->
        config[:app]
    end
  end

  defp build_state(release_name, release_path, opts, ocibuild_config, config) do
    %{
      # Release info
      release_name: release_name,
      release_path: to_charlist(release_path),
      # Configuration with CLI overrides
      base_image:
        get_opt(opts, :base, ocibuild_config, :base_image, "debian:stable-slim") |> to_binary(),
      workdir: Keyword.get(ocibuild_config, :workdir, "/app") |> to_binary(),
      env: Keyword.get(ocibuild_config, :env, %{}) |> to_erlang_map(),
      expose: Keyword.get(ocibuild_config, :expose, []),
      labels: Keyword.get(ocibuild_config, :labels, %{}) |> to_erlang_map(),
      cmd: get_opt(opts, :cmd, ocibuild_config, :cmd, "start") |> to_binary(),
      description: get_description(opts, ocibuild_config),
      tag: get_tag(opts, ocibuild_config, release_name, config[:version]) |> to_binary(),
      output: get_opt_binary(opts, :output),
      push: get_opt_binary(opts, :push),
      chunk_size: get_chunk_size(opts),
      platform: get_platform(opts, ocibuild_config),
      uid: opts[:uid] || Keyword.get(ocibuild_config, :uid),
      app_version: get_app_version(config),
      vcs_annotations: get_vcs_annotations(opts, ocibuild_config)
    }
  end

  defp get_chunk_size(opts) do
    case opts[:chunk_size] do
      nil ->
        nil

      size when is_integer(size) and size >= 1 and size <= 100 ->
        size * 1024 * 1024

      size ->
        IO.warn("--chunk-size #{size} MB out of range (1-100), using default")
        nil
    end
  end

  defp get_platform(opts, ocibuild_config) do
    case opts[:platform] || Keyword.get(ocibuild_config, :platform) do
      nil -> nil
      platform when is_binary(platform) -> platform
      platform when is_list(platform) -> to_binary(platform)
    end
  end

  # Get application version from Mix project config
  defp get_app_version(config) do
    case config[:version] do
      nil -> :undefined
      version -> to_binary(version)
    end
  end

  # Get VCS annotations setting: CLI flag takes precedence, then config, then default true
  defp get_vcs_annotations(opts, ocibuild_config) do
    cond do
      # CLI --no-vcs-annotations disables VCS annotations
      opts[:no_vcs_annotations] -> false
      # Check config for explicit setting
      Keyword.has_key?(ocibuild_config, :vcs_annotations) ->
        Keyword.get(ocibuild_config, :vcs_annotations)
      # Default to enabled
      true -> true
    end
  end

  defp get_opt(opts, opt_key, config, config_key, default) do
    opts[opt_key] || Keyword.get(config, config_key, default)
  end

  defp get_opt_binary(opts, key) do
    case opts[key] do
      nil -> nil
      val -> to_binary(val)
    end
  end

  defp get_description(opts, ocibuild_config) do
    case opts[:desc] || Keyword.get(ocibuild_config, :description) do
      nil -> :undefined
      desc -> to_binary(desc)
    end
  end

  defp get_tag(opts, ocibuild_config, release_name, version) do
    cond do
      opts[:tag] -> opts[:tag]
      Keyword.has_key?(ocibuild_config, :tag) -> Keyword.get(ocibuild_config, :tag)
      true -> "#{release_name}:#{version}"
    end
  end

  defp format_error({:release_not_found, name, path}) do
    """
    Release '#{name}' not found at #{path}.

    Make sure to build the release first:

        MIX_ENV=prod mix release
    """
  end

  defp format_error({:release_not_found, reason}),
    do: "Failed to find release: #{inspect(reason)}"

  defp format_error({:collect_failed, reason}),
    do: "Failed to collect release files: #{inspect(reason)}"

  defp format_error({:build_failed, reason}), do: "Failed to build image: #{inspect(reason)}"
  defp format_error({:save_failed, reason}), do: "Failed to save image: #{inspect(reason)}"
  defp format_error({:push_failed, reason}), do: "Failed to push image: #{inspect(reason)}"

  defp format_error({:bundled_erts, message}),
    do: "Multi-platform build failed: #{message}"

  defp format_error({:nif_warning, files}),
    do: "Warning: Native code detected that may not be portable: #{inspect(files)}"

  defp format_error(reason), do: "OCI build error: #{inspect(reason)}"

  # Convert Elixir map to Erlang-compatible map with binary keys
  defp to_erlang_map(map) when is_map(map) do
    Map.new(map, fn {k, v} -> {to_binary(k), to_binary(v)} end)
  end

  defp to_binary(value) when is_binary(value), do: value
  defp to_binary(value) when is_atom(value), do: Atom.to_string(value)
  defp to_binary(value) when is_list(value), do: to_string(value)
  defp to_binary(value), do: to_string(value)
end
