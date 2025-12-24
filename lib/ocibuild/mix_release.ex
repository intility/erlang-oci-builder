defmodule Ocibuild.MixRelease do
  @moduledoc """
  Release step functions for building OCI images during `mix release`.

  ## Usage

  Add the step to your release configuration in `mix.exs`:

      def project do
        [
          app: :myapp,
          version: "1.0.0",
          releases: [
            myapp: [
              steps: [:assemble, &Ocibuild.MixRelease.build_image/1]
            ]
          ],
          ocibuild: [
            base_image: "debian:stable-slim",
            push: "ghcr.io/myorg",  # Registry to push to (omit to skip push)
            tag: "myapp:1.0.0",     # Optional, defaults to release_name:version
            workdir: "/app",
            env: %{"LANG" => "C.UTF-8"},
            expose: [8080],
            description: "My awesome application"
          ]
        ]
      end

  Then run:

      MIX_ENV=prod mix release

  The OCI image will be built automatically after the release is assembled.

  ## Configuration Options

    * `:base_image` - Base image (default: "debian:stable-slim")
    * `:tag` - Image tag (default: release_name:release_version)
    * `:push` - Registry to push to (e.g., "ghcr.io/myorg"). Omit to skip push.
    * `:workdir` - Working directory in container (default: "/app")
    * `:env` - Environment variables map
    * `:expose` - Ports to expose
    * `:labels` - Image labels map
    * `:cmd` - Release start command (default: "start")
    * `:description` - Image description (OCI manifest annotation)
    * `:chunk_size` - Chunk size in MB for uploads (default: 5)
    * `:platform` - Target platforms. Single string like "linux/amd64" or
      comma-separated string like "linux/amd64,linux/arm64" for multi-platform builds.
    * `:uid` - User ID to run as (default: 65534 for nobody)
    * `:vcs_annotations` - Enable automatic VCS annotations (default: true)
  """

  @doc """
  Build an OCI image from an assembled release.

  This function is designed to be used as a release step:

      steps: [:assemble, &Ocibuild.MixRelease.build_image/1]

  It receives a `Mix.Release` struct and must return it unchanged.
  """
  @spec build_image(Mix.Release.t()) :: Mix.Release.t()
  def build_image(%Mix.Release{} = release) do
    config = Mix.Project.config()
    ocibuild_config = config[:ocibuild] || []

    # Build state map for the adapter
    state = build_state(release, ocibuild_config)

    # Delegate to ocibuild_release:run/3
    case :ocibuild_release.run(:ocibuild_mix, state, %{}) do
      {:ok, _state} ->
        release

      {:error, reason} ->
        Mix.raise(format_error(reason))
    end
  end

  defp build_state(release, ocibuild_config) do
    %{
      # Release info
      release_name: release.name,
      release_path: to_charlist(release.path),
      # Configuration
      base_image: Keyword.get(ocibuild_config, :base_image, "debian:stable-slim") |> to_binary(),
      workdir: Keyword.get(ocibuild_config, :workdir, "/app") |> to_binary(),
      env: Keyword.get(ocibuild_config, :env, %{}) |> to_erlang_map(),
      expose: Keyword.get(ocibuild_config, :expose, []),
      labels: Keyword.get(ocibuild_config, :labels, %{}) |> to_erlang_map(),
      cmd: Keyword.get(ocibuild_config, :cmd, "start") |> to_binary(),
      description: get_description(ocibuild_config),
      tag: get_tag(ocibuild_config, release.name, release.version) |> to_binary(),
      output: nil,
      push: get_push(ocibuild_config),
      chunk_size: get_chunk_size(ocibuild_config),
      platform: get_platform(ocibuild_config),
      app_version: to_binary(release.version),
      uid: Keyword.get(ocibuild_config, :uid),
      vcs_annotations: Keyword.get(ocibuild_config, :vcs_annotations, true)
    }
  end

  defp get_chunk_size(ocibuild_config) do
    case Keyword.get(ocibuild_config, :chunk_size) do
      nil ->
        nil

      size when is_integer(size) and size >= 1 and size <= 100 ->
        size * 1024 * 1024

      size ->
        IO.warn("chunk_size #{size} MB out of range (1-100), using default")
        nil
    end
  end

  # Returns :undefined (atom) or binary for Erlang interop with ocibuild_release
  defp get_description(ocibuild_config) do
    case Keyword.get(ocibuild_config, :description) do
      nil -> :undefined
      desc -> to_binary(desc)
    end
  end

  defp get_tag(ocibuild_config, release_name, version) do
    case Keyword.get(ocibuild_config, :tag) do
      nil -> "#{release_name}:#{version}"
      tag -> tag
    end
  end

  defp get_push(ocibuild_config) do
    case Keyword.get(ocibuild_config, :push) do
      nil -> nil
      registry -> to_binary(registry)
    end
  end

  defp get_platform(ocibuild_config) do
    case Keyword.get(ocibuild_config, :platform) do
      nil -> nil
      platform when is_binary(platform) -> platform
      platform when is_list(platform) -> to_binary(platform)
    end
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
