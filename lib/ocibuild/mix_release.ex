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
            base_image: "debian:slim",
            registry: "ghcr.io/myorg",
            # tag: "myapp:1.0.0",  # Optional, defaults to release_name:version
            workdir: "/app",
            env: %{"LANG" => "C.UTF-8"},
            expose: [8080]
          ]
        ]
      end

  Then run:

      MIX_ENV=prod mix release

  The OCI image will be built automatically after the release is assembled.

  ## Configuration Options

    * `:base_image` - Base image (default: "debian:slim")
    * `:tag` - Image tag (default: release_name:release_version)
    * `:registry` - Registry for push
    * `:workdir` - Working directory in container (default: "/app")
    * `:env` - Environment variables map
    * `:expose` - Ports to expose
    * `:labels` - Image labels map
    * `:push` - Push to registry after build (default: false)
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

    # Get configuration
    base_image = Keyword.get(ocibuild_config, :base_image, "debian:slim")
    workdir = Keyword.get(ocibuild_config, :workdir, "/app")
    env_map = Keyword.get(ocibuild_config, :env, %{}) |> to_erlang_map()
    expose_ports = Keyword.get(ocibuild_config, :expose, [])
    labels = Keyword.get(ocibuild_config, :labels, %{}) |> to_erlang_map()
    should_push = Keyword.get(ocibuild_config, :push, false)
    # Elixir releases use "start" command (Erlang uses "foreground")
    cmd = Keyword.get(ocibuild_config, :cmd, "start")

    # Get or generate tag
    tag = get_tag(ocibuild_config, release.name, release.version)

    Mix.shell().info("Building OCI image: #{tag}")
    Mix.shell().info("  Release path: #{release.path}")
    Mix.shell().info("  Base image: #{base_image}")

    # Collect release files
    case :ocibuild_rebar3.collect_release_files(to_charlist(release.path)) do
      {:ok, files} ->
        Mix.shell().info("  Collected #{length(files)} files")

        # Build image with Elixir-appropriate start command
        case :ocibuild_rebar3.build_image(
               to_binary(base_image),
               files,
               to_charlist(release.name),
               to_binary(workdir),
               env_map,
               expose_ports,
               labels,
               to_binary(cmd)
             ) do
          {:ok, image} ->
            output_image(image, tag, ocibuild_config, should_push)
            release

          {:error, reason} ->
            Mix.raise("Failed to build OCI image: #{inspect(reason)}")
        end

      {:error, reason} ->
        Mix.raise("Failed to collect release files: #{inspect(reason)}")
    end
  end

  defp get_tag(ocibuild_config, release_name, version) do
    case Keyword.get(ocibuild_config, :tag) do
      nil -> "#{release_name}:#{version}"
      tag -> tag
    end
  end

  defp output_image(image, tag, ocibuild_config, should_push) do
    # Generate output path
    safe_tag = String.replace(tag, ":", "-")
    output_path = "#{safe_tag}.tar.gz"

    Mix.shell().info("  Saving to #{output_path}")

    case :ocibuild.save(image, to_charlist(output_path)) do
      :ok ->
        Mix.shell().info("OCI image saved successfully")

        if should_push do
          push_image(image, tag, ocibuild_config)
        else
          Mix.shell().info("To load: docker load < #{output_path}")
        end

      {:error, reason} ->
        Mix.raise("Failed to save OCI image: #{inspect(reason)}")
    end
  end

  defp push_image(image, tag, ocibuild_config) do
    registry = Keyword.get(ocibuild_config, :registry, "docker.io")
    {repo, image_tag} = parse_tag(tag)
    auth = :ocibuild_rebar3.get_push_auth()

    Mix.shell().info("  Pushing to #{registry}/#{repo}:#{image_tag}")

    repo_tag = "#{repo}:#{image_tag}"

    case :ocibuild.push(image, to_binary(registry), to_binary(repo_tag), auth) do
      :ok ->
        Mix.shell().info("Push successful!")

      {:error, reason} ->
        Mix.raise("Failed to push OCI image: #{inspect(reason)}")
    end
  end

  defp parse_tag(tag) do
    case String.split(tag, ":") do
      [repo, image_tag] -> {repo, image_tag}
      [repo] -> {repo, "latest"}
    end
  end

  # Convert Elixir map to Erlang-compatible map with binary keys
  defp to_erlang_map(map) when is_map(map) do
    Map.new(map, fn {k, v} -> {to_binary(k), to_binary(v)} end)
  end

  defp to_binary(value) when is_binary(value), do: value
  defp to_binary(value) when is_atom(value), do: Atom.to_string(value)
  defp to_binary(value) when is_list(value), do: to_string(value)
  defp to_binary(value), do: to_string(value)
end
