defmodule Mix.Tasks.Ocibuild do
  @shortdoc "Build OCI container image from Mix release"
  @moduledoc """
  Builds an OCI container image from a Mix release.

  ## Usage

      MIX_ENV=prod mix release
      MIX_ENV=prod mix ocibuild -t myapp:1.0.0

  Or push directly to a registry:

      MIX_ENV=prod mix ocibuild -t myapp:1.0.0 --push -r ghcr.io/myorg

  ## Options

    * `-t, --tag` - Image tag (e.g., myapp:1.0.0). Defaults to release_name:version
    * `-r, --registry` - Registry for push (e.g., ghcr.io)
    * `-o, --output` - Output tarball path (default: <tag>.tar.gz)
    * `--push` - Push to registry after build
    * `--base` - Override base image
    * `--release` - Release name (if multiple configured)

  ## Configuration

  Add to your `mix.exs`:

      def project do
        [
          # ...
          ocibuild: [
            base_image: "debian:slim",
            registry: "ghcr.io/myorg",
            workdir: "/app",
            env: %{"LANG" => "C.UTF-8"},
            expose: [8080],
            labels: %{}
          ]
        ]
      end

  ## Authentication

  Set environment variables for registry authentication:

      export OCIBUILD_TOKEN="your-token"

  Or:

      export OCIBUILD_USERNAME="user"
      export OCIBUILD_PASSWORD="pass"
  """

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    {opts, _remaining, _invalid} =
      OptionParser.parse(args,
        aliases: [t: :tag, r: :registry, o: :output],
        switches: [
          tag: :string,
          registry: :string,
          output: :string,
          push: :boolean,
          base: :string,
          release: :string
        ]
      )

    # Ensure the project is compiled
    Mix.Task.run("compile", [])

    config = Mix.Project.config()
    ocibuild_config = config[:ocibuild] || []

    # Find release
    case find_release(config, opts) do
      {:ok, release_name, release_path} ->
        Mix.shell().info("Using release: #{release_name} at #{release_path}")
        build_and_output(release_name, release_path, opts, ocibuild_config, config)

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

  defp build_and_output(release_name, release_path, opts, ocibuild_config, config) do
    # Get configuration with CLI overrides
    base_image = get_base_image(opts, ocibuild_config)
    workdir = Keyword.get(ocibuild_config, :workdir, "/app")
    env_map = Keyword.get(ocibuild_config, :env, %{}) |> to_erlang_map()
    expose_ports = Keyword.get(ocibuild_config, :expose, [])
    labels = Keyword.get(ocibuild_config, :labels, %{}) |> to_erlang_map()

    # Get or generate tag
    tag = get_tag(opts, ocibuild_config, release_name, config[:version])

    Mix.shell().info("Building OCI image: #{tag}")
    Mix.shell().info("  Base image: #{base_image}")

    # Collect release files using Erlang function
    case :ocibuild_rebar3.collect_release_files(to_charlist(release_path)) do
      {:ok, files} ->
        Mix.shell().info("  Collected #{length(files)} files from release")

        # Build image
        case :ocibuild_rebar3.build_image(
               to_binary(base_image),
               files,
               to_charlist(release_name),
               to_binary(workdir),
               env_map,
               expose_ports,
               labels
             ) do
          {:ok, image} ->
            output_image(image, tag, opts, ocibuild_config)

          {:error, reason} ->
            Mix.raise("Failed to build image: #{inspect(reason)}")
        end

      {:error, reason} ->
        Mix.raise("Failed to collect release files: #{inspect(reason)}")
    end
  end

  defp get_base_image(opts, ocibuild_config) do
    cond do
      opts[:base] -> opts[:base]
      Keyword.has_key?(ocibuild_config, :base_image) -> Keyword.get(ocibuild_config, :base_image)
      true -> "debian:slim"
    end
  end

  defp get_tag(opts, ocibuild_config, release_name, version) do
    cond do
      opts[:tag] ->
        opts[:tag]

      Keyword.has_key?(ocibuild_config, :tag) ->
        Keyword.get(ocibuild_config, :tag)

      true ->
        # Auto-generate from release name and version
        "#{release_name}:#{version}"
    end
  end

  defp output_image(image, tag, opts, ocibuild_config) do
    should_push = opts[:push] || false

    # Determine output path
    output_path =
      case opts[:output] do
        nil ->
          safe_tag = String.replace(tag, ":", "-")
          "#{safe_tag}.tar.gz"

        path ->
          path
      end

    # Save tarball
    Mix.shell().info("  Saving to #{output_path}")

    case :ocibuild.save(image, to_charlist(output_path)) do
      :ok ->
        Mix.shell().info("Image saved successfully")

        if should_push do
          push_image(image, tag, opts, ocibuild_config)
        else
          Mix.shell().info("\nTo load the image:\n  docker load < #{output_path}")
          :ok
        end

      {:error, reason} ->
        Mix.raise("Failed to save image: #{inspect(reason)}")
    end
  end

  defp push_image(image, tag, opts, ocibuild_config) do
    registry =
      cond do
        opts[:registry] -> opts[:registry]
        Keyword.has_key?(ocibuild_config, :registry) -> Keyword.get(ocibuild_config, :registry)
        true -> "docker.io"
      end

    {repo, image_tag} = parse_tag(tag)
    auth = :ocibuild_rebar3.get_auth()

    Mix.shell().info("  Pushing to #{registry}/#{repo}:#{image_tag}")

    repo_tag = "#{repo}:#{image_tag}"

    case :ocibuild.push(image, to_binary(registry), to_binary(repo_tag), auth) do
      :ok ->
        Mix.shell().info("Push successful!")
        :ok

      {:error, reason} ->
        Mix.raise("Failed to push image: #{inspect(reason)}")
    end
  end

  defp parse_tag(tag) do
    case String.split(tag, ":") do
      [repo, image_tag] -> {repo, image_tag}
      [repo] -> {repo, "latest"}
    end
  end

  defp format_error({:release_not_found, name, path}) do
    """
    Release '#{name}' not found at #{path}.

    Make sure to build the release first:

        MIX_ENV=prod mix release
    """
  end

  defp format_error(reason), do: inspect(reason)

  # Convert Elixir map to Erlang-compatible map with binary keys
  defp to_erlang_map(map) when is_map(map) do
    Map.new(map, fn {k, v} -> {to_binary(k), to_binary(v)} end)
  end

  defp to_binary(value) when is_binary(value), do: value
  defp to_binary(value) when is_atom(value), do: Atom.to_string(value)
  defp to_binary(value) when is_list(value), do: to_string(value)
  defp to_binary(value), do: to_string(value)
end
