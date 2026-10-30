defmodule Ocibuild.Lock do
  @moduledoc """
  Shared module for parsing Mix lock files.

  This module provides dependency extraction from mix.lock files,
  used by both the Mix task and Mix release step for smart dependency layering.

  Uses `Mix.Dep.Lock.read/1` instead of `Code.eval_string` for safer lockfile parsing.
  """

  @doc """
  Get dependencies from mix.lock for smart layer classification.

  Returns a list of dependency maps with `:name`, `:version`, and `:source` keys.
  Hex packages also carry `:deps` (names of the packages they require), and every
  package carries `:direct` when the project's own deps are known (see
  `get_dependencies/2`). Returns an empty list if mix.lock doesn't exist or can't be parsed.

  ## Examples

      iex> Ocibuild.Lock.get_dependencies()
      [
        %{name: "plug", version: "1.14.0", source: "hex", deps: ["mime"], direct: true},
        %{name: "mime", version: "2.0.5", source: "hex", deps: [], direct: false}
      ]
  """
  @spec get_dependencies() :: [dependency()]
  def get_dependencies do
    get_dependencies("mix.lock", project_direct_names(), Mix.Project.deps_path())
  end

  @doc """
  Get dependencies from a specific lock file path.

  This variant is useful for testing or when the lock file is not in the current directory.
  """
  @spec get_dependencies(Path.t()) :: [dependency()]
  def get_dependencies(lock_path) do
    get_dependencies(lock_path, nil)
  end

  @doc """
  Get dependencies from a specific lock file path, marking which ones are direct.

  `direct_names` are the package names declared by the project (`nil` when unknown, in
  which case the `:direct` key is left out). Mirrors the optional keys of
  `ocibuild_adapter:dependency()`.

  When `deps_path` is given, the license names are read from each Hex package's
  `hex_metadata.config` in that directory (no network involved).
  """
  @spec get_dependencies(Path.t(), [String.t()] | nil, Path.t() | nil) :: [dependency()]
  def get_dependencies(lock_path, direct_names, deps_path \\ nil) do
    if File.exists?(lock_path) do
      try do
        lock_map = Mix.Dep.Lock.read(lock_path)
        lock_map |> parse_lock_map() |> mark_direct(direct_names) |> put_licenses(deps_path)
      rescue
        exception ->
          log_error(
            "ocibuild: failed to parse #{lock_path}, falling back to single-layer mode: " <>
              Exception.message(exception)
          )

          []
      end
    else
      # Don't log for missing lock file - it's expected for projects without dependencies
      []
    end
  end

  @type dependency() :: %{
          required(:name) => String.t(),
          required(:version) => String.t(),
          required(:source) => String.t(),
          optional(:deps) => [String.t()],
          optional(:direct) => boolean(),
          optional(:checksum) => String.t(),
          optional(:licenses) => [String.t()]
        }

  # Parse the lock map into a list of dependency info maps
  defp parse_lock_map(lock_map) do
    Enum.map(lock_map, fn
      {name, {:hex, _, version, _, _, deps, _, outer_checksum}} ->
        %{name: Atom.to_string(name), version: version, source: "hex", deps: required_deps(deps)}
        |> put_checksum(outer_checksum)

      {name, {:hex, _, version, _, _, deps, _}} ->
        %{name: Atom.to_string(name), version: version, source: "hex", deps: required_deps(deps)}

      {name, {:git, url, ref, _}} ->
        %{name: Atom.to_string(name), version: ref, source: url}

      {name, _} ->
        %{name: Atom.to_string(name), version: "unknown", source: "unknown"}
    end)
  end

  # The outer checksum is the SHA-256 of the Hex tarball. Only 8-tuple entries carry it.
  defp put_checksum(dep, checksum) when is_binary(checksum), do: Map.put(dep, :checksum, checksum)
  defp put_checksum(dep, _), do: dep

  # Names of the deps a hex package requires. Optional deps are skipped: a package can be
  # in the lock file because the project uses it directly, not because this one needs it.
  defp required_deps(deps) do
    for {name, _requirement, opts} <- deps, !Keyword.get(opts, :optional, false) do
      Atom.to_string(name)
    end
  end

  # Names declared in the project's mix.exs. `nil` for an umbrella root, whose own deps list
  # does not cover the child apps (the SBOM then treats unreferenced packages as direct).
  defp project_direct_names do
    if Mix.Project.umbrella?() do
      nil
    else
      for dep <- Mix.Project.config()[:deps] || [], do: dep |> elem(0) |> Atom.to_string()
    end
  end

  defp mark_direct(deps, nil), do: deps

  defp mark_direct(deps, direct_names) do
    Enum.map(deps, &Map.put(&1, :direct, &1.name in direct_names))
  end

  # License names a Hex package declares, from the hex_metadata.config unpacked next to its
  # source. Missing or unreadable metadata (git deps, not fetched) leaves the key out.
  defp put_licenses(deps, nil), do: deps

  defp put_licenses(deps, deps_path) do
    Enum.map(deps, fn
      %{source: "hex", name: name} = dep ->
        case read_licenses(Path.join([deps_path, name, "hex_metadata.config"])) do
          [] -> dep
          licenses -> Map.put(dep, :licenses, licenses)
        end

      dep ->
        dep
    end)
  end

  defp read_licenses(metadata_path) do
    with {:ok, terms} <- :file.consult(String.to_charlist(metadata_path)),
         {"licenses", licenses} when is_list(licenses) <- List.keyfind(terms, "licenses", 0) do
      Enum.filter(licenses, &is_binary/1)
    else
      _ -> []
    end
  end

  # Log error message using appropriate method based on context
  defp log_error(message) do
    if function_exported?(Mix, :shell, 0) do
      Mix.shell().error(message)
    else
      IO.warn(message)
    end
  end
end
