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
  Returns an empty list if mix.lock doesn't exist or can't be parsed.

  ## Examples

      iex> Ocibuild.Lock.get_dependencies()
      [
        %{name: "jason", version: "1.4.0", source: "hex"},
        %{name: "plug", version: "1.14.0", source: "hex"}
      ]
  """
  @spec get_dependencies() :: [%{name: String.t(), version: String.t(), source: String.t()}]
  def get_dependencies do
    get_dependencies("mix.lock")
  end

  @doc """
  Get dependencies from a specific lock file path.

  This variant is useful for testing or when the lock file is not in the current directory.
  """
  @spec get_dependencies(Path.t()) :: [
          %{name: String.t(), version: String.t(), source: String.t()}
        ]
  def get_dependencies(lock_path) do
    if File.exists?(lock_path) do
      try do
        lock_map = Mix.Dep.Lock.read(lock_path)
        parse_lock_map(lock_map)
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

  # Parse the lock map into a list of dependency info maps
  defp parse_lock_map(lock_map) do
    Enum.map(lock_map, fn
      {name, {:hex, _, version, _, _, _, _, _}} ->
        %{name: Atom.to_string(name), version: version, source: "hex"}

      {name, {:hex, _, version, _, _, _, _}} ->
        %{name: Atom.to_string(name), version: version, source: "hex"}

      {name, {:git, url, ref, _}} ->
        %{name: Atom.to_string(name), version: ref, source: url}

      {name, _} ->
        %{name: Atom.to_string(name), version: "unknown", source: "unknown"}
    end)
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
