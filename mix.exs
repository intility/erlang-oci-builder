defmodule Ocibuild.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/rhblind/ocibuild"

  def project do
    [
      app: :ocibuild,
      version: @version,
      elixir: "~> 1.14",
      erlc_paths: ["src"],
      erlc_include_path: "include",
      elixirc_paths: ["lib"],
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: [:crypto, :ssl, :inets]
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
    ]
  end

  defp description do
    "Build and publish OCI container images from the BEAM - no Docker required."
  end

  defp package do
    [
      name: "ocibuild",
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @source_url},
      files: ~w(lib src include priv mix.exs rebar.config README.md LICENSE CHANGELOG.md)
    ]
  end

  defp docs do
    [
      main: "readme",
      source_url: @source_url,
      extras: ["README.md"]
    ]
  end
end
