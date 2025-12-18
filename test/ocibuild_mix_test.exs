defmodule OcibuildMixTest do
  use ExUnit.Case, async: true

  describe "file collection" do
    test "collects files from a mock release directory" do
      tmp_dir = create_mock_release()

      try do
        {:ok, files} = :ocibuild_release.collect_release_files(to_charlist(tmp_dir))

        # Should have collected the files we created
        assert length(files) >= 3

        # Check that bin/myapp exists with correct path
        bin_file = Enum.find(files, fn {path, _, _} -> path == "/app/bin/myapp" end)
        assert bin_file != nil
        {_, _, bin_mode} = bin_file
        assert Bitwise.band(bin_mode, 0o777) == 0o755

        # Check lib file exists
        lib_file =
          Enum.find(files, fn {path, _, _} ->
            path == "/app/lib/myapp-1.0.0/ebin/myapp.beam"
          end)

        assert lib_file != nil
        {_, _, lib_mode} = lib_file
        assert Bitwise.band(lib_mode, 0o777) == 0o644
      after
        cleanup_temp_dir(tmp_dir)
      end
    end

    test "collects empty directory" do
      tmp_dir = make_temp_dir("ocibuild_empty")

      try do
        {:ok, files} = :ocibuild_release.collect_release_files(to_charlist(tmp_dir))
        assert files == []
      after
        cleanup_temp_dir(tmp_dir)
      end
    end
  end

  describe "image building" do
    test "builds scratch image with files" do
      files = [
        {"/app/bin/myapp", "#!/bin/sh\necho hello", 0o755},
        {"/app/lib/myapp.beam", "beam_data", 0o644}
      ]

      {:ok, image} =
        :ocibuild_release.build_image(
          "scratch",
          files,
          ~c"myapp",
          "/app",
          %{},
          [],
          %{},
          "foreground",
          %{}
        )

      assert is_map(image)
      assert length(:maps.get(:layers, image)) == 1

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      assert :maps.get("Entrypoint", inner_config) == ["/app/bin/myapp", "foreground"]
      assert :maps.get("WorkingDir", inner_config) == "/app"
    end

    test "builds image with environment variables" do
      files = [{"/app/test", "data", 0o644}]
      env_map = %{"LANG" => "C.UTF-8", "PORT" => "8080"}

      {:ok, image} =
        :ocibuild_release.build_image(
          "scratch",
          files,
          ~c"myapp",
          "/app",
          env_map,
          [],
          %{},
          "foreground",
          %{}
        )

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      env_list = :maps.get("Env", inner_config)

      assert Enum.any?(env_list, fn e -> String.contains?(to_string(e), "LANG=") end)
      assert Enum.any?(env_list, fn e -> String.contains?(to_string(e), "PORT=") end)
    end

    test "builds image with exposed ports" do
      files = [{"/app/test", "data", 0o644}]

      {:ok, image} =
        :ocibuild_release.build_image(
          "scratch",
          files,
          ~c"myapp",
          "/app",
          %{},
          [8080, 443],
          %{},
          "foreground",
          %{}
        )

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      exposed_ports = :maps.get("ExposedPorts", inner_config)

      assert :maps.is_key("8080/tcp", exposed_ports)
      assert :maps.is_key("443/tcp", exposed_ports)
    end

    test "builds image with labels" do
      files = [{"/app/test", "data", 0o644}]
      labels = %{"org.opencontainers.image.version" => "1.0.0"}

      {:ok, image} =
        :ocibuild_release.build_image(
          "scratch",
          files,
          ~c"myapp",
          "/app",
          %{},
          [],
          labels,
          "foreground",
          %{}
        )

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      image_labels = :maps.get("Labels", inner_config)

      assert :maps.get("org.opencontainers.image.version", image_labels) == "1.0.0"
    end
  end

  describe "push authentication" do
    test "returns empty map when no env vars set" do
      System.delete_env("OCIBUILD_PUSH_TOKEN")
      System.delete_env("OCIBUILD_PUSH_USERNAME")
      System.delete_env("OCIBUILD_PUSH_PASSWORD")

      assert :ocibuild_release.get_push_auth() == %{}
    end

    test "returns token auth when OCIBUILD_PUSH_TOKEN set" do
      System.put_env("OCIBUILD_PUSH_TOKEN", "mytoken123")

      try do
        auth = :ocibuild_release.get_push_auth()
        assert auth == %{token: "mytoken123"}
      after
        System.delete_env("OCIBUILD_PUSH_TOKEN")
      end
    end

    test "returns username/password auth when set" do
      System.delete_env("OCIBUILD_PUSH_TOKEN")
      System.put_env("OCIBUILD_PUSH_USERNAME", "myuser")
      System.put_env("OCIBUILD_PUSH_PASSWORD", "mypass")

      try do
        auth = :ocibuild_release.get_push_auth()
        assert auth == %{username: "myuser", password: "mypass"}
      after
        System.delete_env("OCIBUILD_PUSH_USERNAME")
        System.delete_env("OCIBUILD_PUSH_PASSWORD")
      end
    end
  end

  describe "pull authentication" do
    test "returns empty map when no env vars set" do
      System.delete_env("OCIBUILD_PULL_TOKEN")
      System.delete_env("OCIBUILD_PULL_USERNAME")
      System.delete_env("OCIBUILD_PULL_PASSWORD")

      assert :ocibuild_release.get_pull_auth() == %{}
    end

    test "returns username/password auth when set" do
      System.delete_env("OCIBUILD_PULL_TOKEN")
      System.put_env("OCIBUILD_PULL_USERNAME", "pulluser")
      System.put_env("OCIBUILD_PULL_PASSWORD", "pullpass")

      try do
        auth = :ocibuild_release.get_pull_auth()
        assert auth == %{username: "pulluser", password: "pullpass"}
      after
        System.delete_env("OCIBUILD_PULL_USERNAME")
        System.delete_env("OCIBUILD_PULL_PASSWORD")
      end
    end
  end

  # Cross-platform helpers

  defp temp_dir do
    case :os.type() do
      {:win32, _} ->
        System.get_env("TEMP") || System.get_env("TMP") || "C:\\Temp"

      _ ->
        "/tmp"
    end
  end

  defp make_temp_dir(prefix) do
    unique = :erlang.unique_integer([:positive])
    dir_name = "#{prefix}_#{unique}"
    tmp_dir = Path.join(temp_dir(), dir_name)
    File.mkdir_p!(tmp_dir)
    tmp_dir
  end

  defp cleanup_temp_dir(dir) do
    File.rm_rf!(dir)
  end

  defp create_mock_release do
    tmp_dir = make_temp_dir("ocibuild_release")

    # Create directory structure
    bin_dir = Path.join(tmp_dir, "bin")
    lib_dir = Path.join([tmp_dir, "lib", "myapp-1.0.0", "ebin"])
    rel_dir = Path.join([tmp_dir, "releases", "1.0.0"])

    File.mkdir_p!(bin_dir)
    File.mkdir_p!(lib_dir)
    File.mkdir_p!(rel_dir)

    # Create bin script (executable)
    bin_path = Path.join(bin_dir, "myapp")
    File.write!(bin_path, "#!/bin/sh\nexec erl -boot release")
    File.chmod!(bin_path, 0o755)

    # Create beam file
    beam_path = Path.join(lib_dir, "myapp.beam")
    File.write!(beam_path, "FOR1...(beam data)")
    File.chmod!(beam_path, 0o644)

    # Create release file
    rel_path = Path.join(rel_dir, "myapp.rel")
    File.write!(rel_path, "{release, {\"myapp\", \"1.0.0\"}, ...}.")

    tmp_dir
  end
end
