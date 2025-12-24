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
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app"
        })

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
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          env: env_map
        })

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      env_list = :maps.get("Env", inner_config)

      assert Enum.any?(env_list, fn e -> String.contains?(to_string(e), "LANG=") end)
      assert Enum.any?(env_list, fn e -> String.contains?(to_string(e), "PORT=") end)
    end

    test "builds image with exposed ports" do
      files = [{"/app/test", "data", 0o644}]

      {:ok, image} =
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          expose: [8080, 443]
        })

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
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          labels: labels
        })

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      image_labels = :maps.get("Labels", inner_config)

      assert :maps.get("org.opencontainers.image.version", image_labels) == "1.0.0"
    end

    test "builds image with custom uid" do
      files = [{"/app/test", "data", 0o644}]

      {:ok, image} =
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          uid: 1000
        })

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      assert :maps.get("User", inner_config) == "1000"
    end

    test "builds image with uid 0 (root)" do
      files = [{"/app/test", "data", 0o644}]

      {:ok, image} =
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          uid: 0
        })

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      assert :maps.get("User", inner_config) == "0"
    end

    test "builds image with default uid (65534) when not specified" do
      files = [{"/app/test", "data", 0o644}]

      {:ok, image} =
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app"
        })

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      assert :maps.get("User", inner_config) == "65534"
    end

    test "builds image with nil uid defaults to 65534 (Elixir compatibility)" do
      files = [{"/app/test", "data", 0o644}]

      {:ok, image} =
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          uid: nil
        })

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      assert :maps.get("User", inner_config) == "65534"
    end

    test "builds image with custom cmd" do
      files = [
        {"/app/bin/myapp", "#!/bin/sh\necho hello", 0o755}
      ]

      {:ok, image} =
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          cmd: "daemon"
        })

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      # Entrypoint should include the custom cmd
      assert :maps.get("Entrypoint", inner_config) == ["/app/bin/myapp", "daemon"]
    end

    test "builds image with start cmd (Elixir default)" do
      files = [
        {"/app/bin/myapp", "#!/bin/sh\necho hello", 0o755}
      ]

      {:ok, image} =
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          cmd: "start"
        })

      config = :maps.get(:config, image)
      inner_config = :maps.get("config", config)
      assert :maps.get("Entrypoint", inner_config) == ["/app/bin/myapp", "start"]
    end

    test "builds image with valid chunk_size" do
      files = [{"/app/test", "data", 0o644}]

      # 10MB is within valid range (1-100)
      {:ok, _image} =
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          chunk_size: 10 * 1024 * 1024
        })
    end

    test "builds image with undefined chunk_size uses default" do
      files = [{"/app/test", "data", 0o644}]

      {:ok, _image} =
        :ocibuild_release.build_image("scratch", files, %{
          release_name: ~c"myapp",
          workdir: "/app",
          chunk_size: nil
        })
    end
  end

  describe "chunk_size constants" do
    test "adapter exposes chunk_size constants" do
      assert :ocibuild_adapter.min_chunk_size_mb() == 1
      assert :ocibuild_adapter.max_chunk_size_mb() == 100
      assert :ocibuild_adapter.default_chunk_size_mb() == 5
    end

    test "adapter exposes chunk_size in bytes" do
      assert :ocibuild_adapter.min_chunk_size() == 1 * 1024 * 1024
      assert :ocibuild_adapter.max_chunk_size() == 100 * 1024 * 1024
      assert :ocibuild_adapter.default_chunk_size() == 5 * 1024 * 1024
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

  describe "multi-platform support" do
    test "detects bundled ERTS in release" do
      tmp_dir = create_mock_release_with_erts()

      try do
        assert :ocibuild_release.has_bundled_erts(to_charlist(tmp_dir)) == true
      after
        cleanup_temp_dir(tmp_dir)
      end
    end

    test "detects no ERTS when not bundled" do
      tmp_dir = create_mock_release()

      try do
        assert :ocibuild_release.has_bundled_erts(to_charlist(tmp_dir)) == false
      after
        cleanup_temp_dir(tmp_dir)
      end
    end

    test "detects native code .so files in priv directory" do
      tmp_dir = create_mock_release_with_nif(".so")

      try do
        {:warning, nif_files} = :ocibuild_release.check_for_native_code(to_charlist(tmp_dir))
        assert length(nif_files) > 0
        [nif_info | _] = nif_files
        assert :maps.get(:extension, nif_info) == ".so"
      after
        cleanup_temp_dir(tmp_dir)
      end
    end

    test "detects native code .dll files in priv directory" do
      tmp_dir = create_mock_release_with_nif(".dll")

      try do
        {:warning, nif_files} = :ocibuild_release.check_for_native_code(to_charlist(tmp_dir))
        assert length(nif_files) > 0
        [nif_info | _] = nif_files
        assert :maps.get(:extension, nif_info) == ".dll"
      after
        cleanup_temp_dir(tmp_dir)
      end
    end

    test "detects native code .dylib files in priv directory" do
      tmp_dir = create_mock_release_with_nif(".dylib")

      try do
        {:warning, nif_files} = :ocibuild_release.check_for_native_code(to_charlist(tmp_dir))
        assert length(nif_files) > 0
        [nif_info | _] = nif_files
        assert :maps.get(:extension, nif_info) == ".dylib"
      after
        cleanup_temp_dir(tmp_dir)
      end
    end

    test "returns ok when no native code present" do
      tmp_dir = create_mock_release()

      try do
        assert :ocibuild_release.check_for_native_code(to_charlist(tmp_dir)) == {:ok, []}
      after
        cleanup_temp_dir(tmp_dir)
      end
    end

    test "validate_multiplatform errors with bundled ERTS" do
      tmp_dir = create_mock_release_with_erts()

      try do
        platforms = [
          %{os: "linux", architecture: "amd64"},
          %{os: "linux", architecture: "arm64"}
        ]

        result = :ocibuild_release.validate_multiplatform(to_charlist(tmp_dir), platforms)
        assert {:error, {:bundled_erts, _msg}} = result
      after
        cleanup_temp_dir(tmp_dir)
      end
    end

    test "validate_multiplatform succeeds without ERTS for multiple platforms" do
      tmp_dir = create_mock_release()

      try do
        platforms = [
          %{os: "linux", architecture: "amd64"},
          %{os: "linux", architecture: "arm64"}
        ]

        assert :ocibuild_release.validate_multiplatform(to_charlist(tmp_dir), platforms) == :ok
      after
        cleanup_temp_dir(tmp_dir)
      end
    end

    test "validate_multiplatform succeeds with ERTS for single platform" do
      tmp_dir = create_mock_release_with_erts()

      try do
        platforms = [%{os: "linux", architecture: "amd64"}]
        assert :ocibuild_release.validate_multiplatform(to_charlist(tmp_dir), platforms) == :ok
      after
        cleanup_temp_dir(tmp_dir)
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

  defp create_mock_release_with_erts do
    tmp_dir = create_mock_release()
    erts_dir = Path.join(tmp_dir, "erts-15.0")
    File.mkdir_p!(Path.join(erts_dir, "bin"))
    File.write!(Path.join([erts_dir, "bin", "beam.smp"]), "beam binary")
    tmp_dir
  end

  defp create_mock_release_with_nif(extension) do
    tmp_dir = create_mock_release()
    priv_dir = Path.join([tmp_dir, "lib", "crypto-1.0.0", "priv"])
    File.mkdir_p!(priv_dir)
    File.write!(Path.join(priv_dir, "crypto_nif#{extension}"), "fake nif binary")
    tmp_dir
  end
end
