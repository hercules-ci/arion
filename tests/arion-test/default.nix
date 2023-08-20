{ usePodman ? false, pkgs, lib ? pkgs.lib, ... }:

let
  # To make some prebuilt derivations available in the vm
  preEval = modules: import ../../src/nix/eval-composition.nix {
    inherit modules;
    inherit pkgs;
  };

  inherit (lib)
    concatMapStringsSep
    optionalAttrs
    optionalString
    ;

  haveSystemd = usePodman || pkgs.arionTestingFlags.dockerSupportsSystemd;

  concatPathLines = paths: concatMapStringsSep "\n" (x: "${x}") paths;

in
{
  name = "arion-test";
  nodes.machine = { pkgs, lib, ... }: {
    environment.systemPackages = [
      pkgs.arion
    ] ++ lib.optional usePodman pkgs.docker;
    virtualisation.docker.enable = !usePodman;
    virtualisation.podman = optionalAttrs usePodman {
      enable = true;
      dockerSocket.enable = true;
    };

    # no caches, because no internet
    nix.settings.substituters = lib.mkForce [];

    virtualisation.writableStore = true;
    # Switch to virtualisation.additionalPaths when dropping all NixOS <= 21.05.
    environment.etc."extra-paths-for-test".text = concatPathLines [
      # Pre-build the image because we don't want to build the world
      # in the vm.
      (preEval [ ../../examples/minimal/arion-compose.nix ]).config.out.dockerComposeYaml
      (preEval [ ../../examples/full-nixos/arion-compose.nix ]).config.out.dockerComposeYaml
      (preEval [ ../../examples/nixos-unit/arion-compose.nix ]).config.out.dockerComposeYaml
      (preEval [ ../../examples/traefik/arion-compose.nix ]).config.out.dockerComposeYaml
      pkgs.stdenv
    ];

    virtualisation.memorySize = 1024;
    virtualisation.diskSize = 8000;
  };
  testScript = ''
    machine.fail("curl --fail localhost:8000")
    machine.succeed("docker --version")

    # Tests
    #  - arion up
    #  - arion down
    #  - examples/minimal
    with subtest("minimal"):
        machine.succeed(
            "rm -rf work && cp -frT ${../../examples/minimal} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d"
        )
        machine.wait_until_succeeds("curl --fail localhost:8000")
        machine.succeed(
            "cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down"
        )
        machine.wait_until_fails("curl --fail localhost:8000")

    # Tests
    #  - running same image again doesn't require a `docker load`
    with subtest("docker load only once"):
        # We assume image loading relies on the `docker images` and `docker load` commands, so this should fail
        machine.fail(
            "export REAL_DOCKER=$(which docker); rm -rf work && cp -frT ${../../examples/minimal} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' PATH=\"${pkgs.writeScriptBin "docker" ''
              #!${pkgs.runtimeShell} -eu
              echo 1>&2 "This failure is expected. Args were" "$@"
              echo "$@" >/tmp/docker-args
              exit 1
            ''}/bin:$PATH\" arion up -d"
        )
        machine.succeed(
            "export REAL_DOCKER=$(which docker); rm -rf work && cp -frT ${../../examples/minimal} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' PATH=\"${pkgs.writeScriptBin "docker" ''
              #!${pkgs.runtimeShell} -eu
              case $1 in
                load)
                  echo 1>&2 "arion must not docker load when upping the same deployment for the second time"
                  exit 1
                  ;;
                images)
                  echo 1>&2 "execing docker to list images"
                  exec $REAL_DOCKER "$@"
                  ;;
                *)
                  echo 1>&2 "Unknown docker invocation. This may be a shortcoming of this docker mock."
                  echo 1>&2 "Invocation: docker" "$@"
                  ;;
              esac
            ''}/bin:$PATH\" arion up -d"
        )
        machine.wait_until_succeeds("curl --fail localhost:8000")
        machine.succeed(
            "cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down"
        )
        machine.wait_until_fails("curl --fail localhost:8000")


    # Tests
    #  - examples/flake
    # This _test_ doesn't work because flake-compat fetches the github
    # tarballs without sha256 and/or Nix doesn't consult the store before
    # downloading.
    # See https://github.com/edolstra/flake-compat/pull/12
    # with subtest("flake"):
    #     machine.succeed(
    #         "rm -rf work && cp -frT ''${../../examples/flake} work && cd work && NIX_PATH= arion up -d"
    #     )
    #     machine.wait_until_succeeds("curl --fail localhost:8000")
    #     machine.succeed("cd work && NIX_PATH= arion down")
    #     machine.wait_until_fails("curl --fail localhost:8000")

    ${optionalString haveSystemd ''
    # Tests
    #  - arion exec
    #  - examples/full-nixos
    with subtest("full-nixos"):
        machine.succeed(
            "rm -rf work && cp -frT ${../../examples/full-nixos} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d"
        )
        machine.wait_until_succeeds("curl --fail localhost:8000")

        machine.succeed(
            """
            set -eux -o pipefail
            cd work
            export NIX_PATH=nixpkgs='${pkgs.path}'
            echo 'target=world; echo Hello $target; exit' \
              | script 'arion exec webserver' \
              | grep 'Hello world'
            """
        ),

        machine.succeed(
            "cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down"
        )
        machine.wait_until_fails("curl --fail localhost:8000")
    ''}

    # Tests
    #  - examples/nixos-unit
    with subtest("nixos-unit"):
        machine.succeed(
            "rm -rf work && cp -frT ${../../examples/nixos-unit} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d"
        )
        machine.wait_until_succeeds("curl --fail localhost:8000")
        machine.succeed(
            "cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down"
        )
        machine.wait_until_fails("curl --fail localhost:8000")

    # Tests
    # - examples/traefik
    # - labels
    with subtest("traefik"):
        machine.succeed(
            "rm -rf work && cp -frT ${../../examples/traefik} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d"
        )
        machine.wait_until_succeeds("curl --fail nix-docs.localhost")
        machine.succeed(
            "cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down"
        )
        machine.wait_until_fails("curl --fail nix-docs.localhost")
  '';
}
