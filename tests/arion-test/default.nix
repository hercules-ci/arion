{ pkgs, ... }:

let
  # To make some prebuilt derivations available in the vm
  preEval = modules: import ../../src/nix/eval-composition.nix {
    inherit modules;
    inherit pkgs;
  };
in
{
  name = "arion-test";
  machine = { pkgs, lib, ... }: {
    environment.systemPackages = [
      pkgs.arion
    ];
    virtualisation.docker.enable = true;
    
    # no caches, because no internet
    nix.binaryCaches = lib.mkForce [];

    # FIXME: Sandbox seems broken with current version of NixOS test
    #        w/ writable store. Error:
    #        machine# error: linking '/nix/store/7r8z2zvhwda85pgpdn5hzzz6hs1njklc-stdenv-linux.drv.chroot/nix/store/6v3y7s4q4wd16hsw393gjpxvcf9159bv-patch-shebangs.sh' to '/nix/store/6v3y7s4q4wd16hsw393gjpxvcf9159bv-patch-shebangs.sh': Operation not permitted
    #
    #        There should be no reason why arion can't run without
    #        sandboxing, so please re-enable.
    nix.useSandbox = false;

    virtualisation.writableStore = true;
    virtualisation.pathsInNixDB = [
      # Pre-build the image because we don't want to build the world
      # in the vm.
      (preEval [ ../../examples/minimal/arion-compose.nix ]).config.out.dockerComposeYaml
      (preEval [ ../../examples/full-nixos/arion-compose.nix ]).config.out.dockerComposeYaml
      (preEval [ ../../examples/nixos-unit/arion-compose.nix ]).config.out.dockerComposeYaml
      (preEval [ ../../examples/traefik/arion-compose.nix ]).config.out.dockerComposeYaml
      pkgs.stdenv
    ];

    virtualisation.memorySize = 1024;
  };
  testScript = ''
    machine.fail("curl localhost:8000")
    machine.succeed("docker --version")

    # Tests
    #  - arion up
    #  - arion down
    #  - examples/minimal
    with subtest("minimal"):
        machine.succeed(
            "rm -rf work && cp -frT ${../../examples/minimal} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d"
        )
        machine.wait_until_succeeds("curl localhost:8000")
        machine.succeed(
            "cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down"
        )
        machine.wait_until_fails("curl localhost:8000")

    # Tests
    #  - arion exec
    #  - examples/full-nixos
    with subtest("full-nixos"):
        machine.succeed(
            "rm -rf work && cp -frT ${../../examples/full-nixos} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d"
        )
        machine.wait_until_succeeds("curl localhost:8000")

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
        machine.wait_until_fails("curl localhost:8000")

    # Tests
    #  - examples/nixos-unit
    with subtest("nixos-unit"):
        machine.succeed(
            "rm -rf work && cp -frT ${../../examples/nixos-unit} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d"
        )
        machine.wait_until_succeeds("curl localhost:8000")
        machine.succeed(
            "cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down"
        )
        machine.wait_until_fails("curl localhost:8000")

    # Tests
    # - examples/traefik
    # - labels
    with subtest("traefik"):
        machine.succeed(
            "rm -rf work && cp -frT ${../../examples/traefik} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d"
        )
        machine.wait_until_succeeds("curl nix-docs.localhost")
        machine.succeed(
            "cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down"
        )
        machine.wait_until_fails("curl nix-docs.localhost")

  '';
}
