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
      (preEval [ ../../examples/minimal/arion-compose.nix ]).config.build.dockerComposeYaml
      (preEval [ ../../examples/full-nixos/arion-compose.nix ]).config.build.dockerComposeYaml
      (preEval [ ../../examples/nixos-unit/arion-compose.nix ]).config.build.dockerComposeYaml
      pkgs.stdenv
    ];
  };
  testScript = ''
    $machine->fail("curl localhost:8000");
    $machine->succeed("docker --version");

    subtest "minimal", sub {
      $machine->succeed("cp -r ${../../examples/minimal} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d");
      $machine->waitUntilSucceeds("curl localhost:8000");
      $machine->succeed("cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down && rm -rf work");
      $machine->waitUntilFails("curl localhost:8000");
    };

    subtest "full-nixos", sub {
      $machine->succeed("cp -r ${../../examples/full-nixos} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d");
      $machine->waitUntilSucceeds("curl localhost:8000");
      # Also test exec with defaultExec
      $machine->succeed("cd work && export NIX_PATH=nixpkgs='${pkgs.path}' && (echo 'nix run -f ~/h/arion arion -c arion exec webserver'; echo 'target=world; echo Hello \$target'; echo exit) | script /dev/null | grep 'Hello world'");
      $machine->succeed("cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down && rm -rf work");
      $machine->waitUntilFails("curl localhost:8000");
    };

    subtest "nixos-unit", sub {
      $machine->succeed("cp -r ${../../examples/nixos-unit} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d");
      $machine->waitUntilSucceeds("curl localhost:8000");
      $machine->succeed("cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down && rm -rf work");
      $machine->waitUntilFails("curl localhost:8000");
    };
  '';
}
