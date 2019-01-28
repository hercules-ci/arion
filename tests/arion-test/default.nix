{ pkgs, ... }:

let
  # To make some prebuilt derivations available in the vm
  preEval = import ../../src/nix/eval-composition.nix {
    modules = [ ../../examples/minimal/arion-compose.nix ];
    inherit pkgs;
  };
in
{
  name = "arion-test";
  machine = { pkgs, lib, ... }: {
    environment.systemPackages = [
      pkgs.arion
      pkgs.docker-compose
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
      preEval.config.build.dockerComposeYaml
      pkgs.stdenv
    ];
  };
  testScript = ''
    $machine->fail("curl localhost:8000");
    $machine->succeed("docker --version");
    $machine->succeed("cp -r ${../../examples/minimal} work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d");
    $machine->waitUntilSucceeds("curl localhost:8000");
  '';
}
