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
      pkgs.stdenv
    ];
  };
  testScript = ''
    $machine->fail("curl localhost:8000");
    $machine->succeed("docker --version");

    my $makeSubtest = sub {
      my ( $subtestName, $exampleSrc, @codeRefs ) = @_;

      subtest $subtestName => sub {
        $machine->succeed("rm -rf work && cp -frT $exampleSrc work && cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion up -d");
        $machine->waitUntilSucceeds("curl localhost:8000");
        $_->() for @codeRefs;
        $machine->succeed("cd work && NIX_PATH=nixpkgs='${pkgs.path}' arion down");
        $machine->waitUntilFails("curl localhost:8000");
      };
    };

    $makeSubtest->("minimal", "${../../examples/minimal}");
    $makeSubtest->("full-nixos", "${../../examples/full-nixos}", sub {
      $machine->succeed("cd work && export NIX_PATH=nixpkgs='${pkgs.path}' && (echo 'nix run -f ~/h/arion arion -c arion exec webserver'; echo 'target=world; echo Hello \$target'; echo exit) | script /dev/null | grep 'Hello world'");
    });
    $makeSubtest->("nixos-unit", "${../../examples/nixos-unit}");
  '';
}
