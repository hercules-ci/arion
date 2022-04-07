{ pkgs ? import ../pkgs.nix, arionTestingFlags ? {} }:
let
  inherit (pkgs) nixosTest recurseIntoAttrs arion lib;

  hasEvalModulesType = (lib.evalModules { modules = [ {} ]; })?type;

in

recurseIntoAttrs {

  test = nixosTest ./arion-test;

  nixosModuleWithDocker =
    lib.optionalAttrs
      hasEvalModulesType
      (
        import ./nixos-virtualization-arion-test/test.nix pkgs {
          virtualisation.arion.backend = "docker";
        }
      );

  nixosModuleWithPodman =
    lib.optionalAttrs
      (hasEvalModulesType && arionTestingFlags.nixosHasPodmanDockerSocket)
      (
        import ./nixos-virtualization-arion-test/test.nix pkgs {
          virtualisation.arion.backend = "podman-socket";
        }
      );

  testWithPodman =
    if arionTestingFlags.nixosHasPodmanDockerSocket
    then nixosTest (import ./arion-test { usePodman = true; inherit pkgs lib; })
    else {};

  testBuild = arion.build {

    # To be more accurately, you can do
    #   pkgs = import ../examples/minimal/arion-pkgs.nix;
    # but this is quite efficient:
    inherit pkgs;

    modules = [ ../examples/minimal/arion-compose.nix ];
  };

}
