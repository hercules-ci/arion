{ pkgs ? import ../pkgs.nix, arionTestingFlags ? {} }:
let
  inherit (pkgs) nixosTest recurseIntoAttrs arion;
in

recurseIntoAttrs {

  test = nixosTest ./arion-test;

  testWithPodman =
    if arionTestingFlags.nixosHasPodmanDockerSocket
    then nixosTest (pkgs.callPackage ./arion-test { usePodman = true; })
    else {};

  testBuild = arion.build {

    # To be more accurately, you can do
    #   pkgs = import ../examples/minimal/arion-pkgs.nix;
    # but this is quite efficient:
    inherit pkgs;

    modules = [ ../examples/minimal/arion-compose.nix ];
  };

}
