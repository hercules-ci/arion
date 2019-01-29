{ pkgs ? import ../pkgs.nix }:
let
  inherit (pkgs) nixosTest recurseIntoAttrs;
in

recurseIntoAttrs {
  test = nixosTest ./arion-test;
  preEval = pkgs.callPackage ./arion-test/preeval.nix {};
}
