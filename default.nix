{ pkgs ? import ./pkgs.nix }:

{
  inherit (pkgs) arion;
  tests = pkgs.callPackage ./tests {};
}
