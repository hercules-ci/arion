{ pkgs ? import ../nix {} }:
let
  inherit (pkgs) recurseIntoAttrs callPackage;
in

recurseIntoAttrs {
  manual = callPackage ./manual {};
}
