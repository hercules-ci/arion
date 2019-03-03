args@{ pkgs ? import ./nix args, ... }:

{
  inherit (pkgs) arion tests;
  doc = pkgs.recurseIntoAttrs (import ./doc { inherit pkgs; });
}
