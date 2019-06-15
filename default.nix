args@{ pkgs ? import ./nix args, system ? null, ... }:

{
  inherit (pkgs) arion tests;
  doc = pkgs.recurseIntoAttrs (import ./doc { inherit pkgs; });
}
