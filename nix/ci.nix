args@{ pkgs ? import ./default.nix args, system ? null, ... }:

{
  inherit (pkgs) arion tests;
  doc = pkgs.recurseIntoAttrs (import ../doc { inherit pkgs; });
}
