args@{ pkgs ? import ./nix args, ... }:

{
  inherit (pkgs) arion tests;
}
