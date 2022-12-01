let flake = import ./nix/compat.nix;
in
{ pkgs ? import flake.inputs.nixpkgs { }
, haskellPackages ? pkgs.haskellPackages
}:
let
  pkgsWithArion = pkgs.extend flake.overlays.default;
in
{
  inherit (pkgsWithArion) arion;
}
