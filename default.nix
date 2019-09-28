{ pkgs ? import ./nix {}
, haskellPackages ? pkgs.haskellPackages
}:
{
  arion = import ./nix/arion.nix { inherit pkgs haskellPackages; };
}
