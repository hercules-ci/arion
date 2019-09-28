self: super: hself: hsuper:
{
  arion-compose = import ./haskell-arion-compose.nix { pkgs = self; haskellPackages = hself; };
}