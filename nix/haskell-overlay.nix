self: super: hself: hsuper:
{
  arion-compose = import ./haskell-arion-compose.nix { pkgs = self; haskellPackages = hself; };
  arion-compose-checked =
              let pkg = super.haskell.lib.buildStrictly hself.arion-compose;
                  checked = super.haskell.lib.overrideCabal pkg (o: {
                    postConfigure = ''${o.postConfigure or ""}
                      if ! ${hsuper.cabal-install}/bin/cabal check;
                      then
                        echo 1>&2 ERROR: cabal file is invalid. Above warnings were errors.
                        exit 1
                      fi
                    '';
                  });
              in checked;
}