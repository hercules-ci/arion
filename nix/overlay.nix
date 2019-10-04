self: super:
let
  inherit (self.arion-project) haskellPkgs;
  inherit (super) lib;

  sources = import ./sources.nix;

in
{

  arion = import ./arion.nix { pkgs = self; };
  tests = super.callPackage ../tests {};
  doc = super.callPackage ../doc {};

  arion-project = super.recurseIntoAttrs {
    haskellPkgs = super.haskellPackages.extend (import ./haskell-overlay.nix self super);
    shell = haskellPkgs.shellFor {
      packages = p: [p.arion-compose];
      buildInputs = [
        haskellPkgs.cabal-install
        haskellPkgs.ghcid
        super.docker-compose
        (import ~/h/ghcide-nix {}).ghcide-ghc864
        self.niv
      ];
    };
  };

  inherit (import (sources.niv) {}) niv;
}
