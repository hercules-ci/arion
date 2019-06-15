self: super:
let
  inherit (self.arion-project) haskellPkgs;
in
{
  arion-v0 = super.callPackage ../arion.nix {};
  arion = super.haskell.lib.justStaticExecutables haskellPkgs.arion-compose;
  tests = super.callPackage ../tests {};
  doc = super.callPackage ../doc {};

  arion-project = super.recurseIntoAttrs {
    haskellPkgs = super.haskellPackages.extend (import ./haskell-overlay.nix self super);
    shell = haskellPkgs.shellFor {
      packages = p: [p.arion-compose];
      buildInputs = [
        haskellPkgs.cabal-install
        haskellPkgs.ghcid
      ];
    };
  };
}
