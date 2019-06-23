self: super:
let
  inherit (self.arion-project) haskellPkgs;

  srcDir = ../src; # TODO gitignoreSource + whitelist nix and arion-image
  eval = import (srcDir + "/nix/eval-composition.nix");
  build = args@{...}:
    let composition = eval args;
    in composition.config.build.dockerComposeYaml;
  hlib = super.haskell.lib;
in
{

  arion-v0 = super.callPackage ../arion.nix {};
  arion = hlib.justStaticExecutables (hlib.overrideCabal haskellPkgs.arion-compose (o: {
      passthru = o.passthru // {
        inherit eval build;
      };
    }));
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
