self: super:
let
  inherit (self.arion-project) haskellPkgs;
  inherit (super) lib;

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
      buildTools = o.buildTools ++ [super.makeWrapper];
      passthru = o.passthru // {
        inherit eval build;
      };
      pname = "arion"; # Cover up the needlessly long Haskell package name

      # PYTHONPATH
      #
      # We close off the python module search path!
      #
      # Accepting directories from the environment into the search path
      # tends to break things. Docker Compose does not have a plugin
      # system as far as I can tell, so I don't expect this to break a
      # feature, but rather to make the program more robustly self-
      # contained.

      postInstall = ''${o.postInstall or ""}
        mkdir -p $out/libexec
        mv $out/bin/arion $out/libexec
        makeWrapper $out/libexec/arion $out/bin/arion \
          --unset PYTHONPATH \
          --prefix PATH : ${lib.makeBinPath [ self.docker-compose ]} \
          ;
      '';
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
        super.docker-compose
      ];
    };
  };
}
