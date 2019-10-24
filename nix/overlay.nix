self: super:
let
  inherit (self.arion-project) haskellPkgs;
  inherit (super) lib;

  sources = import ./sources.nix;

in
{

  inherit (import ./.. { pkgs = self; }) arion;
  tests = super.callPackage ../tests {};

  doc-options = import ../docs/options.nix {};
  doc-options-check = self.runCommand "doc-options-check" {} ''
    diff --color -u ${../docs/modules/ROOT/partials/NixOSOptions.adoc} ${self.doc-options}
    touch $out
  '';
  doc = self.stdenv.mkDerivation { 
    name = "arion-documentation"; 
    buildInputs = [super.antora]; 
    src = ../.;
    HOME = ".";
    buildPhase = "antora antora-playbook";
    installPhase = ''
      mkdir $out
      mv public/* $out/
    '';
  };

  arion-project = super.recurseIntoAttrs {
    haskellPkgs = super.haskellPackages.extend (import ./haskell-overlay.nix self super);
    shell = haskellPkgs.shellFor {
      packages = p: [p.arion-compose];
      buildInputs = [
        haskellPkgs.cabal-install
        haskellPkgs.ghcid
        super.docker-compose
        self.niv
        self.releaser
      ];
    };
  };

  inherit (import (sources.niv) {}) niv;
  releaser = self.haskellPackages.callCabal2nix "releaser" sources.releaser {};
}
