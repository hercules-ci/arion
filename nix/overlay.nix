self: super:
let
  inherit (self.arion-project) haskellPkgs;
  inherit (super) lib;

  sources = import ./sources.nix;

  fakeRepo = src: super.runCommand "source" { inherit src; nativeBuildInputs = [super.git]; } ''
    cp -r --no-preserve=mode $src $out
    git init
    cp -r .git $out
  '';

in
{

  inherit (import ./.. { pkgs = self; }) arion;
  tests = super.callPackage ../tests {};

  doc-options = import ../docs/options.nix {};
  doc-options-check = self.runCommand "doc-options-check" {} ''
    if diff --color -u ${../docs/modules/ROOT/partials/NixOSOptions.adoc} ${self.doc-options}; then
      touch $out
    else
      echo 1>&2 "The doc options have changed and need to be added."
      echo 1>&2 "Please run ./update-options in the root of your arion clone."
      exit 1
    fi
  '';
  doc = self.stdenv.mkDerivation { 
    name = "arion-documentation"; 
    nativeBuildInputs = [super.antora];
    src = fakeRepo ../.;
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
      nativeBuildInputs = [
        haskellPkgs.cabal-install
        haskellPkgs.ghcid
        haskellPkgs.haskell-language-server
        super.docker-compose
        self.niv
        self.nixpkgs-fmt
        self.releaser
      ];
    };
  };

  inherit (import (sources.niv) {}) niv;
  releaser = self.haskellPackages.callCabal2nix "releaser" sources.releaser {};
}
