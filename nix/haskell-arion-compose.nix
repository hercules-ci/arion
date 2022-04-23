
# NOTE: This file produces a haskell library, not the arion package!

{ pkgs ? import ./default.nix {}, haskellPackages ? pkgs.haskellPackages }:
let
  inherit (pkgs.haskell.lib) overrideCabal addBuildTools;
in
  overrideCabal (addBuildTools (haskellPackages.callCabal2nix "arion-compose" ./.. {}) [pkgs.nix]) (o: o // {
    src = pkgs.lib.sourceByRegex ../. [
      ".*[.]cabal"
      "LICENSE"
      "src/?.*"
      "README.asciidoc"
    ];
    preCheck = ''
      export NIX_LOG_DIR=$TMPDIR
      export NIX_STATE_DIR=$TMPDIR
      export NIX_PATH=nixpkgs=${pkgs.path}
    '';
  })