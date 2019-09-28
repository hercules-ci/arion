{ pkgs ? import ./. {}
, lib ? pkgs.lib
, haskellPackages ? pkgs.haskellPackages
, arion-compose ? import ./haskell-arion-compose.nix { inherit pkgs haskellPackages; }
}:

let
  inherit (pkgs.haskell.lib) justStaticExecutables overrideCabal;

  srcDir = ../src;
  eval = import (srcDir + "/nix/eval-composition.nix");
  build = args@{...}:
    let composition = eval args;
    in composition.config.build.dockerComposeYaml;

in
  justStaticExecutables (overrideCabal arion-compose (o: {
    buildTools = o.buildTools ++ [pkgs.makeWrapper];
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
        --prefix PATH : ${lib.makeBinPath [ pkgs.docker-compose ]} \
        ;
    '';
  }))
