let
  sources = import ./sources.nix;
  lib = import (sources."nixpkgs" + "/lib");
  inherit (import sources."project.nix" { inherit lib; }) dimension;
in

dimension "Nixpkgs version" {
    "nixos-19_03" = {
      nixpkgsSource = "nixpkgs";
    };
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";

      # Broken on unstable, wontfix because doc tooling will be changed.
      # TODO: reenable
      enableDoc = false;
    };
  } (
    _name: { nixpkgsSource, enableDoc ? true }:


      dimension "System" {
        "x86_64-linux" = {};
        # TODO: darwin
        # "x86_64-darwin" = { enableNixOSTests = false; };
      } (
        system: {}:
          let
            pkgs = import ./. { inherit system; nixpkgsSrc = sources.${nixpkgsSource}; };
          in
          {
            inherit (pkgs) arion tests;
          } // lib.optionalAttrs enableDoc {
            doc = pkgs.recurseIntoAttrs (import ../doc { inherit pkgs; });
          }
      )
  )
