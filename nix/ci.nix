let
  sources = import ./sources.nix;
  lib = import (sources."nixpkgs" + "/lib");
  inherit (import sources."project.nix" { inherit lib; }) dimension;
in

dimension "Nixpkgs version" {
    "nixos-19_03" = {
      nixpkgsSource = "nixpkgs";
      isReferenceNixpkgs = true;
    };
    "nixos-19_09" = {
      nixpkgsSource = "nixos-19.09";

      # Broken since 19.09, wontfix because doc tooling will be changed.
      # TODO: reenable
      enableDoc = false;
    };
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";

      # Broken since 19.09, wontfix because doc tooling will be changed.
      # TODO: reenable
      enableDoc = false;
    };
  } (
    _name: { nixpkgsSource, isReferenceNixpkgs ? false, enableDoc ? true }:


      dimension "System" {
        "x86_64-linux" = { isReferenceTarget = isReferenceNixpkgs; };
        # TODO: darwin
        # "x86_64-darwin" = { enableNixOSTests = false; };
      } (
        system: { isReferenceTarget ? false }:
          let
            pkgs = import ./. { inherit system; nixpkgsSrc = sources.${nixpkgsSource}; };
          in
          {
            inherit (pkgs) arion tests;
          } // lib.optionalAttrs enableDoc {
            doc = pkgs.recurseIntoAttrs (import ../doc { inherit pkgs; });
          } // lib.optionalAttrs isReferenceTarget {
            inherit (pkgs.arion-project.haskellPkgs) arion-compose-checked;
          }
      )
  )
