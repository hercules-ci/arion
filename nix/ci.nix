let
  sources = import ./sources.nix;
  lib = import (sources."nixpkgs" + "/lib");
  inherit (import sources."project.nix" { inherit lib; }) dimension;
in

dimension "Nixpkgs version" {
    "nixos-19_03" = {
      nixpkgsSource = "nixpkgs";
      isReferenceNixpkgs = true;
      enableDoc = false;
    };
    "nixos-19_09" = {
      nixpkgsSource = "nixos-19.09";
      enableDoc = true;
    };
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";
      enableDoc = true;
    };
  } (
    _name: { nixpkgsSource, isReferenceNixpkgs ? false, enableDoc ? true }:


      dimension "System" {
        "x86_64-linux" = { isReferenceTarget = isReferenceNixpkgs; };
        "x86_64-darwin" = { enableNixOSTests = false; };
      } (
        system: { isReferenceTarget ? false, enableNixOSTests ? true }:
          let
            pkgs = import ./. { inherit system; nixpkgsSrc = sources.${nixpkgsSource}; };
          in
          {
            inherit (pkgs) arion;
          } // lib.optionalAttrs enableNixOSTests {
            inherit (pkgs) tests;
          } // lib.optionalAttrs enableDoc {
            inherit (pkgs) doc doc-options doc-options-check;
          } // lib.optionalAttrs isReferenceTarget {
            inherit (pkgs.arion-project.haskellPkgs) arion-compose-checked;
          }
      )
  )
