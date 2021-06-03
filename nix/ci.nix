let
  sources = import ./sources.nix;
  lib = import (sources."nixos-unstable" + "/lib");
  inherit (import (sources."project.nix" + "/lib/dimension.nix") { inherit lib; }) dimension;
in

dimension "Nixpkgs version" {
    "nixos-20_09" = {
      nixpkgsSource = "nixos-20.09";
      isReferenceNixpkgs = true;
      enableDoc = true;
      dockerSupportsSystemd = true;
      nixosHasPodmanDockerSocket = false;
    };
    "nixos-21_05" = {
      nixpkgsSource = "nixos-21.05";
      enableDoc = true;
    };
    "nixos-unstable" = {
      nixpkgsSource = "nixos-unstable";
      enableDoc = true;
    };
  } (
    _name: { nixpkgsSource, isReferenceNixpkgs ? false, enableDoc ? true,
             dockerSupportsSystemd ? false, nixosHasPodmanDockerSocket ? true }:


      dimension "System" {
        "x86_64-linux" = { isReferenceTarget = isReferenceNixpkgs; };
        "x86_64-darwin" = { enableNixOSTests = false; };
      } (
        system: { isReferenceTarget ? false, enableNixOSTests ? true }:
          let
            pkgs = import ./. {
              inherit system dockerSupportsSystemd nixosHasPodmanDockerSocket;
              nixpkgsSrc = sources.${nixpkgsSource};
            };
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
