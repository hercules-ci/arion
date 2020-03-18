let
  sources = import ./sources.nix;
  lib = import (sources."nixpkgs" + "/lib");
  inherit (import (sources."project.nix" + "/lib/dimension.nix") { inherit lib; }) dimension;
in

dimension "Nixpkgs version" {
    "nixos-19_03" = {
      # flyingcircus.io latest long-term support is based off 19.03
      # https://flyingcircus.io/doc/
      # It is nice to have some level of support for their platform,
      # but we don't guarantee any support.
      nixpkgsSource = "nixos-19.03";
      enableDoc = false;
      nixosTestIsPerl = true;
    };
    "nixos-19_09" = {
      nixpkgsSource = "nixos-19.09";
      enableDoc = false;
      nixosTestIsPerl = true;
    };
    "nixos-20_03" = {
      nixpkgsSource = "nixos-20.03";
      isReferenceNixpkgs = true;
      enableDoc = true;
    };
    # "nixos-unstable" = {
    #   nixpkgsSource = "nixos-unstable";
    #   enableDoc = true;
    # };
  } (
    _name: { nixpkgsSource, isReferenceNixpkgs ? false, enableDoc ? true, nixosTestIsPerl ? false }:


      dimension "System" {
        "x86_64-linux" = { isReferenceTarget = isReferenceNixpkgs; };
        "x86_64-darwin" = { enableNixOSTests = false; };
      } (
        system: { isReferenceTarget ? false, enableNixOSTests ? true }:
          let
            pkgs = import ./. {
              inherit system nixosTestIsPerl;
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
