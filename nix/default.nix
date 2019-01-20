/**
 * This is the entry-point for all nix execution in this project.
 */
{ nixpkgsSrc ? ./nixpkgs.nix, ... }:
import (import ./nixpkgs.nix) {
  # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
  config = {
  };
  overlays = [
    # all the packages are defined there:
    (import ./overlay.nix)
  ];
}
