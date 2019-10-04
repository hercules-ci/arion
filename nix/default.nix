/**
 * This is the entry-point for all nix execution in this project.
 */
{ sources ? import ./sources.nix
, nixpkgsSrc ? sources.nixpkgs
, system ? null
, ...
}:

import nixpkgsSrc ({
  # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
  config = {
  };
  overlays = [
    # all the packages are defined there:
    (import ./overlay.nix)
  ];
} // (if system == null then {} else {
  inherit system;
}))
