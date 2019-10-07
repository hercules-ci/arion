{ sources ? import ./sources.nix
, nixpkgsSrc ? sources.nixpkgs
, system ? builtins.currentSystem
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
  inherit system;
})
