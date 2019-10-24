{ sources ? import ./sources.nix
, nixpkgsName ? "nixos-19.09"
, nixpkgsSrc ? sources.${nixpkgsName}
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
