{ sources ? import ./sources.nix
, nixpkgsName ? "nixos-20.03"
, nixpkgsSrc ? sources.${nixpkgsName}
, system ? builtins.currentSystem
, nixosTestIsPerl ? false
, ...
}:

import nixpkgsSrc ({
  # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
  config = {
  };
  overlays = [
    # all the packages are defined there:
    (_: _: { inherit nixosTestIsPerl; })
    (import ./overlay.nix)
  ];
  inherit system;
})
