{ sources ? import ./sources.nix
, nixpkgsName ? "nixos-unstable"
, nixpkgsSrc ? sources.${nixpkgsName}
, system ? builtins.currentSystem
, nixosTestIsPerl ? false
, dockerSupportsSystemd ? false
, nixosHasPodmanDockerSocket ? true
, ...
}:

import nixpkgsSrc ({
  # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
  config = {
  };
  overlays = [
    (_: _: {
      inherit nixosTestIsPerl;
      arionTestingFlags = {
        inherit nixosTestIsPerl dockerSupportsSystemd nixosHasPodmanDockerSocket;
      };
    })
    (import ./overlay.nix)
  ];
  inherit system;
})
