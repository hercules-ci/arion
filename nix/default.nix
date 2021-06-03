{ sources ? import ./sources.nix
, nixpkgsName ? "nixos-unstable" # match ./ci.nix isReferenceNixpkgs
, nixpkgsSrc ? sources.${nixpkgsName}
, system ? builtins.currentSystem
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
      arionTestingFlags = {
        inherit dockerSupportsSystemd nixosHasPodmanDockerSocket;
      };
    })
    (import ./overlay.nix)
  ];
  inherit system;
})
