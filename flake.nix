{
  description = "Arion - use Docker Compose via Nix";

  outputs = { self, nixpkgs }:
  let
    lib = import (nixpkgs + "/lib");
    systems = [
      "aarch64-linux"
      "x86_64-darwin"
      "x86_64-linux"
    ];
    arionFromPkgs = pkgs: import ./nix/arion.nix { inherit pkgs; };
  in {

    # The overlay is currently the recommended way to integrate arion,
    # because its arion attribute behaves just like Nixpkgs.
    overlay = final: prev: {
      arion = arionFromPkgs final;
    };

    packages = lib.genAttrs systems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      arion = arionFromPkgs pkgs;
    });

    # Does not include the eval and build functions like you may expect from Nixpkgs.
    defaultPackage = lib.genAttrs systems (system:
      self.packages.${system}.arion
    );

    lib = {
      eval = import ./src/nix/eval-composition.nix;
      build = args@{...}:
        let composition = self.lib.eval args;
        in composition.config.out.dockerComposeYaml;
    };

    nixosModules.arion = ./nixos-module.nix;

  };
}
