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
  in {

    packages = lib.genAttrs systems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      arion = import ./nix/arion.nix { inherit pkgs; };
    });

    defaultPackage = lib.genAttrs systems (system:
      self.packages.${system}.arion
    );

  };
}
