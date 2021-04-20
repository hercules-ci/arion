{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;

  outputs = { self, nixpkgs, ... }: {

    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    # # alternative:
    # pkgs = import nixpkgs { config = { }; overlays = [ ]; system = "x86_64-linux"; };

    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.hello;

  };
}
