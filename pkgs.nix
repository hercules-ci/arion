import <nixpkgs> {
  overlays = [ (self: super: { arion = super.callPackage ./arion.nix {}; }) ];
}
