self: super: {
  arion = super.callPackage ../arion.nix {};
  tests = super.callPackage ../tests {};
}
