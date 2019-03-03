self: super: {
  arion = super.callPackage ../arion.nix {};
  tests = super.callPackage ../tests {};
  doc = super.callPackage ../doc {};
}
