self: super: hself: hsuper:
let
  inherit (self.haskell.lib) addBuildTools overrideCabal;
in
{
  arion-compose = overrideCabal (addBuildTools (hself.callCabal2nix "arion-compose" ./.. {}) [self.nix]) (o: o // {
    preCheck = ''
      export NIX_LOG_DIR=$TMPDIR
      export NIX_STATE_DIR=$TMPDIR
      export NIX_PATH=nixpkgs=${self.path}
    '';
  });
}