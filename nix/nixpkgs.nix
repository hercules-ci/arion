# to update: $ nix-prefetch-url --unpack url
builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/be445a9074f139d63e704fa82610d25456562c3d.tar.gz";
  sha256 = "15dc7gdspimavcwyw9nif4s59v79gk18rwsafylffs9m1ld2dxwa";
}
