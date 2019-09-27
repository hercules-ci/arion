# to update: $ nix-prefetch-url --unpack url
builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/bd5e8f35c2e9d1ddc9cd2fea7a23563336d54acb.tar.gz";
  sha256 = "1wnzqqijrwf797nb234q10zb1h7086njradkkrx3a15b303grsw4";
}
