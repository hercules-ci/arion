(import
  (
    let lock = builtins.fromJSON (builtins.readFile ./flake.lock); in
    fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/009399224d5e398d03b22badca40a37ac85412a1.tar.gz";
      sha256 = "sha256:0xcr9fibnapa12ywzcnlf54wrmbqqb96fmmv8043zhsycws7bpqy";
    }
  )
  { src = ../.; }
).defaultNix
