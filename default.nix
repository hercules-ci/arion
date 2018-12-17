{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) lib stdenv;

  arion = stdenv.mkDerivation {
    name = "arion";
    src = ./src;
    unpackPhase = "";
    buildPhase = "";
    installPhase = ''
      mkdir -p $out/bin $out/share/arion
      cp -a nix $out/share/arion/
      cp -a arion-image $out/share/arion/
      substitute arion $out/bin/arion \
        --subst-var-by path ${lib.makeBinPath [pkgs.jq pkgs.coreutils]} \
        --subst-var-by nix_dir $out/share/arion/nix \
        ;
      chmod a+x $out/bin/arion
    '';
  };

in
{
  inherit arion;
}
