{ stdenv, lib
, coreutils, docker_compose, jq
}:
let

  # TODO: Replace by a new expression for the new Haskell main
  arion = stdenv.mkDerivation {
    name = "arion";
    src = ./src;
    unpackPhase = "";
    buildPhase = "";
    installPhase = ''
      mkdir -p $out/bin $out/share/arion
      cp -a nix $out/share/arion/
      cp -a arion-image $out/share/arion/
      tar -czf $out/share/arion/arion-image/tarball.tar.gz -C arion-image/tarball .
      substitute arion $out/bin/arion \
        --subst-var-by path ${lib.makeBinPath [jq coreutils docker_compose]} \
        --subst-var-by nix_dir $out/share/arion/nix \
        ;
      chmod a+x $out/bin/arion
    '';
    inherit passthru;
  };

  passthru = {
    inherit eval build;
  };

  eval = import "${nix_dir}/eval-composition.nix";

  build = args@{...}:
    let composition = eval args;
    in composition.config.build.dockerComposeYaml;

  nix_dir = "${arion.outPath}/share/arion/nix";

in
  arion
