{ config, lib, pkgs, ... }:
let
  inherit (lib)
    mkIf
    mkOption
    types
    ;
  inherit (types)
    bool
    ;

  recommendedContents = { runCommand, bash, coreutils }:
    runCommand "recommended-contents" {} ''
      mkdir -p $out/bin $out/usr/bin $out/var/empty
      ln -s ${bash}/bin/sh $out/bin/sh
      ln -s ${coreutils}/bin/env $out/usr/bin/env
    '';
in
{
  options = {
    image.enableRecommendedContents = mkOption {
      type = bool;
      default = false;
      description = ''
        Add the `/bin/sh` and `/usr/bin/env` symlinks and some lightweight
        files.
      '';
    };
  };

  config = mkIf config.image.enableRecommendedContents {
    image.contents = [
      (pkgs.callPackage recommendedContents {})
    ];
    image.rawConfig.Env = {
      "PATH" = lib.mkDefault "/run/current-system/sw/bin:/bin:/usr/bin:/usr/local/bin";
    };
  };
}
