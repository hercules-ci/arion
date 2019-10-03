

# This module is subject to change.
# In particular, arion-base should use a generic non-service image building system

{ config, lib, pkgs, ... }: 

let

  tag = lib.head (lib.strings.splitString "-" (baseNameOf builtImage.outPath));
  name = "arion-base";

  builtImage = pkgs.dockerTools.buildLayeredImage {
    inherit name;
    contents = pkgs.runCommand "minimal-contents" {} ''
        mkdir -p $out/bin $out/usr/bin
        ln -s /run/system/bin/sh $out/bin/sh
        ln -s /run/system/usr/bin/env $out/usr/bin/env
      '';
    config = {};
  };

in

{

  options = {
    arionBaseImage = lib.mkOption {
      type = lib.types.str;
      description = "Image to use when using useHostStore. Don't use this option yourself. It's going away.";
      internal = true;
    };
  };

  config = {
    arionBaseImage = "${name}:${tag}";
    build.imagesToLoad = lib.mkIf (lib.any (s: s.service.useHostStore) (lib.attrValues config.services)) [
      { image = builtImage; imageName = name; imageTag = tag; }
    ];  
  };
}