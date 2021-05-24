

# This module is subject to change.
# In particular, arion-base should use a generic non-service image building system

{ config, lib, pkgs, ... }: 

let
  name = "arion-base";

  imageExe = pkgs.dockerTools.streamLayeredImage {
    inherit name;
    contents = pkgs.runCommand "minimal-contents" {} ''
        mkdir -p $out/bin $out/usr/bin
        ln -s /run/system/bin/sh $out/bin/sh
        ln -s /run/system/usr/bin/env $out/usr/bin/env
      '';
    config = {};
  };
  inherit (imageExe) imageTag;

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
    arionBaseImage = "${name}:${imageTag}";
    build.imagesToLoad = lib.mkIf (lib.any (s: s.service.useHostStore) (lib.attrValues config.services)) [
      { imageExe = imageExe; imageName = name; inherit imageTag; }
    ];  
  };
}