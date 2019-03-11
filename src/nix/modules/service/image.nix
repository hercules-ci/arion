{ pkgs, lib, config, ... }:
let
  inherit (lib) types mkOption;
  inherit (types) attrsOf listOf nullOr package str unspecified bool;
in
{
  options = {
    build.image = mkOption {
      type = nullOr package;
      description = ''
        Docker image derivation to be <code>docker load</code>ed.
      '';
      internal = true;
    };
    build.imageName = mkOption {
      type = str;
      description = "Derived from build.image";
      internal = true;
    };
    build.imageTag = mkOption {
      type = str;
      description = "Derived from build.image";
      internal = true;
    };
    image.nixBuild = mkOption {
      type = bool;
      description = ''
        Whether to build this image with Nixpkgs'
        <code>dockerTools.buildLayeredImage</code>
        and then load it with <code>docker load</code>.
      '';
      default = true;
    };
    image.name = mkOption {
      type = str;
      default = config.service.name;
      defaultText = lib.literalExample "config.service.name";
      description = ''
        A human readable name for the docker image.

        Shows up in the <code>docker ps</code> output in the
        <code>IMAGE</code> column, among other places.
      '';
    };
    image.contents = mkOption {
      type = listOf package;
      default = [];
      description = ''
         Top level paths in the container.
      '';
    };
    image.rawConfig = mkOption {
      type = attrsOf unspecified;
      default = {};
      description = ''
        This is a low-level fallback for when a container option has not
        been modeled in the Arion module system.

        This attribute set does not have an appropriate merge function.
        Please use the specific <code>image</code> options instead.

        Run-time configuration of the container. A full list of the
        options are available at in the <link xlink:href="https://github.com/moby/moby/blob/master/image/spec/v1.2.md#image-json-field-descriptions">Docker Image Specification
        v1.2.0</link>.
      '';
    };
    image.command = mkOption {
      type = listOf str;
      default = [];
      description = ''
      '';
    };
  };
  config = {
    build.image = pkgs.dockerTools.buildLayeredImage {
      inherit (config.image)
              name
              contents
      ;
      config = config.image.rawConfig;
    };
    build.imageName = config.build.image.imageName;
    build.imageTag =
                 if config.build.image.imageTag != ""
                 then config.build.image.imageTag
                 else lib.head (lib.strings.splitString "-" (baseNameOf config.build.image.outPath));

    service.image = lib.mkDefault "${config.build.imageName}:${config.build.imageTag}";
    image.rawConfig.Cmd = config.image.command;
  };
}
