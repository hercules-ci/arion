{ pkgs, lib, config, options, ... }:
let
  inherit (lib) types mkOption;
  inherit (types) attrsOf listOf nullOr package str unspecified bool;

  # TODO: dummy-config is a useless layer. Nix 2.3 will let us inspect
  #       the string context instead, so we can avoid this.
  contentsList = config.image.contents ++ [
    (pkgs.writeText "dummy-config.json" (builtins.toJSON config.image.rawConfig))
  ];

  builtImage = pkgs.dockerTools.buildLayeredImage {
    inherit (config.image)
      name
      contents
      ;
    config = config.image.rawConfig;
    maxLayers = 100;

    # TODO: allow use of image's Nix package instead
    # TODO: option to disable db generation
    extraCommands = ''
        echo "Generating the nix database..."
        echo "Warning: only the database of the deepest Nix layer is loaded."
        echo "         If you want to use nix commands in the container, it would"
        echo "         be better to only have one layer that contains a nix store."
        export NIX_REMOTE=local?root=$PWD
        ${pkgs.nix}/bin/nix-store --load-db < ${pkgs.closureInfo {rootPaths = contentsList;}}/registration
        mkdir -p nix/var/nix/gcroots/docker/
        for i in ${lib.concatStringsSep " " contentsList}; do
          ln -s $i nix/var/nix/gcroots/docker/$(basename $i)
        done;
      '';
  };

  priorityIsDefault = option: option.highestPrio >= (lib.mkDefault true).priority;
in
{
  options = {
    build.image = mkOption {
      type = nullOr package;
      description = ''
        Docker image derivation to be `docker load`ed.

        By default, when `services.<name>.image.nixBuild` is enabled, this is
        the image produced using `services.<name>.image.command`,
        `services.<name>.image.contents`, and
        `services.<name>.image.rawConfig`.
      '';
      defaultText = lib.literalExample ''
        pkgs.dockerTools.buildLayeredImage { ... };
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
        `dockerTools.buildLayeredImage`
        and then load it with `docker load`.

        By default, an image will be built with Nix unless `service.image`
        is set. See also `image.name`, which defaults to
        the service name.
      '';
    };
    image.name = mkOption {
      type = str;
      default = config.service.name;
      defaultText = lib.literalExample "config.service.name";
      description = ''
        A human readable name for the docker image.

        Shows up in the `docker ps` output in the
        `IMAGE` column, among other places.
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
        Please use the specific `image` options instead.

        Run-time configuration of the container. A full list of the
        options is available in the https://github.com/moby/moby/blob/master/image/spec/v1.2.md#image-json-field-descriptions[Docker Image Specification
        v1.2.0].
      '';
    };
    image.command = mkOption {
      type = listOf str;
      default = [];
      description = ''
      '';
    };
    image.drv = mkOption {
      type = nullOr package;
      inherit (options.build.image) description defaultText;
      example = lib.literalExample ''
        let
          myimage = pkgs.dockerTools.buildImage {
            name = "my-image";
            contents = [ pkgs.coreutils ];
          };
        in
        config.services = {
          myservice = {
            image.drv = myimage;
            # ...
          };
        }
      '';
    };
  };
  config = {
    build.image = config.image.drv or builtImage;
    build.imageName = config.build.image.imageName;
    build.imageTag =
                 if config.build.image.imageTag != ""
                 then config.build.image.imageTag
                 else lib.head (lib.strings.splitString "-" (baseNameOf config.build.image.outPath));

    service.image = lib.mkDefault "${config.build.imageName}:${config.build.imageTag}";
    image.rawConfig.Cmd = config.image.command;

    image.nixBuild = lib.mkDefault (priorityIsDefault options.service.image);
  };
}
