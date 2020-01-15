{ pkgs, lib, config, options, ... }:
let
  inherit (lib)
    all
    flip
    functionArgs
    hasAttr
    isDerivation
    mkOption
    optionalAttrs
    types
    warn
    ;
  inherit (pkgs)
    dockerTools
    ;
  inherit (types)
    addCheck
    attrs
    attrsOf
    bool
    coercedTo
    listOf
    nullOr
    oneOf
    package
    path
    str
    unspecified
    ;

  # TODO: dummy-config is a useless layer. Nix 2.3 will let us inspect
  #       the string context instead, so we can avoid this.
  contentsList = config.image.contents ++ [
    (pkgs.writeText "dummy-config.json" (builtins.toJSON config.image.rawConfig))
  ];

  # Shim for `services.<name>.image.tarball` definitions that refer to
  # arbitrary paths and not `dockerTools`-produced derivations.
  dummyImagePackage = outPath: {
    inherit outPath;
    type = "derivation";
    imageName = config.image.name;
    imageTag = if config.image.tag == null then "" else config.image.tag;
  };

  # Type matching the essential attributes of derivations produced by
  # `dockerTools` builder functions.
  imagePackageType = addCheck attrs (x: isDerivation x && (all (flip hasAttr x) [ "imageName" "imageTag" ]));

  # `coercedTo path dummyImagePackage package` looks sufficient, but it is not.
  # `coercedTo` defines this `check` function:
  #
  #   x: (coercedType.check x && finalType.check (coerceFunc x)) || finalType.check x;
  #
  # and this `merge` function:
  #
  #   loc: defs:
  #     let
  #       coerceVal = val:
  #         if coercedType.check val then coerceFunc val
  #         else val;
  #     in finalType.merge loc (map (def: def // { value = coerceVal def.value; }) defs);
  #
  # Meaning that values that satisfy `finalType.check` may still be subject to
  # coercion.  In this case, derivations satisfy `path.check`, so will be
  # coerced using the `dummyImagePackage` function.  To avoid this unnecessary
  # coercion, we instead force checking whether the value satisfies
  # `imagePackageType.check` *first* via placing `imagePackageType` at the head
  # of the list provided to `oneOf`.
  imageTarballType = oneOf [ imagePackageType (coercedTo path dummyImagePackage imagePackageType) ];

  includeStorePathsWarningAndDefault = lib.warn ''
    You're using a version of Nixpkgs that doesn't support the includeStorePaths
    parameter in dockerTools.streamLayeredImage. Without this, Arion's
    useHostStore does not achieve the intended speedup.
  '' {};

  buildOrStreamLayeredImage = args:
    let
      args_base = builtins.intersectAttrs
        {
          name = null; tag = null; contents = null; config = null;
          created = null; extraCommands = null; maxLayers = null;
          fakeRootCommands = null;
        }
        args;
      acceptedArgs = functionArgs dockerTools.streamLayeredImage;
      args_no_store = lib.optionalAttrs (!(args.includeStorePaths or true)) (
        if acceptedArgs ? includeStorePaths
        then { inherit (args) includeStorePaths; }
        else includeStorePathsWarningAndDefault
      );
      args_streamLayered = args_base // args_no_store;
    in
      if dockerTools?streamLayeredImage
      then dockerTools.streamLayeredImage args_streamLayered // { isExe = true; }
      else dockerTools.buildLayeredImage args_base;

  builtImage = buildOrStreamLayeredImage {
    inherit (config.image)
      name
      tag
      contents
      includeStorePaths
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

    fakeRootCommands = config.image.fakeRootCommands;
  };

  priorityIsDefault = option: option.highestPrio >= (lib.mkDefault true).priority;
in
{
  options = {
    build.image = mkOption {
      type = nullOr package;
      description = ''
        Docker image derivation to be `docker load`-ed.

        By default, when `services.<name>.image.nixBuild` is enabled, this is
        the image produced using `services.<name>.image.command`,
        `services.<name>.image.contents`, and
        `services.<name>.image.rawConfig`.
      '';
      defaultText = lib.literalExample ''
        pkgs.dockerTools.buildLayeredImage {
          # ...
        };
      '';
      internal = true;
    };
    build.imageName = mkOption {
      type = str;
      description = "Derived from `build.image`";
      internal = true;
    };
    build.imageTag = mkOption {
      type = str;
      description = "Derived from `build.image`";
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
      default = "localhost/" + config.service.name;
      defaultText = lib.literalExpression or lib.literalExample ''"localhost/" + config.service.name'';
      description = ''
        A human readable name for the Docker image.

        Shows up in the `docker ps` output in the
        `IMAGE` column, among other places.

        ::: {.important}
        If you set {option}`services.<name>.image.tarball` to an arbitrary
        Docker image tarball (and not, say, a derivation produced by one of the
        [`dockerTools`](https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-dockerTools)
        builder functions), then you **must** set {option}`services.<name>.image.name`
        to the name of the image in the tarball.  Otherwise, Arion will not be
        able to properly identify the image in the generated Docker Compose
        configuration file.
        :::
      '';
    };
    image.tag = mkOption {
      type = nullOr str;
      default = null;
      description = ''
        A tag to assign to the built image, or (if you specified an image archive
        with `image.tarball`) the tag that arion should use when referring to
        the loaded image.

        ::: {.important}
        If you set {option}`services.<name>.image.tarball` to an arbitrary
        Docker image tarball (and not, say, a derivation produced by one of the
        [`dockerTools`](https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-dockerTools)
        builder functions), then you **must** set {option}`services.<name>.image.tag`
        to one of the tags associated with `services.<name>.image.name` in the
        image tarball.  Otherwise, Arion will not be able to properly identify
        the image in the generated Docker Compose configuration file.
        :::
      '';
    };
    image.contents = mkOption {
      type = listOf package;
      default = [];
      description = ''
         Top level paths in the container.
      '';
    };
    image.fakeRootCommands = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Commands that build the root of the container in the current working directory.

        See [`dockerTools.buildLayeredImage`](https://nixos.org/manual/nixpkgs/stable/#ssec-pkgs-dockerTools-buildLayeredImage).
      '';
    };
    image.includeStorePaths = mkOption {
      type = bool;
      default = true;
      internal = true;
      description = ''
        Include all referenced store paths. You generally want this in your
        image, unless you load store paths via some other means, like `useHostStore = true`;
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
        options is available in the [Docker Image Specification
        v1.2.0](https://github.com/moby/moby/blob/master/image/spec/v1.2.md#image-json-field-descriptions).
      '';
    };
    image.command = mkOption {
      type = listOf str;
      default = [];
      description = ''
      '';
    };
    image.tarball = mkOption {
      type = nullOr imageTarballType;
      default = builtImage;
      defaultText = "${builtins.storeDir}/image-built-from-service-configuration.tar.gz";
      description = ''
        Docker image tarball to be `docker load`-ed.  This can be a derivation
        produced with one of the [`dockerTools`](https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-dockerTools)
        builder functions, or a Docker image tarball at some arbitrary
        location.

        ::: {.note}
        Using this option causes Arion to ignore most other options in the
        {option}`services.<name>.image` namespace. The exceptions are
        {option}`services.<name>.image.name` and {option}`services.<name>.image.tag`,
        which are used when the provided {option}`services.<name>.image.tarball`
        is not a derivation with the attributes `imageName` and `imageTag`.
        :::
      '';
      example = lib.literalExample or lib.literalExpression ''
        let
          myimage = pkgs.dockerTools.buildImage {
            name = "my-image";
            contents = [ pkgs.coreutils ];
          };
        in
        config.services = {
          myservice = {
            image.tarball = myimage;
            # ...
          };
        }
      '';
    };
  };
  config = lib.mkMerge [{
      build.image = config.image.tarball;
      build.imageName = config.build.image.imageName;
      build.imageTag =
                   if config.build.image.imageTag != ""
                   then config.build.image.imageTag
                   else lib.head (lib.strings.splitString "-" (baseNameOf config.build.image.outPath));
      image.rawConfig.Cmd = config.image.command;
      image.nixBuild = lib.mkDefault (priorityIsDefault options.service.image);
    }
    ( lib.mkIf (config.service.build.context == null)
    {
      service.image = lib.mkDefault "${config.build.imageName}:${config.build.imageTag}";
    })
  ];
}
