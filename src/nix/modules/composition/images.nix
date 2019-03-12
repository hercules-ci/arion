{ pkgs, lib, config, ... }:
let
  inherit (lib.types) listOf package unspecified;

  serviceImages =
    lib.mapAttrs addDetails (
      lib.filterAttrs filterFunction config.docker-compose.evaluatedServices
    );

  filterFunction = _serviceName: service:
    service.config.image.nixBuild;

  addDetails = _serviceName: service:
    let
      inherit (service.config) build;
    in {
      image = build.image.outPath;
      imageName = build.imageName or service.image.name;
      imageTag =
                 if build.image.imageTag != ""
                 then build.image.imageTag
                 else lib.head (lib.strings.splitString "-" (baseNameOf build.image.outPath));
    };
in
{
  options = {
    build.imagesToLoad = lib.mkOption {
      type = listOf unspecified;
      description = "List of dockerTools image derivations.";
    };
  };
  config = {
    build.imagesToLoad = lib.attrValues serviceImages;
    docker-compose.raw.x-arion.images = config.build.imagesToLoad;
  };
}
