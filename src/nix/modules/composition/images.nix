{ pkgs, lib, config, ... }:
let
  inherit (lib.types) listOf package unspecified;

  serviceImages =
    lib.mapAttrs addDetails (
      lib.filterAttrs filterFunction config.services
    );

  filterFunction = serviceName: service:
    builtins.addErrorContext "while evaluating whether the service ${serviceName} defines an image"
      service.image.nixBuild;

  addDetails = serviceName: service:
    builtins.addErrorContext "while evaluating the image for service ${serviceName}"
    (let
      inherit (service) build;
    in {
      imageName = build.imageName or service.image.name;
      imageTag =
                 if build.image.imageTag != ""
                 then build.image.imageTag
                 else lib.head (lib.strings.splitString "-" (baseNameOf build.image.outPath));
    } // (if build.image.isExe or false
        then {
          imageExe = build.image.outPath;
        }
        else {
          image = build.image.outPath;
        }
      )
    );
in
{
  options = {
    build.imagesToLoad = lib.mkOption {
      type = listOf unspecified;
      internal = true;
      description = "List of dockerTools image derivations.";
    };
  };
  config = {
    build.imagesToLoad = lib.attrValues serviceImages;
    docker-compose.extended.images = config.build.imagesToLoad;
  };
}
