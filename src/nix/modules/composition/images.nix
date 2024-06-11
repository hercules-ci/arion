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
      (
        let
          imageAttrName = "image${lib.optionalString (service.image.tarball.isExe or false) "Exe"}";
        in
        {
          inherit (service.image.tarball) imageName imageTag;
          ${imageAttrName} = service.image.tarball.outPath;
        }
      );
in
{
  options = {
    build.imagesToLoad = lib.mkOption {
      type = listOf unspecified;
      internal = true;
      description = "List of `dockerTools` image derivations.";
    };
  };
  config = {
    assertions =
      let
        assertionsForRepoTagComponent = component: attrName:
          lib.mapAttrsToList
            (name: value: {
              assertion = lib.types.nonEmptyStr.check value.${attrName};
              message = lib.replaceStrings [ "\n" ] [ " " ] ''
                Unable to infer the ${component} of the image associated with
                config.services.${name}.  Please set
                config.services.${name}.image.${attrName} to a non-empty
                string.
              '';
            })
            serviceImages;

        nameAssertions = assertionsForRepoTagComponent "name" "imageName";
        tagAssertions = assertionsForRepoTagComponent "tag" "imageTag";
      in
      nameAssertions ++ tagAssertions;

    build.imagesToLoad = lib.attrValues serviceImages;
    docker-compose.extended.images = config.build.imagesToLoad;
  };
}
