/*

   This is a composition-level module.

   It defines the low-level options that are read by arion, like
    - build.dockerComposeYaml

   It declares options like
    - docker-compose.services

 */
{ pkgs, lib, config, ... }:
let
  evalService = name: modules: pkgs.callPackage ../../eval-service.nix {} { inherit name modules; inherit (config) host; };

in
{
  options = {
    build.dockerComposeYaml = lib.mkOption {
      type = lib.types.package;
      description = "A derivation that produces a docker-compose.yaml file for this composition.";
      readOnly = true;
    };
    build.dockerComposeYamlText = lib.mkOption {
      type = lib.types.string;
      description = "The text of build.dockerComposeYaml.";
      readOnly = true;
    };
    build.dockerComposeYamlAttrs = lib.mkOption {
      type = lib.types.attrsOf lib.types.unspecified;
      description = "The text of build.dockerComposeYaml.";
      readOnly = true;
    };
    docker-compose.raw = lib.mkOption {
      type = lib.types.attrs;
      description = "Attribute set that will be turned into the docker-compose.yaml file, using Nix's toJSON builtin.";
    };
    docker-compose.extended = lib.mkOption {
      type = lib.types.attrs;
      description = "Attribute set that will be turned into the x-arion section of the docker-compose.yaml file.";
    };
    docker-compose.services = lib.mkOption {
      default = {};
      type = with lib.types; attrsOf (coercedTo unspecified (a: [a]) (listOf unspecified));
      description = "A attribute set of service configurations. A service specifies how to run an image. Each of these service configurations is specified using modules whose options are described in the Service Options section.";
    };
    docker-compose.evaluatedServices = lib.mkOption {
      type = lib.types.attrsOf lib.types.attrs;
      description = "Attribute set of evaluated service configurations.";
      readOnly = true;
    };
  };
  config = {
    build.dockerComposeYaml = pkgs.writeText "docker-compose.yaml" config.build.dockerComposeYamlText;
    build.dockerComposeYamlText = builtins.toJSON (config.build.dockerComposeYamlAttrs);
    build.dockerComposeYamlAttrs = config.docker-compose.raw;

    docker-compose.evaluatedServices = lib.mapAttrs evalService config.docker-compose.services;
    docker-compose.raw = {
      version = "3.4";
      services = lib.mapAttrs (k: c: c.config.build.service) config.docker-compose.evaluatedServices;
      x-arion = config.docker-compose.extended;
    };
  };
}
