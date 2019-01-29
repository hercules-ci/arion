/*

   This is a composition-level module.

   It defines the low-level options that are read by arion, like
    - build.dockerComposeYaml

   It declares options like
    - docker-compose.services

 */
{ pkgs, uid, lib, config, ... }:

let
  evalService = name: modules: (pkgs.callPackage ./eval-service.nix {} { inherit modules uid; }).config.build.service;

in
{
  options = {
    build.dockerComposeYaml = lib.mkOption {
      type = lib.types.package;
      description = "";
    };
    build.dockerComposeYamlText = lib.mkOption {
      type = lib.types.string;
      description = "";
    };
    docker-compose.raw = lib.mkOption {
      type = lib.types.attrs;
      description = "";
    };
    docker-compose.services = lib.mkOption {
      default = {};
      type = with lib.types; attrsOf (coercedTo unspecified (a: [a]) (listOf unspecified));
      description = "";
    };
  };
  config = {
    build.dockerComposeYaml = pkgs.writeText "docker-compose.yaml" config.build.dockerComposeYamlText;
    build.dockerComposeYamlText = builtins.toJSON (config.docker-compose.raw);

    docker-compose.raw = {
      version = "3";
      services = lib.mapAttrs evalService config.docker-compose.services;
    };
  };
}
