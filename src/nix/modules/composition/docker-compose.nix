/*

   This is a composition-level module.

   It defines the low-level options that are read by arion, like
    - build.dockerComposeYaml

   It declares options like
    - docker-compose.services

 */
{ pkgs, lib, config, ... }:

let
  evalService = name: modules:
    let
      composite = lib.evalModules {
        check = true;
        modules = builtinModules ++ modules;
      };

      builtinModules = [
        argsModule
        ../service/docker-compose-service.nix
        ../service/host-store.nix
        ../service/host.nix
      ];

      argsModule = {
        _file = ./docker-compose.nix;
        key = ./docker-compose.nix;
        config._module.args.pkgs = lib.mkForce pkgs;
        config.host = config.host;
      };

    in
      composite.config.build.service;

in
{
  options = {
    build.dockerComposeYaml = lib.mkOption {
      type = lib.types.package;
    };
    build.dockerComposeYamlText = lib.mkOption {
      type = lib.types.string;
    };
    docker-compose.raw = lib.mkOption {
      type = lib.types.attrs;
    };
    docker-compose.services = lib.mkOption {
      default = {};
      type = with lib.types; attrsOf (coercedTo unspecified (a: [a]) (listOf unspecified));
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
