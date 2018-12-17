{ pkgs, uid, lib, config, ... }:

let
  evalService = name: modules:
    let
      composite = lib.evalModules {
        check = true;
        modules = builtinModules ++ modules;
      };

      builtinModules = [
        argsModule
        ./service.nix
        ./service-host-store.nix
      ];

      argsModule = {
        _file = ./docker-compose-module.nix;
        key = ./docker-compose-module.nix;
        config._module.args.pkgs = lib.mkForce pkgs;
        config._module.args.uid = uid;
      };

    in
      composite.config.build.service;

in
{
  options = {
    build.dockerComposeYaml = lib.mkOption {
      type = lib.types.package;
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
    build.dockerComposeYaml = pkgs.writeText "docker-compose.yaml" (builtins.toJSON (config.docker-compose.raw));

    docker-compose.raw = {
      version = "3";
      services = lib.mapAttrs evalService config.docker-compose.services;
    };
  };
}
