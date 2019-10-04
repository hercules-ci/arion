/*

   This is a composition-level module.

   It defines the low-level options that are read by arion, like
    - out.dockerComposeYaml

   It declares options like
    - services

 */
compositionArgs@{ lib, config, options, pkgs, ... }:
let
  inherit (lib) types;

  service = {
    imports = [ argsModule ] ++ import ../service/all-modules.nix;
  };
  argsModule =
    { name, # injected by types.submodule
      ...
    }: {
      _file = ./docker-compose.nix;
      key = ./docker-compose.nix;

      config._module.args.pkgs = lib.mkDefault compositionArgs.pkgs;
      config.host = compositionArgs.config.host;
      config.composition = compositionArgs.config;
      config.service.name = name;
    };

in
{
  imports = [
    ../lib/assert.nix
    (lib.mkRenamedOptionModule ["docker-compose" "services"] ["services"])
  ];
  options = {
    out.dockerComposeYaml = lib.mkOption {
      type = lib.types.package;
      description = "A derivation that produces a docker-compose.yaml file for this composition.";
      readOnly = true;
    };
    out.dockerComposeYamlText = lib.mkOption {
      type = lib.types.str;
      description = "The text of out.dockerComposeYaml.";
      readOnly = true;
    };
    out.dockerComposeYamlAttrs = lib.mkOption {
      type = lib.types.attrsOf lib.types.unspecified;
      description = "The text of out.dockerComposeYaml.";
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
    services = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule service);
      description = "An attribute set of service configurations. A service specifies how to run an image as a container.";
    };
  };
  config = {
    out.dockerComposeYaml = pkgs.writeText "docker-compose.yaml" config.out.dockerComposeYamlText;
    out.dockerComposeYamlText = builtins.toJSON (config.out.dockerComposeYamlAttrs);
    out.dockerComposeYamlAttrs = config.assertWarn config.docker-compose.raw;

    docker-compose.raw = {
      version = "3.4";
      services = lib.mapAttrs (k: c: c.out.service) config.services;
      x-arion = config.docker-compose.extended;
    };
  };
}
