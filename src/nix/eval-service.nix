{ lib, pkgs, ... }:

{ modules, host, name }:
let
  composite = lib.evalModules {
    check = true;
    modules = builtinModules ++ modules;
  };

  builtinModules = [
    argsModule
    ./modules/service/docker-compose-service.nix
    ./modules/service/host-store.nix
    ./modules/service/host.nix
    ./modules/service/nixos.nix
    ./modules/service/nixos-init.nix
  ];

  argsModule = {
    _file = ./docker-compose.nix;
    key = ./docker-compose.nix;
    config._module.args.pkgs = lib.mkForce pkgs;
    config.host = host;
    config.service.name = name;
  };

in
  composite
