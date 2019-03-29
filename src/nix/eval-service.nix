{ lib, pkgs, ... }:

{ modules, host, name }:
let
  composite = lib.evalModules {
    check = true;
    modules = builtinModules ++ modules;
  };

  builtinModules = [
    argsModule
    ./modules/service/default-exec.nix
    ./modules/service/docker-compose-service.nix
    ./modules/service/extended-info.nix
    ./modules/service/host-store.nix
    ./modules/service/host.nix
    ./modules/service/image.nix
    ./modules/service/nixos.nix
    ./modules/service/nixos-init.nix
  ];

  argsModule = {
    _file = ./eval-service.nix;
    key = ./eval-service.nix;
    config._module.args.pkgs = lib.mkForce pkgs;
    config.host = host;
    config.service.name = name;
  };

in
  composite
