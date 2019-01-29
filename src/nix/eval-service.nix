{ lib, pkgs, ... }:

{ modules, uid }:
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
  composite
