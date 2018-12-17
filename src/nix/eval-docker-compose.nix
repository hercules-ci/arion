{ modules ? [], uid ? 0, pkgs }:

let _pkgs = pkgs;
in
let
  pkgs = if builtins.typeOf _pkgs == "path"
         then import _pkgs
         else if builtins.typeOf _pkgs == "set"
         then _pkgs
         else builtins.abort "The pkgs argument must be an attribute set or a path to an attribute set.";

  inherit (pkgs) lib;

  composite = lib.evalModules {
    check = true;
    modules = builtinModules ++ modules;
  };

  builtinModules = [
    argsModule
    ./docker-compose-module.nix
  ];

  argsModule = {
    _file = ./eval-docker-compose.nix;
    key = ./eval-docker-compose.nix;
    config._module.args.pkgs = lib.mkIf (pkgs != null) (lib.mkForce pkgs);
    config._module.args.uid = uid;
  };

in
  # Typically you need composite.config.build.dockerComposeYaml
  composite
