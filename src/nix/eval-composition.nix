{ modules ? [], uid ? 0, pkgs, customNixRootPath ? "", }:

let _pkgs = pkgs;
in
let
  pkgs = if builtins.typeOf _pkgs == "path"
         then import _pkgs
         else if builtins.typeOf _pkgs == "set"
         then _pkgs
         else builtins.abort "The pkgs argument must be an attribute set or a path to an attribute set.";

  inherit (pkgs) lib;

  composition = lib.evalModules {
    check = true;
    modules = builtinModules ++ modules;
  };

  builtinModules = [
    argsModule
    ./modules/composition/docker-compose.nix
  ];

  argsModule = {
    _file = ./eval-composition.nix;
    key = ./eval-composition.nix;
    config._module.args.pkgs = lib.mkIf (pkgs != null) (lib.mkForce pkgs);
    config._module.args.uid = uid;
    config._module.args.customNixRootPath = customNixRootPath;
  };

in
  # Typically you need composition.config.build.dockerComposeYaml
  composition
