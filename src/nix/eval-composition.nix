{ modules ? [], uid ? "0", pkgs, hostNixStorePrefix ? "", }:

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
    modules = builtinModules ++ modules;
  };

  builtinModules = [
    argsModule
  ] ++ import ./modules.nix;

  argsModule = {
    _file = ./eval-composition.nix;
    key = ./eval-composition.nix;
    config._module.args.pkgs = lib.mkIf (pkgs != null) (lib.mkForce pkgs);
    config._module.args.check = true;
    config.host.nixStorePrefix = hostNixStorePrefix;
    config.host.uid = lib.toInt uid;
  };

in
  # Typically you need composition.config.out.dockerComposeYaml
  composition // {
    # throw in lib and pkgs for repl convenience
    inherit lib;
    inherit (composition._module.args) pkgs;
  }
