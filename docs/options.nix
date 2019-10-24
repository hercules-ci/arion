{ pkgs ? import ../nix {} }:

let
  eval = import (pkgs.path + "/nixos/lib/eval-config.nix") {
    baseModules = import ../src/nix/module-composition.nix;
    modules = [];
  };
  options = pkgs.nixosOptionsDoc {
    options = eval.options;
  };

in pkgs.writeText "agent-options" ''
  = Arion options

  ${options.optionsAsciiDoc}
''
