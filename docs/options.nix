{ pkgs ? import ../nix {} }:

let
  eval = pkgs.lib.evalModules {
    modules = import ../src/nix/modules.nix;
  };
  options = pkgs.nixosOptionsDoc {
    options = eval.options;
  };

in (pkgs.writeText "agent-options" ''
  = Arion options

  ${options.optionsAsciiDoc}
'').overrideAttrs (o: {
  # Work around https://github.com/hercules-ci/hercules-ci-agent/issues/168
  allowSubstitutes = true;
})
