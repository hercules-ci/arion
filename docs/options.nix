{ pkgs ? import ../nix {} }:

let
  eval = pkgs.lib.evalModules {
    modules = import ../src/nix/modules.nix;
  };
  options = pkgs.nixosOptionsDoc {
    options = eval.options;
  };

in (pkgs.runCommand "agent-options.adoc" { } ''
  cat >$out <<EOF
  = Arion options

  EOF
  cat ${options.optionsAsciiDoc} >>$out
'').overrideAttrs (o: {
  # Work around https://github.com/hercules-ci/hercules-ci-agent/issues/168
  allowSubstitutes = true;
})
