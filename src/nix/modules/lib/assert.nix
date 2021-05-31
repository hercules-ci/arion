{ config, lib, pkgs, ... }:

# based on nixpkgs/nixos/modules/system/activation/top-level.nix

let
  inherit (lib)
    concatStringsSep
    filter
    mkOption
    showWarnings
    types
    ;

  # Handle assertions and warnings
  failedAssertions = map (x: x.message) (filter (x: !x.assertion) config.assertions);

  assertWarn = if failedAssertions != []
    then throw "\nFailed assertions:\n${concatStringsSep "\n" (map (x: "- ${x}") failedAssertions)}"
    else showWarnings config.warnings;

in

{
  imports = [ ./assertions.nix ];
  options.assertWarn = mkOption {
    type = types.unspecified; # a function
    # It's for the wrapping program to know about this. User need not care.
    internal = true;
    readOnly = true;
  };
  config = { inherit assertWarn; };
}

