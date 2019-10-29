/*
  Generic options for adding NixOS modules and evaluating the NixOS
  configuration. The methods for installing NixOS in the image are
  defined elsewhere.
 */
{ pkgs, lib, config, ... }:
let
  inherit (lib) types;
  inherit (types) attrs;
in
{
  options = {
    nixos.configuration = lib.mkOption {
      type = with types; coercedTo unspecified (a: [a]) (listOf unspecified);
      default = {};
      description = ''
        Modules to add to the NixOS configuration.

        This option is unused by default, because not all images use NixOS.

        One way to use this is to enable `nixos.useSystemd`, but the
        NixOS configuration can be used in other ways.
      '';
    };

    nixos.build = lib.mkOption {
      type = attrs;
      readOnly = true;
      description = ''
        NixOS build products from `config.system.build`, such as `toplevel` and `etc`.

        This option is unused by default, because not all images use NixOS.

        One way to use this is to enable `nixos.useSystemd`, but the
        NixOS configuration can be used in other ways.
      '';
    };

    nixos.evaluatedConfig = lib.mkOption {
      type = attrs;
      readOnly = true;
      description = ''
        Evaluated NixOS configuration, to be read by service-level modules.

        This option is unused by default, because not all images use NixOS.

        One way to use this is to enable `nixos.useSystemd`, but the
        NixOS configuration can be used in other ways.
      '';
    };
  };

  config = {
    nixos.build = builtins.addErrorContext "while evaluating the service nixos configuration" (
      pkgs.nixos config.nixos.configuration
    );
    nixos.configuration = { config, ... }: { system.build.theConfig = config; };
    nixos.evaluatedConfig = config.nixos.build.theConfig;
  };
}
