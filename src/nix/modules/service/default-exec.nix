{ config, lib, ... }:
let
  inherit (lib) types mkOption;
in
{
  options = {
    service.defaultExec = mkOption {
      type = types.listOf types.str;
      default = ["/bin/sh"];
      description = ''
        Container program and arguments to invoke when calling
        `arion exec <service.name>` without further arguments.
      '';
    };
  };
  config = {
    out.extendedInfo.defaultExec = config.service.defaultExec;
  };
}