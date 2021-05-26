{ config, lib, name, ... }:
let
  inherit (lib)
    concatStringsSep
    optional
    ;

  dynamicUserServices = lib.attrNames (
    lib.filterAttrs
      (k: v:
        v.enable &&
        v.serviceConfig.DynamicUser or false)
      config.nixos.evaluatedConfig.systemd.services
    );

  
in
{
  config = {
    warnings = 
      optional (config.nixos.useSystemd && !(config.service.capabilities.SYS_ADMIN or false) && dynamicUserServices != []) (
        ''In service ${name}, the following units require `SYS_ADMIN` capability
          because of DynamicUser.
          ${concatStringsSep "\n" (map (srv: "    - services.${name}.nixos.configuration.systemd.services.${srv}") dynamicUserServices)}
          You can avoid DynamicUser or use
              services.${name}.service.capabilities.SYS_ADMIN = true;
        ''
      );
  };
}