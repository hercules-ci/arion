/*

   This service-level bind mounts the host store into the container
   when the service.useHostStore option is set to true.

 */
{ lib, config, pkgs, customNixRootPath, ... }:

let
  inherit (lib) mkOption types mkIf;
in
{
  options = {
    service.useHostStore = mkOption {
      type = types.bool;
      default = false;
      description = "Bind mounts the host store if enabled, avoiding copying.";
    };
  };
  config = mkIf config.service.useHostStore {
    service.image = "arion-base";
    service.build.context = "${../../../arion-image}";
    service.volumes = [
      "${customNixRootPath}/nix/store:/nix/store"
      "${customNixRootPath}${pkgs.buildEnv { name = "container-system-env"; paths = [ pkgs.bashInteractive pkgs.coreutils ]; }}:/run/system"
    ];
  };
}
