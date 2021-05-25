/*

   This service-level module bind mounts the host store into the container
   when the service.useHostStore option is set to true.

 */
{ lib, config, pkgs, ... }:

let
  inherit (lib) mkOption types mkIf;
  escape = s: lib.replaceStrings ["$"] ["$$"] s;
in
{
  options = {
    service.useHostStore = mkOption {
      type = types.bool;
      default = false;
      description = "Bind mounts the host store if enabled, avoiding copying.";
    };
    service.hostStoreAsReadOnly = mkOption {
      type = types.bool;
      default = true;
      description = "Adds a ':ro' (read-only) access mode to the host nix store bind mount.";
    };
    service.useHostNixDaemon = mkOption {
      type = types.bool;
      default = false;
      description = "Make the host Nix daemon available.";
    };
  };
  config = mkIf config.service.useHostStore {
    image.includeStorePaths = false;
    service.environment.NIX_REMOTE = lib.optionalString config.service.useHostNixDaemon "daemon";
    service.volumes = [
      "${config.host.nixStorePrefix}/nix/store:/nix/store${lib.optionalString config.service.hostStoreAsReadOnly ":ro"}"
      ] ++ lib.optional config.service.useHostNixDaemon "/nix/var/nix/daemon-socket:/nix/var/nix/daemon-socket";
    service.command = lib.mkDefault (map escape (config.image.rawConfig.Cmd or []));
  };
}
