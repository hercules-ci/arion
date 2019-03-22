/*
  Invokes the NixOS init system in the container.
 */
{ config, lib, pkgs, ... }:
let
  inherit (lib) types;
in
{
  options = {
    nixos.useSystemd = lib.mkOption {
      type = types.bool;
      default = false;
      description = ''
        When enabled, call the NixOS systemd-based init system.

        Configure NixOS with <code>nixos.configuration</code>.
      '';
    };
  };

  config = lib.mkIf (config.nixos.useSystemd) {
    nixos.configuration.imports = [
      ../nixos/container-systemd.nix
      ../nixos/default-shell.nix
      (pkgs.path + "/nixos/modules/profiles/minimal.nix")
    ];
    image.command = [ "${config.nixos.build.toplevel}/init" ];
    service.environment.container = "docker";
    service.volumes = [
      "/sys/fs/cgroup:/sys/fs/cgroup:ro"
    ];
    service.tmpfs = [
      "/run"          # noexec is fine because exes should be symlinked from elsewhere anyway
      "/run/wrappers" # noexec breaks this intentionally
    ] ++ lib.optional (config.nixos.evaluatedConfig.boot.tmpOnTmpfs) "/tmp:exec,mode=777";

    service.stop_signal = "SIGRTMIN+3";
    service.tty = true;
    service.defaultExec = [config.nixos.build.x-arion-defaultShell "-l"];
  };
}
