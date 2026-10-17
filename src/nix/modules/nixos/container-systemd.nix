/*
  NixOS configuration to for running a mostly normal systemd-based
  NixOS in Docker.
 */
{ pkgs, lib, options, ...}: {

  # imports = [
  #   # This profile doesn't seem to work well.
  #   (pkgs.path + "/nixos/modules/profiles/docker-container.nix")
  #   This one works, but can not be imported here due because imports can not depend on pkgs.
  #   (pkgs.path + "/nixos/modules/profiles/minimal.nix")
  # ];
  boot.isContainer = true;
  boot.specialFileSystems = lib.mkForce {};
  networking.hostName = "";

  # NixOS 26.11 replaces services.journald.console with settings.
  services.journald =
    if options.services.journald ? settings
    then {
      settings.Journal = {
        ForwardToConsole = true;
        TTYPath = "/dev/console";
      };
    }
    else {
      console = "/dev/console";
    };

  systemd.services.systemd-logind.enable = false;
  systemd.services.console-getty.enable = false;

  systemd.sockets.nix-daemon.enable = lib.mkDefault false;
  systemd.services.nix-daemon.enable = lib.mkDefault false;
}
