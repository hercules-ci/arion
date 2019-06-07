{ pkgs, lib, config, ... }:

let
  inherit (lib) types;

  ngrokConfig = pkgs.writeText "ngrok-tunnels.yml" (builtins.toJSON config.ngrok.config);

in
{
  options.ngrok = {
    enable = lib.mkOption {
      description = "Turn on ngrok service";
      type = types.bool;
      default = false;
    };
    config = lib.mkOption {
      type = types.attrs;
      description = "ngrok.conf configuration";
    };
    userConfigFile = lib.mkOption {
      type = types.path;
      default = ~/.ngrok2/ngrok.yml;
      description = "path to ngrok.conf user configuration";
    };
  };

  config = lib.mkIf (config.ngrok.enable) {
    service.command = [ "${pkgs.ngrok}/bin/ngrok" "start" "--all" "--config" "${ngrokConfig}" "--config" "${config.ngrok.userConfigFile}" ];
    service.ports = [ "4040:4040" ];
    ngrok.config.web_addr = "0.0.0.0:4040";
  };
}
