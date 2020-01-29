{ pkgs, ... }:

let
  webRoot = "/www";

  webserverImage = pkgs.dockerTools.buildLayeredImage {
    name = "a-webserver";

    config = {
      Entrypoint = [
        "${pkgs.darkhttpd}/bin/darkhttpd" webRoot
      ];

      Volumes = {
        "${webRoot}" = {};
      };
    };
  };
in {
  config.services = {

    webserver = {
      image.drv = webserverImage;
      service.command = [ "--port" "8000" ];
      service.ports = [
        "8000:8000" # host:container
      ];
      service.volumes = [
        "${webRoot}:${pkgs.nix.doc}/share/doc/nix/manual"
      ];
    };
  };
}
