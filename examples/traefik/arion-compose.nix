/*
 
  An example of
   - traefik HTTP reverse proxy
   - minimal images
   - routing via docker labels

  Run `arion up -d` and open http://nix-docs.localhost/

 */
{ lib, pkgs, ... }: {
  config.project.name = "traefik";
  config.networks = {
    traefik-custom = {
      name = "traefik-custom";
      ipam = {
        config = [{
          subnet = "172.32.0.0/16";
          gateway = "172.32.0.1";
        }];
      };
    };
  };
  config.services = {
    traefik = {
      image.command = [
        "${pkgs.traefik}/bin/traefik"
        "--api.insecure=true"
        "--providers.docker=true"
        "--providers.docker.exposedbydefault=false"
        "--entrypoints.web.address=:80"
      ];
      service = {
        container_name = "traefik";
        stop_signal = "SIGINT";
        ports = [ "80:80" "8080:8080" ];
        volumes = [ "/var/run/docker.sock:/var/run/docker.sock:ro" ];
        networks = [ "traefik-custom" ];
      };
    };

    nix-docs = {
      image.command = ["${pkgs.writeScript "entrypoint" ''
        #!${pkgs.bash}/bin/bash
        cd ${pkgs.nix.doc}/share/doc/nix/manual
        ${pkgs.python3}/bin/python -m http.server
      ''}"];
      service.container_name = "simple-service";
      service.stop_signal = "SIGINT";
      service.labels = {
        "traefik.enable" = "true";
        "traefik.http.routers.nix-docs.rule" = "Host(`nix-docs.localhost`)";
        "traefik.http.routers.nix-docs.entrypoints" = "web";
        "traefik.http.services.nix-docs.loadBalancer.server.port" = "8000";
      };
      service.networks = {
        traefik-custom = {
          ipv4_address = "172.32.0.5";
        };
      };
    };
  };
}

