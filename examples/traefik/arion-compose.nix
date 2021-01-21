{ pkgs, ... }: {

  config.services = {
    traefik.service = {
      image = "traefik:v2.4";
      container_name = "traefik";
      command = [
        "--api.insecure=true"
        "--providers.docker=true"
        "--providers.docker.exposedbydefault=false"
        "--entrypoints.web.address=:80"
      ];
      ports = [ "80:80" "8080:8080" ];
      volumes = [ "/var/run/docker.sock:/var/run/docker.sock:ro" ];
    };

    whoami.service = {
      image = "traefik/whoami";
      container_name = "simple-service";
      labels = {
        "traefik.enable" = "true";
        "traefik.http.routers.whoami.rule" = "Host(`whoami.localhost`)";
        "traefik.http.routers.whoami.entrypoints" = "web";
      };
    };
  };
}

