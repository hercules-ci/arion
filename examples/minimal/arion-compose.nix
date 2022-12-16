{ pkgs, ... }:
{
  project.name = "webapp";
  services = {

    webserver = {
      image.enableRecommendedContents = true;
      service.useHostStore = true;
      service.command = [ "sh" "-c" ''
                  cd "$$WEB_ROOT"
                  ${pkgs.python3}/bin/python -m http.server
                '' ];
      service.ports = [
        "8000:8000" # host:container
      ];
      service.environment.WEB_ROOT = "${pkgs.nix.doc}/share/doc/nix/manual";
      service.stop_signal = "SIGINT";
    };
  };
}
