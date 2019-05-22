{
  docker-compose.services.webserver = { pkgs, ... }: {
    nixos.useSystemd = true;
    nixos.configuration.boot.tmpOnTmpfs = true;
    nixos.configuration.services.nginx.enable = true;

    # Please don't do this
    nixos.configuration.services.nginx.virtualHosts.localhost.root = "/run/secrets";

    service.useHostStore = true;
    service.ports = [
      "8000:80" # host:container
    ];
    service.secrets."foo.txt".source = "foo";
  };
  docker-compose.secrets.foo.file = ./foo.key;
}
