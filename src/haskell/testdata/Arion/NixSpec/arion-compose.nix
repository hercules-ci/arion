{
  project.name = "unit-test-data";
  services.webserver = { pkgs, ... }: {
    nixos.useSystemd = true;
    nixos.configuration.boot.tmpOnTmpfs = true;
    nixos.configuration.services.nginx.enable = true;
    nixos.configuration.services.nginx.virtualHosts.localhost.root = "${pkgs.nix.doc}/share/doc/nix/manual";
    service.useHostStore = true;
    service.ports = [
      "8000:80" # host:container
    ];
  };
}
