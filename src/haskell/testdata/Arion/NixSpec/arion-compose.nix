{
  project.name = "unit-test-data";
  services.webserver = { pkgs, ... }: {
    nixos.useSystemd = true;
    nixos.configuration.boot.tmp.useTmpfs = true;
    nixos.configuration.services.nginx.enable = true;
    nixos.configuration.services.nginx.virtualHosts.localhost.root = "${pkgs.nix.doc}/share/doc/nix/manual";
    service.useHostStore = true;
    service.ports = [
      "8000:80" # host:container
    ];
  };
  services.extra-options.service = {
    image = "busybox";
    depends_on.webserver.required = false;
    depends_on.depends-on-list.restart = true;
    device_cgroup_rules = [ "c 1:3 mr" ];
    read_only = true;
    security_opt = [ "no-new-privileges:true" ];
  };
  services.depends-on-list.service = {
    image = "busybox";
    depends_on = [ "webserver" ];
  };
}
