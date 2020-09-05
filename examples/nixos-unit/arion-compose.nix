
/*

  DISCLAIMER

  This uses a somewhat hidden feature in NixOS, which is the
  "runner". It's a script that's available on systemd services
  that lets you run the service independently from systemd.
  However, it was clearly not intended for public consumption
  so please use it with care.
  It does not support all features of systemd so you are on
  your own if you use it in production.

  One known issue is that the script does not respond to docker's
  SIGTERM shutdown signal.

 */

{
  services.webserver = { config, pkgs, ... }: {

    nixos.configuration = {config, pkgs, ...}: {
      boot.isContainer = true;
      services.nginx.enable = true;
      services.nginx.virtualHosts.localhost.root = "${pkgs.nix.doc}/share/doc/nix/manual";
      system.build.run-nginx = pkgs.writeScript "run-nginx" ''
        #!${pkgs.bash}/bin/bash
        PATH='${config.systemd.services.nginx.environment.PATH}'
        echo nginx:x:${toString config.users.users.nginx.uid}:${toString config.users.groups.nginx.gid}:nginx web server user:/var/empty:/bin/sh >>/etc/passwd
        echo nginx:x:${toString config.users.groups.nginx.gid}:nginx >>/etc/group
        echo 'nobody:x:65534:65534:Unprivileged account do not use:/var/empty:/run/current-system/sw/bin/nologin' >>/etc/passwd
        echo 'nogroup:x:65534:' >>/etc/group
        mkdir -p /var/log/nginx /run/nginx/ /var/cache/nginx
        chown nginx /var/log/nginx /run/nginx/ /var/cache/nginx
        ${config.systemd.services.nginx.runner}
      '';
    };
    service.command = [ config.nixos.build.run-nginx ];
    service.useHostStore = true;
    service.ports = [
      "8000:80" # host:container
    ];
  };
}
