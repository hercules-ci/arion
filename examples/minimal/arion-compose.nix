{ pkgs, ... }:
{
  config.docker-compose.services = {

    webserver.imports = [ ./service-webserver.nix ];

    # webserver = {...}: {
    #  imports = [ ./service-webserver.nix ];
    #};

  };
}
