{ config, lib, ... }:
let
  inherit (lib) mkOption;
  inherit (lib.types) attrsOf unspecified;
in
{
  options = {
    build.extendedInfo = mkOption {
      type = attrsOf unspecified;
      description = ''
        Information about a service to include in the Docker Compose file,
        but that will not be used by the <code>docker-compose</code> command
        itself.

        It will be inserted in <code>x-arion.serviceInfo.&lt;service.name></code>.
      '';
      default = {};
    };
  };
}