{ config, lib, ... }:
let
  inherit (lib) mkOption;
  inherit (lib.types) attrsOf unspecified;
in
{
  imports = [
    (lib.mkRenamedOptionModule ["build" "extendedInfo"] ["out" "extendedInfo"])
  ];
  options = {
    out.extendedInfo = mkOption {
      type = attrsOf unspecified;
      description = ''
        Information about a service to include in the Docker Compose file,
        but that will not be used by the `docker-compose`> command
        itself.

        It will be inserted in `x-arion.serviceInfo.<service.name>`.
      '';
      default = {};
    };
  };
}