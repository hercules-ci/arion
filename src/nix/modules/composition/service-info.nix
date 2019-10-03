/*
   Adds extendedInfo from services to the Docker Compose file.

   This contains fields that are not in Docker Compose schema.
 */
{ config, lib, ... }:
let
  inherit (lib) mapAttrs filterAttrs;

  serviceInfo =
    filterAttrs (_k: v: v != {})
      (mapAttrs (_serviceName: service: service.out.extendedInfo)
        config.services
      );

in
{
  config = {
    docker-compose.extended.serviceInfo = serviceInfo;
  };
}
