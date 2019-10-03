/*
   Adds extendedInfo from services to the Docker Compose file.

   This contains fields that are not in Docker Compose schema.
 */
{ config, lib, ... }:
let
  serviceInfo =
    lib.mapAttrs getInfo (
      lib.filterAttrs filterFunction config.services
    );

  filterFunction = _serviceName: service:
    # shallow equality suffices for emptiness test
    builtins.attrNames service.build.extendedInfo != [];

  getInfo = _serviceName: service: service.build.extendedInfo;

in
{
  config = {
    docker-compose.extended.serviceInfo = serviceInfo;
  };
}
