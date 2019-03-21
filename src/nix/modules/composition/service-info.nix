/*
   Adds extendedInfo from services to the Docker Compose file.

   This contains fields that are not in Docker Compose schema.
 */
{ config, lib, ... }:
let
  serviceInfo =
    lib.mapAttrs getInfo (
      lib.filterAttrs filterFunction config.docker-compose.evaluatedServices
    );

  filterFunction = _serviceName: service:
    # shallow equality suffices for emptiness test
    builtins.attrNames service.config.build.extendedInfo != [];

  getInfo = _serviceName: service: service.config.build.extendedInfo;

in
{
  config = {
    docker-compose.extended.serviceInfo = serviceInfo;
  };
}
