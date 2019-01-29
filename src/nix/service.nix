/*

   This service-level module defines the build.service option, using
   the user-facing options service.image, service.volumes, etc.

 */
{ pkgs, lib, config, ... }:

let
  inherit (lib) mkOption types;
  inherit (types) listOf nullOr attrsOf string either int;
in
{
  options = {
    service.volumes = mkOption {
      type = listOf types.unspecified;
      default = [];
      description = "";
    };
    service.build.context = mkOption {
      type = nullOr string;
      default = null;
      description = "";
    };
    service.environment = mkOption {
      type = attrsOf (either string int);
      default = {};
      description = "";
    };
    service.image = mkOption {
      type = string;
      description = "";
    };
    service.command = mkOption {
      type = nullOr types.unspecified;
      default = null;
      description = "";
    };
    service.depends_on = mkOption {
      type = listOf string;
      default = [];
      description = "";
    };
    service.working_dir = mkOption {
      type = nullOr string;
      default = null;
      description = "";
    };
    service.entrypoint = mkOption {
      type = nullOr string;
      default = null;
      description = "";
    };
    service.restart = mkOption {
      type = nullOr string;
      default = null;
      description = "";
    };
    service.ports = mkOption {
      type = listOf types.unspecified;
      default = [];
      description = ''
        Expose ports on host. "host:container" or structured.
        See https://docs.docker.com/compose/compose-file/#ports
      '';
    };
    service.expose = mkOption {
      type = listOf string;
      default = [];
      description = "";
    };

    build.service = mkOption {
      type = attrsOf types.unspecified;
      description = "";
    };
  };

  config.build.service = {
    inherit (config.service)
      volumes
      environment
      image
      ;
  } // lib.optionalAttrs (config.service.build.context != null) {
    inherit (config.service) build;
  } // lib.optionalAttrs (config.service.command != null) {
    inherit (config.service) command;
  } // lib.optionalAttrs (config.service.depends_on != []) {
    inherit (config.service) depends_on;
  } // lib.optionalAttrs (config.service.restart != null) {
    inherit (config.service) restart;
  } // lib.optionalAttrs (config.service.working_dir != null) {
    inherit (config.service) working_dir;
  } // lib.optionalAttrs (config.service.entrypoint != null) {
    inherit (config.service) entrypoint;
  } // lib.optionalAttrs (config.service.ports != []) {
    inherit (config.service) ports;
  } // lib.optionalAttrs (config.service.expose != []) {
    inherit (config.service) expose;
  };
}
