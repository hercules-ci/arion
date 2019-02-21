/*

   This service-level module defines the build.service option, using
   the user-facing options service.image, service.volumes, etc.

 */
{ pkgs, lib, config, ... }:

let
  inherit (lib) mkOption types;
  inherit (types) listOf nullOr attrsOf string either int bool;
in
{
  options = {
    service.volumes = mkOption {
      type = listOf types.unspecified;
      default = [];
    };
    service.environment = mkOption {
      type = attrsOf (either string int);
      default = {};
    };
    service.image = mkOption {
      type = string;
    };
    service.build.context = mkOption {
      type = nullOr string;
      default = null;
    };
    service.command = mkOption {
      type = nullOr types.unspecified;
      default = null;
    };
    service.depends_on = mkOption {
      type = listOf string;
      default = [];
    };
    service.env_file = mkOption {
      type = listOf string;
      default = [];
    };
    service.privileged = mkOption {
      type = nullOr bool;
      default = null;
    };
    service.hostname = mkOption {
      type = nullOr string;
      default = null;
    };
    service.restart = mkOption {
      type = nullOr string;
      default = null;
    };
    service.working_dir = mkOption {
      type = nullOr string;
      default = null;
    };
    service.entrypoint = mkOption {
      type = nullOr string;
      default = null;
    };
    service.ports = mkOption {
      type = listOf types.unspecified;
      default = [];
      description = ''
        Expose ports on host. "host:container" or structured.
        See https://docs.docker.com/compose/compose-file/#ports
      '';
    };
    service.links = mkOption {
      type = listOf string;
      default = [];
    };
    service.external_links = mkOption {
      type = listOf string;
      default = [];
    };
    service.extra_hosts = mkOption {
      type = listOf string;
      default = [];
    };
    service.expose = mkOption {
      type = listOf string;
      default = [];
    };
    build.service = mkOption {
      type = attrsOf types.unspecified;
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
  } // lib.optionalAttrs (config.service.env_file != []) {
    inherit (config.service) env_file;
  } // lib.optionalAttrs (config.service.privileged != null) {
    inherit (config.service) privileged;
  } // lib.optionalAttrs (config.service.hostname != null) {
    inherit (config.service) hostname;
  } // lib.optionalAttrs (config.service.restart != null) {
    inherit (config.service) restart;
  } // lib.optionalAttrs (config.service.working_dir != null) {
    inherit (config.service) working_dir;
  } // lib.optionalAttrs (config.service.entrypoint != null) {
    inherit (config.service) entrypoint;
  } // lib.optionalAttrs (config.service.ports != []) {
    inherit (config.service) ports;
  } // lib.optionalAttrs (config.service.links != []) {
    inherit (config.service) links;
  } // lib.optionalAttrs (config.service.external_links != []) {
    inherit (config.service) external_links;
  } // lib.optionalAttrs (config.service.extra_hosts != []) {
    inherit (config.service) extra_hosts;
  } // lib.optionalAttrs (config.service.expose != []) {
    inherit (config.service) expose;
  };
}
