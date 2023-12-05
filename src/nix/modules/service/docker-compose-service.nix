/*

   This service-level module defines the out.service option, using
   the user-facing options service.image, service.volumes, etc.

 */
{ pkgs, lib, config, options, ... }:

let
  inherit (lib) mkOption types;
  inherit (types) listOf nullOr attrsOf str either int bool submodule enum;

  inherit (import ../../lib.nix { inherit lib; })
    link
    serviceRef
    ;

  cap_add = lib.attrNames (lib.filterAttrs (name: value: value == true) config.service.capabilities);
  cap_drop = lib.attrNames (lib.filterAttrs (name: value: value == false) config.service.capabilities);

in
{
  imports = [
    (lib.mkRenamedOptionModule ["build" "service"] ["out" "service"])
  ];

  options = {
    out.service = mkOption {
      type = attrsOf types.unspecified;
      description = ''
        Raw input for the service in `docker-compose.yaml`.

        You should not need to use this option. If anything is
        missing, please contribute the missing option.

        This option is user accessible because it may serve as an
        escape hatch for some.
      '';
      apply = config.assertWarn;
    };

    service.name = mkOption {
      type = str;
      description = ''
        The name of the service - `<name>` in the composition-level `services.<name>`
      '';
      readOnly = true;
    };

    service.volumes = mkOption {
      type = listOf types.unspecified;
      default = [];
      description = serviceRef "volumes";
    };
    service.tmpfs = mkOption {
      type = listOf types.str;
      default = [];
      description = serviceRef "tmpfs";
    };
    service.build.context = mkOption {
      type = nullOr str;
      default = null;
      description = ''
        Locates a Dockerfile to use for creating an image to use in this service.

        https://docs.docker.com/compose/compose-file/build/#context
      '';
    };
    service.hostname = mkOption {
      type = nullOr str;
      default = null;
      description = ''
        ${serviceRef "hostname"}
      '';
    };
    service.tty = mkOption {
      type = nullOr bool;
      default = null;
      description = ''
        ${serviceRef "tty"}
      '';
    };
    service.environment = mkOption {
      type = attrsOf (either str int);
      default = {};
      description = serviceRef "environment";
    };
    service.image = mkOption {
      type = nullOr str;
      default = null;
      description = serviceRef "image";
    };
    service.command = mkOption {
      type = nullOr types.unspecified;
      default = null;
      description = serviceRef "command";
    };
    service.container_name = mkOption {
      type = nullOr types.str;
      default = null;
      description = serviceRef "container_name";
    };
    service.depends_on =
      let conditionsModule = {
            options = {
              condition = mkOption {
                type = enum ["service_started" "service_healthy" "service_completed_successfully"];
                description = serviceRef "depends_on";
                default = "service_started";
              };
            };
          };
       in mkOption {
         type = either (listOf str) (attrsOf (submodule conditionsModule));
         default = [];
         description = serviceRef "depends_on";
       };
    service.healthcheck = mkOption {
      description = serviceRef "healthcheck";
      type = submodule ({ config, options, ...}: {
        options = {
          _out = mkOption {
            internal = true;
            default = lib.optionalAttrs (options.test.highestPrio < 1500) {
              inherit (config) test interval timeout start_period retries;
            };
          };
          test = mkOption {
            type = nullOr (listOf str);
            default = null;
            example = [ "CMD" "pg_isready" ];
            description = serviceRef "healthcheck";
          };
          interval = mkOption {
            type = str;
            default = "30s";
            example = "1m";
            description = serviceRef "healthcheck";
          };
          timeout = mkOption {
            type = str;
            default = "30s";
            example = "10s";
            description = serviceRef "healthcheck";
          };
          start_period = mkOption {
            type = str;
            default = "0s";
            example = "30s";
            description = serviceRef "healthcheck";
          };
          retries = mkOption {
            type = int;
            default = 3;
            description = serviceRef "healthcheck";
          };
        };
      });
    };
    service.devices = mkOption {
      type = listOf str;
      default = [];
      description = ''
        See ${link "https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities"
        "`docker run --device` documentation"}

        ${serviceRef "devices"}
      '';
    };
    service.dns = mkOption {
      type = listOf str;
      default = [];
      example = [ "8.8.8.8" "8.8.4.4" ];
      description = serviceRef "dns";
    };
    service.labels = mkOption {
      type = attrsOf str;
      default = {};
      example = {
        "com.example.foo" = "bar";
        "traefik.enable" = "true";
        "traefik.http.routers.my-service.rule" = "Host(`my-service.localhost`)";
        "traefik.http.routers.my-service.entrypoints" = "web";
      };
      description = serviceRef "labels";
    };
    service.links = mkOption {
      type = listOf str;
      default = [];
      description = serviceRef "links";
    };
    service.external_links = mkOption {
      type = listOf str;
      default = [];
      description = serviceRef "external_links";
    };
    service.extra_hosts = mkOption {
      type = listOf str;
      default = [];
      description = serviceRef "extra_hosts";
    };
    service.working_dir = mkOption {
      type = nullOr str;
      default = null;
      description = ''
        ${serviceRef "working_dir"}
      '';
    };
    service.privileged = mkOption {
      type = nullOr bool;
      default = null;
      description = ''
        ${serviceRef "privileged"}
      '';
    };
    service.entrypoint = mkOption {
      type = nullOr str;
      default = null;
      description = serviceRef "entrypoint";
    };
    service.restart = mkOption {
      type = nullOr str;
      default = null;
      description = serviceRef "restart";
    };
    service.user = mkOption {
      type = nullOr str;
      default = null;
      description = ''
        ${serviceRef "user"}
      '';
    };
    service.ports = mkOption {
      type = listOf types.unspecified;
      default = [];
      description = ''
        Expose ports on host. "host:container" or structured.

        ${serviceRef "ports"}
      '';
    };
    service.expose = mkOption {
      type = listOf str;
      default = [];
      description = serviceRef "expose";
    };
    service.env_file = mkOption {
      type = listOf str;
      default = [];
      description = serviceRef "env_file";
    };
    service.network_mode = mkOption {
      type = nullOr str;
      default = null;
      description = serviceRef "network_mode";
    };
    service.networks =
      let
        networksModule = submodule ({ config, options, ...}: {
          options = {
            _out = mkOption {
              internal = true;
              readOnly = true;
              default = lib.mapAttrs (k: opt: opt.value) (lib.filterAttrs (_: opt: opt.isDefined) { inherit (options) aliases ipv4_address ipv6_address link_local_ips priority; });
            };
            aliases = mkOption {
              type = listOf str;
              description = serviceRef "aliases";
              default = [ ];
            };
            ipv4_address = mkOption {
              type = str;
              description = serviceRef "ipv4_address-ipv6_address";
            };
            ipv6_address = mkOption {
              type = str;
              description = serviceRef "ipv4_address-ipv6_address";
            };
            link_local_ips = mkOption {
              type = listOf str;
              description = serviceRef "link_local_ips";
            };
            priority = mkOption {
              type = int;
              description = serviceRef "priority";
            };
          };
        });
      in
      mkOption {
        type = either (listOf str) (attrsOf networksModule);
        default = [];
        description = serviceRef "networks";
      };
    service.stop_signal = mkOption {
      type = nullOr str;
      default = null;
      description = serviceRef "stop_signal";
    };
    service.stop_grace_period = mkOption {
      type = nullOr str;
      default = null;
      description = serviceRef "stop_grace_period";
    };
    service.sysctls = mkOption {
      type = attrsOf (either str int);
      default = {};
      description = serviceRef "sysctls";
    };
    service.capabilities = mkOption {
      type = attrsOf (nullOr bool);
      default = {};
      example = { ALL = true; SYS_ADMIN = false; NET_ADMIN = false; };
      description = ''
        Enable/disable linux capabilities, or pick Docker's default.

        Setting a capability to `true` means that it will be
        "added". Setting it to `false` means that it will be "dropped".

        Omitted and `null` capabilities will therefore be set
        according to Docker's ${
          link "https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities"
               "default list of capabilities."
        }

        ${serviceRef "cap_add"}
        ${serviceRef "cap_drop"}
      '';
    };
  };

  config.out.service = {
    inherit (config.service)
      volumes
      environment
      sysctls
      ;
  } // lib.optionalAttrs (config.service.image != null) {
    inherit (config.service) image;
  } // lib.optionalAttrs (config.service.build.context != null) {
    inherit (config.service) build;
  } // lib.optionalAttrs (cap_add != []) {
    inherit cap_add;
  } // lib.optionalAttrs (cap_drop != []) {
    inherit cap_drop;
  } // lib.optionalAttrs (config.service.command != null) {
    inherit (config.service) command;
  } // lib.optionalAttrs (config.service.container_name != null) {
    inherit (config.service) container_name;
  } // lib.optionalAttrs (config.service.depends_on != []) {
    inherit (config.service) depends_on;
  } // lib.optionalAttrs (options.service.healthcheck.highestPrio < 1500) {
    healthcheck = config.service.healthcheck._out;
  } // lib.optionalAttrs (config.service.devices != []) {
    inherit (config.service) devices;
  } // lib.optionalAttrs (config.service.entrypoint != null) {
    inherit (config.service) entrypoint;
  } // lib.optionalAttrs (config.service.env_file != []) {
    inherit (config.service) env_file;
  } // lib.optionalAttrs (config.service.expose != []) {
    inherit (config.service) expose;
  } // lib.optionalAttrs (config.service.external_links != []) {
    inherit (config.service) external_links;
  } // lib.optionalAttrs (config.service.extra_hosts != []) {
    inherit (config.service) extra_hosts;
  } // lib.optionalAttrs (config.service.hostname != null) {
    inherit (config.service) hostname;
  } // lib.optionalAttrs (config.service.dns != []) {
    inherit (config.service) dns;
  } // lib.optionalAttrs (config.service.labels != {}) {
    inherit (config.service) labels;
  } // lib.optionalAttrs (config.service.links != []) {
    inherit (config.service) links;
  } // lib.optionalAttrs (config.service.ports != []) {
    inherit (config.service) ports;
  } // lib.optionalAttrs (config.service.privileged != null) {
    inherit (config.service) privileged;
  } // lib.optionalAttrs (config.service.network_mode != null) {
    inherit (config.service) network_mode;
  } // lib.optionalAttrs (config.service.networks != [] && config.service.networks != {}) {
    networks =
      if (builtins.isAttrs config.service.networks) then builtins.mapAttrs (_: v: v._out) config.service.networks
      else config.service.networks;
  } // lib.optionalAttrs (config.service.restart != null) {
    inherit (config.service) restart;
  } // lib.optionalAttrs (config.service.stop_signal != null) {
    inherit (config.service) stop_signal;
  } // lib.optionalAttrs (config.service.stop_grace_period != null) {
    inherit (config.service) stop_grace_period;
  } // lib.optionalAttrs (config.service.tmpfs != []) {
    inherit (config.service) tmpfs;
  } // lib.optionalAttrs (config.service.tty != null) {
    inherit (config.service) tty;
  } // lib.optionalAttrs (config.service.working_dir != null) {
    inherit (config.service) working_dir;
  } // lib.optionalAttrs (config.service.user != null) {
    inherit (config.service) user;
  };
}
