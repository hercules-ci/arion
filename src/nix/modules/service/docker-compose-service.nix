/*

   This service-level module defines the out.service option, using
   the user-facing options service.image, service.volumes, etc.

 */
{ pkgs, lib, config, ... }:

let
  inherit (lib) mkOption types;
  inherit (types) listOf nullOr attrsOf str either int bool;

  link = url: text:
    ''link:${url}[${text}]'';
  dockerComposeRef = fragment:
    ''See ${link "https://docs.docker.com/compose/compose-file/#${fragment}" "Docker Compose#${fragment}"}'';
  dockerComposeKitchenSink = ''
    Analogous to the `docker run` counterpart.

    ${dockerComposeRef "domainname-hostname-ipc-mac_address-privileged-read_only-shm_size-stdin_open-tty-user-working_dir"}
  '';

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
      description = dockerComposeRef "volumes";
    };
    service.tmpfs = mkOption {
      type = listOf types.str;
      default = [];
      description = dockerComposeRef "tmpfs";
    };
    service.build.context = mkOption {
      type = nullOr str;
      default = null;
      description = ''
        Locates a Dockerfile to use for creating an image to use in this service.

        ${dockerComposeRef "context"}
      '';
    };
    service.hostname = mkOption {
      type = nullOr str;
      default = null;
      description = dockerComposeKitchenSink;
    };
    service.tty = mkOption {
      type = nullOr bool;
      default = null;
      description = dockerComposeKitchenSink;
    };
    service.environment = mkOption {
      type = attrsOf (either str int);
      default = {};
      description = dockerComposeRef "environment";
    };
    service.image = mkOption {
      type = str;
      description = dockerComposeRef "image";
    };
    service.command = mkOption {
      type = nullOr types.unspecified;
      default = null;
      description = dockerComposeRef "command";
    };
    service.container_name = mkOption {
      type = nullOr types.str;
      default = null;
      description = dockerComposeRef "container_name";
    };
    service.depends_on = mkOption {
      type = listOf str;
      default = [];
      description = dockerComposeRef "depends_on";
    };
    service.devices = mkOption {
      type = listOf str;
      default = [];
      description = ''
        See ${link "https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities"
        "`docker run --device` documentation"}

        ${dockerComposeRef "devices"}
      '';
    };
    service.dns = mkOption {
      type = listOf str;
      default = [];
      example = [ "8.8.8.8" "8.8.4.4" ];
      description = dockerComposeRef "dns";
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
      description = dockerComposeRef "labels";
    };
    service.links = mkOption {
      type = listOf str;
      default = [];
      description = dockerComposeRef "links";
    };
    service.external_links = mkOption {
      type = listOf str;
      default = [];
      description = dockerComposeRef "external_links";
    };
    service.extra_hosts = mkOption {
      type = listOf str;
      default = [];
      description = dockerComposeRef "extra_hosts";
    };
    service.working_dir = mkOption {
      type = nullOr str;
      default = null;
      description = dockerComposeKitchenSink;
    };
    service.privileged = mkOption {
      type = nullOr bool;
      default = null;
      description = dockerComposeKitchenSink;
    };
    service.entrypoint = mkOption {
      type = nullOr str;
      default = null;
      description = dockerComposeRef "entrypoint";
    };
    service.restart = mkOption {
      type = nullOr str;
      default = null;
      description = dockerComposeRef "restart";
    };
    service.user = mkOption {
      type = nullOr str;
      default = null;
      description = dockerComposeKitchenSink;
    };
    service.ports = mkOption {
      type = listOf types.unspecified;
      default = [];
      description = ''
        Expose ports on host. "host:container" or structured.

        ${dockerComposeRef "ports"}
      '';
    };
    service.expose = mkOption {
      type = listOf str;
      default = [];
      description = dockerComposeRef "expose";
    };
    service.env_file = mkOption {
      type = listOf str;
      default = [];
      description = dockerComposeRef "env_file";
    };
    service.network_mode = mkOption {
      type = nullOr str;
      default = null;
      description = dockerComposeRef "network_mode";
    };
    service.networks = mkOption {
      type = nullOr (listOf types.str);
      default = null;
      description = dockerComposeRef "networks";
    };
    service.stop_signal = mkOption {
      type = nullOr str;
      default = null;
      description = dockerComposeRef "stop_signal";
    };
    service.sysctls = mkOption {
      type = attrsOf (either str int);
      default = {};
      description = dockerComposeRef "sysctls";
    };
    service.capabilities = mkOption {
      type = attrsOf (nullOr bool);
      default = {};
      example = { ALL = true; SYS_ADMIN = false; NET_ADMIN = false; };
      description = ''
        Enable/disable linux capabilities, or pick Docker's default.

        Setting a capability to `true` means that it will be
        "added". Setting it to `false` means that it will be "dropped".
        ${dockerComposeRef "cap_add-cap_drop"}

        Omitted and `null` capabilities will therefore be set
        according to Docker's ${
          link "https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities"
               "default list of capabilities."
        }
      '';
    };
  };

  config.out.service = {
    inherit (config.service)
      volumes
      environment
      sysctls
      image
      ;
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
  } // lib.optionalAttrs (config.service.networks != null) {
    inherit (config.service) networks;
  } // lib.optionalAttrs (config.service.restart != null) {
    inherit (config.service) restart;
  } // lib.optionalAttrs (config.service.stop_signal != null) {
    inherit (config.service) stop_signal;
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
