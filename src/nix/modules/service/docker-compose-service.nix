/*

   This service-level module defines the build.service option, using
   the user-facing options service.image, service.volumes, etc.

 */
{ pkgs, lib, config, ... }:

let
  inherit (lib) mkOption types mapAttrs mapAttrsToList;
  inherit (types) listOf nullOr attrsOf str either int bool;

  cfg = config.service;

  link = url: text:
    ''<link xlink:href="${url}">${text}</link>'';
  dockerComposeRef = fragment:
    ''See <link xlink:href="https://docs.docker.com/compose/compose-file/#${fragment}">Docker Compose#${fragment}</link>'';
  dockerComposeKitchenSink = ''
    Analogous to the <code>docker run</code> counterpart.

    ${dockerComposeRef "domainname-hostname-ipc-mac_address-privileged-read_only-shm_size-stdin_open-tty-user-working_dir"}
  '';

  cap_add = lib.attrNames (lib.filterAttrs (name: value: value == true) config.service.capabilities);
  cap_drop = lib.attrNames (lib.filterAttrs (name: value: value == false) config.service.capabilities);

  serviceSecretType = types.submodule {
    options = {
      source = mkOption {
        type = nullOr str;
        default = null;
        description = dockerComposeRef "secrets";
      };
      uid = mkOption {
        type = nullOr (either str int);
        default = null;
        description = dockerComposeRef "secrets";
      };
      gid = mkOption {
        type = nullOr (either str int);
        default = null;
        description = dockerComposeRef "secrets";
      };
      mode = mkOption {
        type = nullOr str;
        # default = "0444";
        default = null;
        description = ''
          The default value of is usually 0444. This option may not be supported
          when not deploying to a Swarm.

          ${dockerComposeRef "secrets"}
        '';
      };
    };
  };
  secrets = mapAttrsToList (k: s: {
    target = k;
  } //lib.optionalAttrs (s.source != null) {
    inherit (s) source;
  } // lib.optionalAttrs (s.uid != null) {
    inherit (s) uid;
  } // lib.optionalAttrs (s.gid != null) {
    inherit (s) gid;
  } // lib.optionalAttrs (s.mode != null) {
    inherit (s) mode;
  }) cfg.secrets;

in
{
  options = {
    build.service = mkOption {
      type = attrsOf types.unspecified;
      description = ''
        Raw input for the service in <code>docker-compose.yaml</code>.

        You should not need to use this option. If anything is
        missing, please contribute the missing option.

        This option is user accessible because it may serve as an
        escape hatch for some.
      '';
    };

    service.name = mkOption {
      type = str;
      description = ''
        The name of the service - <code>&lt;name></code> in the composition-level <code>docker-compose.services.&lt;name></code>
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
        "<code>docker run --device</code> documentation"}

        ${dockerComposeRef "devices"}
      '';
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

        Setting a capability to <code>true</code> means that it will be
        "added". Setting it to <code>false</code> means that it will be "dropped".
        ${dockerComposeRef "cap_add-cap_drop"}

        Omitted and <code>null</code> capabilities will therefore be set
        according to Docker's ${
          link "https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities"
               "default list of capabilities."
        }
      '';
    };
    service.secrets = mkOption {
      type = attrsOf serviceSecretType;
      default = {};
      description = dockerComposeRef "secrets";
      example = {
        redis_secret = {
          source = "web_cache_redis_secret";
          uid = 123;
          gid = 123;
          mode = "0440";
        };
      };
    };
  };

  config.build.service = {
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
  } // lib.optionalAttrs (config.service.links != []) {
    inherit (config.service) links;
  } // lib.optionalAttrs (config.service.ports != []) {
    inherit (config.service) ports;
  } // lib.optionalAttrs (config.service.privileged != null) {
    inherit (config.service) privileged;
  } // lib.optionalAttrs (config.service.network_mode != null) {
    inherit (config.service) network_mode;
  } // lib.optionalAttrs (config.service.restart != null) {
    inherit (config.service) restart;
  } // lib.optionalAttrs (config.service.stop_signal != null) {
    inherit (config.service) stop_signal;
  } // lib.optionalAttrs (secrets != null) {
    inherit secrets;
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
