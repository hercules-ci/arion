{ config, lib, options, ... }:

let
  inherit (lib)
    mkOption
    optionalAttrs
    types
    ;
  inherit (import ../../lib.nix { inherit lib; })
    networkRef
    ;
in
{
  options = {
    driver = mkOption {
      description = ''
        `"none"`, `"host"`, or a platform-specific value.
        ${networkRef "driver"}
      '';
      type = types.str;
    };

    driver_opts = mkOption {
      description = ''
        ${networkRef "driver_opts"}
      '';
      type = types.lazyAttrsOf types.raw or types.unspecified;
    };

    attachable = mkOption {
      description = ''
        ${networkRef "attachable"}
      '';
      type = types.bool;
      example = true;
    };

    enable_ipv6 = mkOption {
      description = ''
        Whether we've entered the 21st century yet.
        
        ${networkRef "enable_ipv6"}
      '';
      type = types.bool;
    };

    ipam = mkOption {
      # TODO model sub-options
      description = ''
        Manage IP addresses.

        ${networkRef "ipam"}
      '';
      type = types.raw or types.unspecified;
    };

    internal = mkOption {
      description = ''
        Achieves "external isolation".

        ${networkRef "internal"}
      '';
      defaultText = false;
      type = types.bool;
    };

    labels = mkOption {
      description = ''
        Metadata.

        ${networkRef "labels"}
      '';
      # no list support, because less expressive wrt overriding
      type = types.attrsOf types.str;
    };

    external = mkOption {
      description = ''
        When `true`, don't create or destroy the network, but assume that it
        exists.

        ${networkRef "external"}
      '';
      type = types.bool;
    };

    name = mkOption {
      description = ''
        Set a custom name for the network.

        It shares a namespace with other projects' networks. `name` is used as-is.

        Note the `default` network's default `name` is set to `project.name` by Arion.

        ${networkRef "name"}
      '';
      type = types.str;
    };

    out = mkOption {
      internal = true;
      description = ''
        This network's contribution to the docker compose yaml file
        under the `networks.''${name}` key.
      '';
      type = lib.types.attrsOf lib.types.raw or lib.types.unspecified;
    };
  };

  config = {
    out =
      lib.mapAttrs
        (k: opt: opt.value)
        (lib.filterAttrs
          (k: opt: opt.isDefined)
          {
            inherit (options)
              driver
              driver_opts
              attachable
              enable_ipv6
              ipam
              internal
              labels
              external
              name
              ;
          }
        );
  };
}
