{ config, lib, options, ... }:

let
  inherit (lib)
    mkOption
    optionalAttrs
    types
    ;
  inherit (import ../../lib.nix { inherit lib; })
    secretRef
    ;
in
{
  options = {
    file = mkOption {
      description = ''
        The secret is created with the contents of the file at the specified path.
        ${secretRef "file"}
      '';
      type = types.nullOr (types.either types.path types.str);
    };

    environment = mkOption {
      description = ''
        The secret is created with the value of an environment variable.
        ${secretRef "environment"}
      '';
      type = types.nullOr types.str;
    };

    external = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether the value of this secret is set via other means.
        ${secretRef "secrets"}
      '';
    };

    out = mkOption {
      internal = true;
      description = ''
        Defines sensitive data that is granted to the services in your Compose application.
        The source of the secret is either `file` or `environment`.
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
              file
              environment
              external
              ;
          }
        );
  };
}
