{ config, lib, ... }:

let
  inherit (lib)
    mkOption
    types
    ;
  inherit (import ../../lib.nix { inherit lib; })
    link
    ;
in
{

  options = {
    secrets = mkOption {
      type = types.lazyAttrsOf (types.submoduleWith {
        modules = [
          ../secrets/secret.nix
        ];
      });
      description = ''
        See ${link "https://docs.docker.com/compose/compose-file/09-secrets/" "Docker Compose Secrets"}
      '';
    };
  };

  config = {

    secrets = {};
    docker-compose.secrets = lib.mapAttrs (k: v: v.out) config.secrets;

  };
}
