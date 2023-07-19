{ config, lib, ... }:

let
  inherit (lib)
    mkOption
    optionalAttrs
    types
    ;
  inherit (import ../../lib.nix { inherit lib; })
    link
    ;
in
{
  options = {
    networks = mkOption {
      type = types.lazyAttrsOf (types.submoduleWith {
        modules = [
          ../networks/network.nix
        ];
      });
      description = ''
        See ${link "https://docs.docker.com/compose/compose-file/06-networks/" "Docker Compose Networks"}
      '';
    };
    enableDefaultNetwork = mkOption {
      type = types.bool;
      description = ''
        Whether to define the default network:

        ```nix
        networks.default = {
          name = config.project.name;
        };
        ```
      '';
      default = true;
    };
  };


  config = {

    networks = optionalAttrs config.enableDefaultNetwork {
      default = {
        name = config.project.name;
      };
    };

    docker-compose.raw.networks =
      lib.mapAttrs (k: v: v.out) config.networks;

  };
}
