{ config, lib, ... }:

let
  inherit (lib)
    mkOption
    optionalAttrs
    types
    ;
  inherit (import ../../lib.nix { inherit lib; })
    dockerComposeRef
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
        ${dockerComposeRef "networks-top-level-element"}
      '';
      default = {};
    };
  };


  config = {

    docker-compose.raw.networks =
      lib.mapAttrs (k: v: v.out) config.networks;

  };
}
