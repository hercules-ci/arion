{ config, lib, ... }:
let 
  inherit (lib) mkOption types;
in
{
  options = {
    deployment.technology = mkOption {
      description = "Which container technology to use.";
      type = types.enum [];
    };
  };
  config = {
    docker-compose.raw.x-arion.technology = config.deployment.technology;
  };
}
