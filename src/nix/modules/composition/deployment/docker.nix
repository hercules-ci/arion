{ lib, ... }:
let 
  inherit (lib) mkOption types;
in
{
  options = {
    deployment.technology = mkOption {
      type = types.enum ["docker"];
      default = "docker";
    };
  };
}
