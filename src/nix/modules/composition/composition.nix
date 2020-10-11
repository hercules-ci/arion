{ config, lib, ... }:
let
  inherit (lib) types mkOption;

  link = url: text:
    ''link:${url}[${text}]'';

in
{
  options = {
    project.name = mkOption {
      description = ''
        Name of the project.

        See ${link "https://docs.docker.com/compose/reference/envvars/#compose_project_name" "COMPOSE_PROJECT_NAME"}
      '';
      type = types.nullOr types.str;
      default = null;
    };
  };
  config = {
    docker-compose.extended.project.name = config.project.name;
  };
}
