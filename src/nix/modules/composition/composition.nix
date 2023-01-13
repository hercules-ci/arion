{ config, lib, ... }:
let
  inherit (lib) types mkOption;

  link = url: text:
    ''[${text}](${url})'';

in
{
  options = {
    _module.args = mkOption {
      internal = true;
    };
    project.name = mkOption {
      description = ''
        Name of the project.

        See ${link "https://docs.docker.com/compose/reference/envvars/#compose_project_name" "COMPOSE_PROJECT_NAME"}

        This is not optional, because getting the project name from a directory name tends to produce different results for different repo checkout location names.
      '';
      type = types.str;
    };
  };
  config = {
    docker-compose.extended.project.name = config.project.name;
  };
}
