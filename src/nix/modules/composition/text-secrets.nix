{ config, lib, ... }:
let
  inherit (lib) mkOption mapAttrsToList concatStrings escapeShellArg;
  inherit (lib.types) attrsOf unspecified;

in
{
  options = {
    textSecrets = mkOption {
      type = attrsOf unspecified; # unspecified for laziness
      default = {};
      description = "Secrets to write to files.";
    };
    build.writeSecretsScript = mkOption {
      type = unspecified; # unspecified for laziness
      readOnly = true;
      internal = true;
      description = "Generated script that writes the textSecrets.";
    };
  };

  config = {
    docker-compose.extended.hasTextSecrets = config.textSecrets != {};
    build.writeSecretsScript = concatStrings (mapAttrsToList (k: v: ''
      mkdir -p "$ARION_SECRETS_DIR"
      echo ${escapeShellArg v} >$ARION_SECRETS_DIR/${escapeShellArg k}
    '') config.textSecrets);
  };
}