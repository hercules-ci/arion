# A user module adds an option to every service, declaring `services` with
# the given submodule type function.
mkSubmoduleType:
{ lib, ... }: {
  options.services = lib.mkOption {
    type = lib.types.attrsOf (mkSubmoduleType lib ({ config, ... }: {
      options.greeting = lib.mkOption { type = lib.types.str; };
      config.service.environment.GREETING = config.greeting;
    }));
  };

  config.services.a = {
    service.image = "nginx";
    greeting = "hello";
  };
}
