{
  # An attribute set definition is a module, so it can use imports.
  services.a = {
    imports = [ { service.environment.IMPORTED = "yes"; } ];
    service.image = "nginx";
  };
}
