{ lib, ... }:
{
  options = {
    host = lib.mkOption {
      type = lib.types.attrs;
      description = ''
        The composition-level host option values.
      '';
    };
  };
}
