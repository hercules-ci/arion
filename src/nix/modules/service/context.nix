{ lib, ... }:
{
  options = {
    host = lib.mkOption {
      type = lib.types.attrs;
      readOnly = true;
      description = ''
        The composition-level host option values.
      '';
    };
    composition = lib.mkOption {
      type = lib.types.attrs;
      readOnly = true;
      description = ''
        The composition configuration.
      '';
    };
  };
}
