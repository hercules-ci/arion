{ config, utils, ... }:
{
  config.system.build.x-arion-defaultShell = utils.toShellPath config.users.defaultUserShell;
}
