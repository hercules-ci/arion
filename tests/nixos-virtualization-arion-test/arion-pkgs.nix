# NOTE: This isn't used in the module!
import <nixpkgs> {
  # We specify the architecture explicitly. Use a Linux remote builder when
  # calling arion from other platforms.
  system = "x86_64-linux";
}
