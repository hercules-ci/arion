# Instead of pinning Nixpkgs, we can opt to use the one in NIX_PATH
import <nixpkgs> {
  # We specify the architecture explicitly. Use a Linux remote builder when
  # calling arion from other platforms.
  system = "x86_64-linux";
}
