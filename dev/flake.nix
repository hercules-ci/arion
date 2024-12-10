{
  # TODO move back into main lock after https://github.com/NixOS/nix/issues/7730, remove this subflake
  description = "Development dependencies in a separate subflake so that they don't end up in your lock";
  inputs = {
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
    git-hooks-nix.url = "github:cachix/git-hooks.nix";
  };
  outputs = { ... }: { };
}
