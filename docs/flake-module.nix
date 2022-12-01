{
  perSystem = { config, pkgs, ... }:
    let
      doc-options = import ./options.nix { };

    in
    {
      packages.doc-options = pkgs.callPackage ./options.nix { };

      checks.doc-options = pkgs.runCommand "doc-options-check" { } ''
        if diff --color -u ${./modules/ROOT/partials/NixOSOptions.adoc} ${config.packages.doc-options}; then
          touch $out
        else
          echo 1>&2 "The doc options have changed and need to be added."
          echo 1>&2 "Please run ./update-options in the root of your arion clone."
          exit 1
        fi
      '';
    };
}
