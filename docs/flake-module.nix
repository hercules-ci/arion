{
  perSystem = { config, pkgs, lib, ... }: {
    packages.generated-option-doc-arion =
      # TODO: use the render pipeline in flake-parts,
      #       which has support for things like {options}`foo`.
      let
        eval = lib.evalModules {
          modules = import ../src/nix/modules.nix;
        };
      in
      (pkgs.nixosOptionsDoc
        {
          options = eval.options;
        }).optionsCommonMark;

    packages.generated-antora-files =
      pkgs.runCommand "generated-antora-files"
        {
          nativeBuildInputs = [ pkgs.pandoc ];
          doc_arion = config.packages.generated-option-doc-arion;
        }
        # TODO: use the render pipeline in flake-parts,
        #       which has support for things like {options}`foo`.
        ''
          mkdir -p $out/modules/ROOT/partials
          pandoc --from=markdown --to=asciidoc \
            < $doc_arion \
            > $out/modules/ROOT/partials/arion-options.adoc
        '';
  };
}
