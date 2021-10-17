{ pkgs, ... }:
let 
    sh = pkgs.stdenv.mkDerivation {
        name = "sh";
        phases = [ "installPhase" ];

        installPhase = ''
            mkdir -p "$out"/bin
            ln -s ${pkgs.bash}/bin/sh "$out"/bin/sh
        '';
    };
in{
  config.project.name = "webapp";
  config.services = {

    webserver = {
      image.contents = [ sh ];
      service.useHostStore = true;
      service.command = [ "sh" "-c" ''
                  cd "$$WEB_ROOT"
                  ${pkgs.python3}/bin/python -m http.server
                '' ];
      service.ports = [
        "8000:8000" # host:container
      ];
      service.environment.WEB_ROOT = "${pkgs.nix.doc}/share/doc/nix/manual";
      service.stop_signal = "SIGINT";
    };
  };
}
