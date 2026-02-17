{ pkgs, ... }:

let
  webRoot = "/www";

  webserverImage = pkgs.dockerTools.buildLayeredImage {
    name = "a-webserver";

    config = {
      Entrypoint = [
        "${pkgs.darkhttpd}/bin/darkhttpd"
        webRoot
      ];

      Volumes = {
        "${webRoot}" = { };
      };
    };
  };
in
{
  project.name = "custom-image";
  services = {

    webserver = {
      image.tarball = webserverImage;

      # The following is essentially equivalent to
      #
      #   { image.tarball = webserverImage; }
      #
      # It is included here as a demonstration of how to configure Arion to
      # load a Docker image from *any* valid image tarball, not just one
      # produced with a `pkgs.dockerTools` builder function.
      #image.tarball = webserverImage.outPath;
      #image.name = webserverImage.imageName;
      #image.tag = webserverImage.imageTag;

      service.command = [ "--port" "8000" ];
      service.ports = [
        "8000:8000" # host:container
      ];
      service.volumes = [
        "${pkgs.nix.doc}/share/doc/nix/manual:${webRoot}"
      ];
    };
  };
}
