{ pkgs, ... }: {
  project.name = "whale";

  docker-compose.raw = {
    volumes.zookeeper = { };
    volumes.kafka = { };
  };

  services.kafka = {
    service.useHostStore = true;
    # service.volumes = [
    #   {
    #     type = "volume";
    #     source = "kafka";
    #     target = "/data";
    #     # volume.nocopy = true;
    #   }
    # ];
    service.ports = [ "9092:9092" ];
    service.depends_on = [ "zookeeper" ];
    image.name = "localhost/kafka";
    image.contents = [
      (pkgs.runCommand "root" { } ''
        mkdir -p $out/bin
        ln -s ${pkgs.runtimeShell} $out/bin/sh
      '')
    ];
    image.command = [
      "${pkgs.apacheKafka}/bin/kafka-server-start.sh"
      "${./kafka/server.properties}"
    ];
  };

  services.zookeeper = {
    service.useHostStore = true;
    service.ports = [ "2181:2181" ];
    # service.volumes = [
    #   {
    #     type = "volume";
    #     source = "zookeeper";
    #     target = "/data";
    #     # volume.nocopy = true;
    #   }
    # ];
    image.name = "localhost/zookeeper";
    image.contents = [
      (pkgs.buildEnv {
        name = "root";
        paths = [
          # pkgs.sed
          pkgs.busybox
        ];
      })
    ];
    image.command = [
      "${pkgs.zookeeper}/bin/zkServer.sh"
      "--config"
      "${./zookeeper}"
      "start-foreground"
    ];
  };
}
