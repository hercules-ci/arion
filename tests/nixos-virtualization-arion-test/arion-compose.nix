{ pkgs, ... }: {
  project.name = "whale";

  docker-compose.volumes = {
    kafka = { };
  };

  services.kafka = {
    service.useHostStore = true;
    service.volumes = [
      {
        type = "volume";
        source = "kafka";
        target = "/var/lib/kafka/data";
      }
    ];
    service.ports = [ "9092:9092" ];
    service.environment = {
      # KRaft requires a unique cluster ID
      KAFKA_CLUSTER_ID = "MkU3OEVBNTcwNTJENDM2Qk";
    };
    image.name = "localhost/kafka";
    image.contents = [
      (pkgs.runCommand "root" { } ''
        mkdir -p $out/bin
        ln -s ${pkgs.runtimeShell} $out/bin/sh
        mkdir -p $out/var/lib/kafka/data
      '')
    ];
    # KRaft requires formatting the log directories with a cluster ID first
    image.command = [ 
      "${pkgs.writeShellScript "start-kafka" ''
        # Format the storage if not already done
        if [ ! -f /var/lib/kafka/data/meta.properties ]; then
          echo "Formatting Kafka storage..."
          ${pkgs.apacheKafka}/bin/kafka-storage.sh format \
            -t $KAFKA_CLUSTER_ID \
            -c ${./kafka/server.properties}
        fi
        
        # Start Kafka server
        exec ${pkgs.apacheKafka}/bin/kafka-server-start.sh ${./kafka/server.properties}
      ''}"
    ];
  };
}
