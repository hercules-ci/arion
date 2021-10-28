pkgs: module:

pkgs.nixosTest {
  name = "test-basic-arion-kafka";
  nodes = {
    machine = { ... }: {
      virtualisation.memorySize = 3000;
      virtualisation.diskSize = 10000;
      imports = [
        ../../nixos-module.nix
        module
      ];

      virtualisation.arion.projects.whale.settings = {
        imports = [ ./arion-compose.nix ];
      };
    };
  };
  testScript = ''
    machine.wait_for_unit("sockets.target")
    machine.wait_for_unit("arion-whale.service")

    machine.succeed("""
      (echo "hello"; echo "world") \
        | ${pkgs.apacheKafka}/bin/kafka-console-producer.sh \
            --topic thetopic --bootstrap-server localhost:9092
    """)

    machine.succeed("""
      (
        set +o pipefail  # we only care for head's exit code
        ( ${pkgs.apacheKafka}/bin/kafka-console-consumer.sh \
            --topic thetopic --from-beginning --bootstrap-server localhost:9092 & \
          echo $! >pid
        ) | grep --line-buffered hello | { read; kill $(<pid); rm pid; }
      ) 2>/dev/console
    """)
    
  '';
}
