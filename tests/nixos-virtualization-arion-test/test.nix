pkgs: module:

pkgs.testers.runNixOSTest ({ lib, ... }:{
  name = "test-basic-arion-kafka";
  nodes = {
    machine = { ... }: {
      virtualisation.memorySize = 4096;
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
  testScript = { nodes, ... }: ''
    machine.wait_for_unit("sockets.target")
    machine.wait_for_unit("arion-whale.service")

    ${# TODO: make the kafka service work on podman-socket; some networking issue
      lib.optionalString (nodes.machine.virtualisation.arion.backend != "podman-socket") ''

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

    # make sure logs were captured
    machine.succeed("""
      # Check that messages were logged with field "CONTAINER_NAME" set to "whale-zookeeper-1"
      journalctl --output json | ${pkgs.jq}/bin/jq 'select(.CONTAINER_NAME=="whale-zookeeper-1") | .MESSAGE' | grep -F 'org.apache.zookeeper'
    """)

    ''}

    machine.wait_until_succeeds("""
      journalctl --grep 'Registering SIGTERM handler' >/dev/null
    """)

    # explore the shared mounts, as they're undocumented
    machine.succeed("""
      mount >&2
      touch /tmp/xchg/this-is-xchg
      touch /tmp/shared/this-is-shared
    """)

    machine.shutdown()

    # show what's in machine.shared_dir by running `ls` on the host
    dir = machine.shared_dir
    import os
    print(f'Contents of {dir}:')
    os.system(f'ls -l {dir}')
    # dir/stop-probe-terminated-cleanly must exist
    assert os.path.exists(f'{dir}/stop-probe-terminated-cleanly'), f'{dir}/stop-probe-terminated-cleanly does not exist'

  '';
})
