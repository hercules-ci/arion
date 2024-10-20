{
  project.name = "unit-test-data";
  services.webserver.service = {
    build.context = "${./build-context}";
    build.secrets = ["foo"];
    ports = [
      "8080:80"
    ];
    secrets = {
      "foo" = {
        target = "/run/secrets/foo";
        uid = "123";
        gid = "123";
        mode = "0440";
      };
    };
  };
  secrets.foo.environment = "FOO";
}
