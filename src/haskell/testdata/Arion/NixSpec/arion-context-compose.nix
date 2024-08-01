{
  project.name = "unit-test-data";
  services.webserver.service = {
    build.context = "${./build-context}";
    build.secrets = ["foo"];
    ports = [
      "8080:80"
    ];
  };
  secrets.foo.environment = "FOO";
}
