{
  project.name = "unit-test-data";
  services.webserver.service = {
    build.context = "${./build-context}";
    ports = [
      "8080:80"
    ];
  };
}
