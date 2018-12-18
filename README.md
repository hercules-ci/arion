
# Run docker-compose without images with Nix

*Wait, what?*

With Arion you can fire up containers without creating images for each
service. Instead, it uses a mostly empty image, sort of like a base
image, and makes the host's Nix store available in the container,
allowing the container to run programs without having to re-package
them into a docker image.

Arion is configured using Nix with modules, like those in
NixOS. Similar to `docker-compose` it can therefore combine
configuration from multiple files. For managing the network and
containers it delegates to the `docker-compose` command.

# Project Status

This project was born out of a process supervision need for local
development environments while working
on [Hercules CI](https://www.hercules-ci.com). (It was also born out
of ancient Greek deities disguised as horses. More on that later.)

We don't use it to deploy to 'real' environments and we have no plans
to do so in the future.

If you do want to use Arion for 'real' environments, you'll probably
want to either build images or manage garbage collection roots if you
control the deployment host. Either of these has yet to be
implemented.

Support for other Linux than NixOS is untested.

# Install

Have [Nix](https://nixos.org/nix/) and Docker installed.

    git clone git@github.com:hercules-ci/arion.git
    nix-env -iA arion -f .

# Example `arion-compose.nix`

This Nix expression serves the Nix manual at host port 8000 when launched with `arion up`. It is a function from a package set (`pkgs`) to a configuration.

```
{ pkgs, ... }:
{
  config.docker-compose.services = {

    webserver = {
      service.useHostStore = true;
      # service.depends_on = [ "backend" ];
      service.command = [ "sh" "-c" ''
                  cd "$$WEB_ROOT"
                  ${pkgs.python3}/bin/python -m http.server
                '' ];
      service.ports = [
        "8000:8000" # host:container
      ];
      service.environment.WEB_ROOT = "${pkgs.nix.doc}/share/doc/nix/manual";
    };

    # backend = { ... }
  };
}
```

The `pkgs` argument comes from a file called `arion-pkgs.nix`. It can be as simple as `import <nixpkgs> {}` to use the Nixpkgs from your `$NIX_PATH`.

# A full featured example

To see how Arion can be used in a project, have a look at [todomvc-nix](https://github.com/nix-community/todomvc-nix/tree/master/deploy/arion).

    git clone git@github.com:nix-community/todomvc-nix.git
    cd todomvc-nix/deploy/arion
    arion up

# How it works

Arion is essentially a thin wrapper around Nix and docker-compose.
When it runs, it does the following:

 - Evaluate the configuration using Nix, producing a `docker-compose.yaml` and a garbage collection root
 - Invoke `docker-compose`
 - Clean up the garbage collection root

Most of the interesting stuff happens in Arion's Nix expressions,
where it runs the module system (known from NixOS) and provides the configuration that makes the Docker Compose file do the things it needs to do.

The interesting part is of course the [service-host-store.nix module](src/nix/service-host-store.nix) which performs the bind mounts to make the host Nix store available in the container.

# "FAQ"

### Do I need to use Hercules CI?

Nope, it's just Nix and Docker Compose under the hood.


### Does Arion support Docker images?

Yes, you can still specify a docker image. For example:

    postgres = {
      service.image = "postgres:10";
      service.volumes = [ "${toString ./.}/postgres-data:/var/lib/postgresql/data" ];
      service.environment.POSTGRES_PASSWORD = "mydefaultpass";
    };

### What about garbage collection?

Arion removes the need for garbage collecting docker images,
delegating this task to Nix.

Arion creates a garbage collection root and cleans it up after
completing the command. This means that `arion up` without `-d` is
safe with respect to garbage collection. A deployment that is more
serious than local development must leave a GC root on the deployment
host. This use case is not supported as of now.

### What is messing with my environment variables?

Docker Compose performs its own environment variable
substitution. This can be a little annoying in `services.command` for
example. Either reference a script from `pkgs.writeScript` or escape
the dollar sign as `$$`.

### Why "Arion"?

Arion comes from Greek mythology. Poseidon, the god of ~Docker~ the
seas had his eye on Demeter. Demeter tried to trick him by disguising
as a horse, but Poseidon saw through the deception and they had Arion.

So Arion is a super fast divine horse; the result of some weird
mixing. Also it talks.

(And we feel morally obliged to name our stuff after Greek mythology)
