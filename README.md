
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

# How do I use Arion?

The command line inherits most `docker-compose` commands. These
examples assume you're in this repo's `examples/` directory.

TODO examples

# Do I need to use Hercules CI?

Nope, it's just Nix and Docker Compose under the hood.

# Why "Arion"?

Arion comes from Greek mythology. Poseidon, the god of ~Docker~ the
seas had his eye on Demeter. Demeter tried to trick him by disguising
as a horse, but Poseidon saw through the deception and they had Arion.

So Arion is a super fast divine horse; the result of some weird
mixing. Also it talks.

(And we feel morally obliged to name our stuff after Greek mythology)
