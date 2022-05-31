# Revision history for Arion

## Next

### Changed

* Healthcheck-based dependencies in `service.depends_on`.
* The `project.name` option is now mandatory.

### Added

* Support `service.healthcheck` for defining custom healthchecks.

## 0.1.3.0 -- 2020-05-03

### Changed

* `useHostStore` now uses an image derived from the `image.*` options. You may
  need to enable `enableRecommendedContents` because with this change, files
  like `/bin/sh` aren't added by default anymore.

* Drop obsolete NixOS 19.03, 19.09 and 20.03 from CI.

### Added

* NixOS-based containers can now run on Podman when it is configured to provide a docker socket. See the [installation docs](https://docs.hercules-ci.com/arion/#_nixos).

* Support `service.dns`, for overriding the DNS servers used by containers.

* Support `service.labels`, which is useful for autodiscovery among other things.

* Add a tested example for Traefik with label-based routing.

* Add a `flake.nix` and an experimental flake example

* Add a warning when systemd `DynamicUser` is used but not available to the
  container.

* CI with NixOS 21.05

## 0.1.2.0 -- 2020-03-05

* Support use of prebuilt `docker-compose.yaml`.
  Separates build and execution without duplicating evaluation.

* Avoid storing tarballs (wasting store space) by using
  `dockerTools.streamLayeredImage` if available.

* Project name is now configurable via the `project.name` option

* Support --no-ansi, --compatibility, --log-level options

## 0.1.1.1 -- 2020-03-20

* Fix ambiguous import of `lines`
* Improve base version constraint
* Fix warnings

## 0.1.1.0 -- 2020-03-19

* Support Nixpkgs 20.03
* Fixes for macOS

## 0.1.0.0 -- 2019-10-04

* First released version. Released on an unsuspecting world.

