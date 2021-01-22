# Revision history for Arion

## Next

* Support `service.labels`, which is useful for autodiscovery among other things.

* Add a tested example for Traefik with label-based routing.

* Drop obsolete NixOS 19.09 and 20.03 support. It may still be usable there.

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
