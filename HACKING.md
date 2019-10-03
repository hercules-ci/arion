
# Hacking on the modules

## Easiest option

The module system does not distinguish between modules and configurations.
This mean you can prototype any feature by factoring out functionality in a real-world project.

## Changing the built-in modules

If your change is not just an addition or if it's better implemented by refactoring, you'll want to fork and edit arion sources directly.

For a fast iteration cycle (but possibly outdated arion command logic):

    ~/src/arion/run-arion-quick up -d

To update the arion command logic on the next run

    rm ~/src/arion/result-run-arion-quick


# Hacking on the arion command

The arion command is written in Haskell. Anyone can make small changes to the code.
Experience with Haskell tooling is not required. You can use the nixified scripts in the root of the repo for common tasks.
 - `build` or `live-check` for typechecking
 - `live-unit-tests` (only the test suite is "live" though)
 - `repl` for a Haskell REPL
 - `run-arion` to run an incrementally built arion
 - `run-arion-via-nix` to run a nix-built arion
 - ~~`run-arion-quick`~~ *not for command hacking;* use stale command. See previous section.
