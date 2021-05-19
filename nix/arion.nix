# Like the upstreamable expression but wired up for the local arion.
{ pkgs ? import ./. {}
, lib ? pkgs.lib
, haskell ? pkgs.haskell
, haskellPackages ? pkgs.haskellPackages
, arion-compose ? import ./haskell-arion-compose.nix { inherit pkgs haskellPackages; }
, runCommand ? pkgs.runCommand
}:
import ./upstreamable/default.nix {
  inherit pkgs lib haskell runCommand;
  haskellPackages = haskellPackages // { inherit arion-compose; };
  evalSrc = ./..;
}
