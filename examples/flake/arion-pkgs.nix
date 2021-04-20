let
  flake = if builtins ? getFlake
    then (builtins.getFlake (toString ./.)).pkgs
    else (import flake-compat { src = ./.; }).defaultNix;
  # NB: this is lazy
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  inherit (lock.nodes.flake-compat.locked) owner repo rev narHash;
  flake-compat = builtins.fetchTarball {
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    sha256 = narHash;
  };
in
  flake.pkgs
