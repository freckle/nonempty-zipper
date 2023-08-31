{ nixpkgs }:
let
  pkgs = nixpkgs.stable;
in
pkgs.haskell.lib.justStaticExecutables
  nixpkgs.stable.hlint
