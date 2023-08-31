{ nixpkgs }:
let
  haskellPackages = nixpkgs.stable.haskell.packages.ghc928.override {
    inherit (nixpkgs.unstable) all-cabal-hashes;
  };
in
haskellPackages.ghcWithPackages (import ./haskell-package-selection.nix)
