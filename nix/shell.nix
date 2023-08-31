{ nixpkgs }:
nixpkgs.stable.mkShell {
  buildInputs =
    with (import ./. { inherit nixpkgs; });
    [
      cabal
      fourmolu
      ghc-with-packages
      haskell-language-server
      hlint
      stack
    ];
}
