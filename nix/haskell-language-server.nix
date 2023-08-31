{ nixpkgs }:
nixpkgs.stable.haskell-language-server.override
{ supportedGhcVersions = [ "928" ]; }
