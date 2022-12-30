{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {inherit system;};
        ghcOverrides = hself: hsuper: {
          polysemy = pkgs.haskell.lib.doJailbreak hsuper.polysemy_1_8_0_0;
          polysemy-mocks = hsuper.callPackage ./default.nix {};
        };
        ghc942Pkgs = pkgs.haskell.packages.ghc942.override {
          overrides = ghcOverrides;
        };
        ghc924Pkgs = pkgs.haskell.packages.ghc924.override {
          overrides = ghcOverrides;
        };
        ghc902Pkgs = pkgs.haskell.packages.ghc902.override {
          overrides = ghcOverrides;
        };
        ghc8107Pkgs = pkgs.haskell.packages.ghc8107.override {
          overrides = ghcOverrides;
        };
        ghc884Pkgs = pkgs.haskell.packages.ghc884.override {
          overrides = ghcOverrides;
        };
      in rec {
        packages = {
          devEnv = pkgs.buildEnv {
            name = "polysemy-mocks-dev-env";
            paths = [
              pkgs.haskell-language-server
              pkgs.dhall
              pkgs.dhall-json
              pkgs.jq
              pkgs.shellcheck
              pkgs.fly
              pkgs.cabal2nix
              pkgs.haskell.compiler.ghc8107
              pkgs.cabal-install
              pkgs.haskellPackages.hspec-discover
            ];
          };
          polysemy-mocks-ghc942 = ghc942Pkgs.polysemy-mocks;
          polysemy-mocks-ghc924 = ghc924Pkgs.polysemy-mocks;
          polysemy-mocks-ghc902 = ghc902Pkgs.polysemy-mocks;
          polysemy-mocks-ghc8107 = ghc8107Pkgs.polysemy-mocks;
          polysemy-mocks-ghc884 = ghc884Pkgs.polysemy-mocks;
        };
        defaultPackage = packages.devEnv;
    });
}
