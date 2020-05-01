{ pkgs ? (import <nixpkgs> {})}:
pkgs.haskell.lib.buildStackProject {
  name = "polysemy-mocks";
  ghc = pkgs.haskell.compiler.ghc882;
}
