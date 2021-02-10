let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.haskell.lib.buildStackProject {
  name = "polysemy-mocks";
  ghc = pkgs.haskell.compiler.ghc884;
}
