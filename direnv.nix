{ci-only ? false}:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  ciDeps = [pkgs.stack];
  devDeps = ciDeps ++
            (with pkgs;
              [ haskell-language-server
                dhall
                dhall-json
              ]
            );
in {
  env = pkgs.buildEnv {
    name = "polysemy-mocks-direnv";
    paths = if ci-only then ciDeps else devDeps;
  };
}
