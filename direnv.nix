let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  ciDeps = [pkgs.stack];
  devDeps = ciDeps ++
            (with pkgs;
              [ haskell-language-server
                dhall
                dhall-json
                jq
                shellcheck
                (import ./nix/fly.nix {inherit stdenv fetchurl;})
              ]
            );
in {
  devEnv = pkgs.buildEnv {
    name = "polysemy-mocks-dev-env";
    paths = devDeps;
  };
  ciEnv = pkgs.buildEnv {
    name = "polysemy-mocks-ci-env";
    paths = ciDeps;
  };
}
