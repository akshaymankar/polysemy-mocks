let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in pkgs.mkShell {
  name = "polysemy-mocks-shell";
  buildInputs = with pkgs; [
    stack
    haskell-language-server
  ];
}
