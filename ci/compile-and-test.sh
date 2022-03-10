#!/usr/bin/env sh

set -eu

readonly repo=${1:?"Please provide path to repository"}
readonly ghc=${2:?"Please provide name of the ghc"}

nix-env -iA nixpkgs.nixFlakes nixpkgs.git nixpkgs.cachix
echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf

cachix use akshaymankar

echo "Building jsonpath with ghc=$ghc"
cachix watch-exec akshaymankar -- nix build -L "./$repo#polysemy-mocks-$ghc"
