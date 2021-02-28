#!/usr/bin/env sh

set -eu

readonly repo=${1:?"Please provide path to repository"}
readonly stackYaml=${2:?"Please provide name of the stack.yaml"}

env="$(nix-build "$repo/direnv.nix" -A ciEnv)"
export PATH="$env/bin:$PATH"

stack test --stack-yaml="$repo/$stackYaml" --nix-pure
