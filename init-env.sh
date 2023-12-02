#! /usr/bin/env bash
set -euo pipefail

# Make sure we're in the same directory as the script
cd -- "$(dirname -- "${BASH_SOURCE[0]}")" || exit
rm -rf _opam
opam switch create ./
opam install -y dune ocaml-lsp-server odoc ocamlformat utop core
opam install -y .
