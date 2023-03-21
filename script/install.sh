#!/bin/bash

# Creating new switch
opam switch create nes ocaml-base-compiler.4.14.0 &> /dev/null \
    || echo "Switch already exists"
opam switch nes \
    || echo "Failed to switch to nes"

# Installing dependencies
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir
opam install -y ocaml-lsp-server ocamlformat ocamlformat-rpc
