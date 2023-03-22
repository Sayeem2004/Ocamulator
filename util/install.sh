#!/bin/bash

# Creating new switch
opam switch create nes ocaml-base-compiler.4.14.0
opam switch nes

# Installing dependencies
opam install -y utop
opam install -y odoc
opam install -y ounit2
opam install -y qcheck
opam install -y bisect_ppx
opam install -y menhir
opam install -y ocaml-lsp-server
opam install -y ocamlformat
opam install -y ocamlformat-rpc
