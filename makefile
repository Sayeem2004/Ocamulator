.PHONY: build test clean

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop bin

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

clean:
	dune clean
