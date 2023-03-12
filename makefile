.PHONY: build clean cloc install run test

build:
	dune build

clean:
	dune clean

cloc:
	cloc --by-file --include-lang=OCaml .

install:
	opam install . --deps-only

run:
	dune exec bin/main.exe

test:
	dune exec test/main.exe
