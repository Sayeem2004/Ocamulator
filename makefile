.PHONY: build clean cloc install run_main run_read test

build:
	dune build

clean:
	dune clean

cloc:
	cloc --by-file --include-lang=OCaml .

install:
	./script/install.sh

run_main:
	OCAMLRUNPARAM=b dune exec bin/main.exe

run_read:
	OCAMLRUNPARAM=b dune exec bin/read.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe
