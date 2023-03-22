.PHONY: bisect build coverage clean cloc doc install opendoc remove run_main
		run_read test zip

build:
	dune build

clean:
	rm -rf _coverage
	dune clean

install:
	./util/install.sh

remove:
	./util/remove.sh

run_main: build
	OCAMLRUNPARAM=b dune exec bin/main.exe

run_read: build
	OCAMLRUNPARAM=b dune exec bin/read.exe

test: build
	OCAMLRUNPARAM=b dune exec test/main.exe

cloc:
	cloc --by-file --include-lang=OCaml .

bisect:
	-dune exec --instrument-with bisect_ppx ./test/main.exe
	bisect-ppx-report html
	bisect-ppx-report summary
	mkdir -p _coverage/temp
	mv *.coverage _coverage/temp

coverage: bisect
	./util/opencov.sh

doc:
	dune build @doc

opendoc: doc
	./util/opendoc.sh

zip:
	rm -f nes.zip
	zip -r nes.zip . -x@util/exclude.lst
