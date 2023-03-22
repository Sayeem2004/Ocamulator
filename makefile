.PHONY: build clean install remove run_main run_read test format cloc bisect
.PHONY: coverage doc opendoc zip

ML_FILES = $(wildcard *.ml) $(wildcard */*.ml) $(wildcard */*/*.ml)
MLI_FILES = $(wildcard *.mli) $(wildcard */*.mli) $(wildcard */*/*.mli)

build:
	@dune build

clean:
	@rm -rf _coverage
	@dune clean
	@rm -f *.zip

install:
	@./util/install.sh

remove:
	@./util/remove.sh

run_main: build
	@OCAMLRUNPARAM=b dune exec bin/main.exe

run_read: build
	@OCAMLRUNPARAM=b dune exec bin/read.exe

test: build
	@OCAMLRUNPARAM=b dune exec test/main.exe

format:
	@for file in $(ML_FILES); do ocamlformat --inplace $$file; done
	@for file in $(MLI_FILES); do ocamlformat --inplace $$file; done
	@for file in $(ML_FILES); do ocp-indent --inplace $$file; done
	@for file in $(MLI_FILES); do ocp-indent --inplace $$file; done

cloc:
	@cloc --by-file --include-lang=OCaml .

bisect:
	@-dune exec --instrument-with bisect_ppx ./test/main.exe
	@bisect-ppx-report html
	@bisect-ppx-report summary
	@mkdir -p _coverage/temp
	@mv *.coverage _coverage/temp

coverage: bisect
	@./util/opencov.sh

doc:
	@dune build @doc

opendoc: doc
	@./util/opendoc.sh

zip:
	@rm -f nes.zip
	@zip -r nes.zip . -x@util/exclude.lst
