.PHONY: build clean test install remove execute simulate format cloc bisect doc zip

ML_FILES = $(wildcard src/*/*.ml) $(wildcard src/*/*/*.ml)
MLI_FILES = $(wildcard src/*/*.mli) $(wildcard src/*/*/*.mli)

build:
	@cd src && OCAMLRUNPARAM=b dune build --no-print-directory

clean:
	@rm -rf src/_coverage *.zip
	@cd src && dune clean --no-print-directory

test: build
	@cd src && OCAMLRUNPARAM=b dune exec test/main.exe

install:
	@./util/script/install.sh

remove:
	@./util/script/remove.sh

execute: build
	@cd src && OCAMLRUNPARAM=b dune exec --no-print-directory bin/execute.exe

simulate: build
	@cd src && OCAMLRUNPARAM=b dune exec --no-print-directory bin/simulate.exe

format:
	@for file in $(ML_FILES); do ocamlformat --inplace $$file; done
	@for file in $(MLI_FILES); do ocamlformat --inplace $$file; done
	@for file in $(ML_FILES); do ocp-indent --inplace $$file; done
	@for file in $(MLI_FILES); do ocp-indent --inplace $$file; done

cloc:
	@cloc --by-file --exclude-list-file=data/exclude.lst --include-lang=OCaml  .

bisect:
	@cd src && dune exec --instrument-with bisect_ppx test/main.exe
	@cd src && bisect-ppx-report html
	@cd src && bisect-ppx-report summary
	@cd src && mkdir -p _coverage/temp
	@cd src && mv *.coverage _coverage/temp
	@./util/script/opencov.sh

doc:
	@cd src && dune build @doc --no-print-directory
	@./util/script/opendoc.sh

zip:
	@rm -f ocamulator.zip
	@zip -r ocamulator.zip . -x@data/exclude.lst
