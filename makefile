.PHONY: build clean install remove run_main run_read test format cloc bisect
.PHONY: coverage doc opendoc zip

ML_FILES = $(wildcard src/*/*.ml) $(wildcard src/*/*/*.ml)
MLI_FILES = $(wildcard src/*/*.mli) $(wildcard src/*/*/*.mli)

build:
	@dune build --root src --no-print-directory

clean:
	@rm -rf src/_coverage *.zip
	@dune clean --root src --no-print-directory

install:
	@./util/script/install.sh

remove:
	@./util/script/remove.sh

run_main: build
	@OCAMLRUNPARAM=b dune exec --root src --no-print-directory bin/main.exe

run_read: build
	@OCAMLRUNPARAM=b dune exec --root src --no-print-directory bin/read.exe

test: build
	@cd src && OCAMLRUNPARAM=b dune exec test/main.exe

format:
	@for file in $(ML_FILES); do ocamlformat --inplace $$file; done
	@for file in $(MLI_FILES); do ocamlformat --inplace $$file; done
	@for file in $(ML_FILES); do ocp-indent --inplace $$file; done
	@for file in $(MLI_FILES); do ocp-indent --inplace $$file; done

cloc:
	@cloc --by-file --include-lang=OCaml .

bisect:
	@cd src && dune exec --instrument-with bisect_ppx test/main.exe
	@cd src && bisect-ppx-report html
	@cd src && bisect-ppx-report summary
	@cd src && mkdir -p _coverage/temp
	@cd src && mv *.coverage _coverage/temp

coverage: bisect
	@./util/script/opencov.sh

doc:
	@dune build --root src --no-print-directory @doc

opendoc: doc
	@./util/script/opendoc.sh

zip:
	@rm -f ocamulator.zip
	@zip -r ocamulator.zip . -x@util/info/exclude.lst
