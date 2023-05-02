.PHONY: build clean install remove run_main run_read test format cloc bisect
.PHONY: coverage doc opendoc zip scrape

ML_FILES = $(wildcard src/*/*.ml) $(wildcard src/*/*/*.ml)
MLI_FILES = $(wildcard src/*/*.mli) $(wildcard src/*/*/*.mli)

build:
	@cd src && dune build --no-print-directory

clean:
	@rm -rf src/_coverage *.zip
	@cd src && dune clean --no-print-directory

install:
	@./util/script/install.sh

remove:
	@./util/script/remove.sh

run_main: build
	@cd src && OCAMLRUNPARAM=b dune exec --no-print-directory bin/main.exe

run_read: build
	@cd src && OCAMLRUNPARAM=b dune exec --no-print-directory bin/read.exe

test: build
	@cd src && OCAMLRUNPARAM=b dune exec test/main.exe

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

coverage: bisect
	@./util/script/opencov.sh

doc:
	cd src && dune build @doc --no-print-directory

opendoc: doc
	@./util/script/opendoc.sh

zip:
	@rm -f ocamulator.zip
	@zip -r ocamulator.zip . -x@data/exclude.lst

scrape:
	@python util/scrape.py
	@prettier --write data/opcode &> /dev/null
