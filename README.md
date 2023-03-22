# Ocamulator

Ocamulator is a NES emulator written in OCaml. It was written from scratch
and was created by Mohammad Khan, Pablo Raigoza, and Evan Vera. Installation and
usage instructions are below along with a more detailed description of the project.

## Installation/Usage

Installation and usage instructions can be found in [INSTALL.md](info/INSTALL.md).

## Goals

- [X] Use Git and GitHub to facilitate our development process.
- [X] Develop the appropriate structure and utilities for a project of this size.
- [X] Find and download example project ROMs from the internet.
- [X] Implement a type system for unsigned integers of different sizes.
- [X] Appropriately test the type system to ensure correctness.
- [X] Create functionality to initialize RAM and CPU state.
- [ ] Produce a UI to display the RAM and CPU states. (In progress)
- [ ] Parse a ROM file and extract relevant instructions. (In progress)
- [ ] Appropriately test the ROM parsing to ensure correctness.
- [ ] Write functionality to handle instructions and modify RAM and CPU state. (In progress)
- [ ] Appropriately test the RAM and CPU states to ensure correctness.
- [ ] Produce a UI to display the RAM and CPU states after modification.
- [ ] Successfully run parser and instruction handler on ROM files.
- [ ] Implement PPU functionality and connect it to CPU and RAM.
- [ ] Appropriately test the PPU to ensure correctness.
- [ ] Display pixel rendering and window functionality.
- [ ] Parse a ROM file and extract relevant PPU information and sprites.
- [ ] Appropriately test the ROM parsing of PPU to ensure correctness.
- [ ] Display the title screen of Super Mario Bros. ROM file.
- [ ] Implement keyboard functionality and connect it to CPU and RAM.
- [ ] Modify display to change based on keyboard input.
- [ ] Appropriately test keyboard functionality to ensure correctness.
- [ ] Emulate and complete Super Mario Bros. ROM file.
- [ ] Emulate and complete other games' ROM files. (Reach goal)
- [ ] Share emulator with friends and have a friendly competition.
- [ ] Develop an AI to attempt to play Super Mario Bros. ROM file. (Reach goal)
- [ ] Test AI against friends and see who can get the highest score. (Reach goal)

## Makefile Commands

`make build` Compiles and builds the project.

`make clean` Purges temporary files from the project.

`make install` Creates a new opam switch and installs the dependencies.

`make remove` Removes the opam switch and dependencies.

`make run_main` Runs the main emulator of the project.

`make run_read` Runs the hex reading portion of the project.

`make test` Runs the test cases for the project.

`make format` Formats all `.ml` and `.mli` files in the project.

`make cloc` Counts the number of lines of code in the project.

`make bisect` Generates an HTML report of the test coverage of the project.

`make coverage` Opens the HTML report of the test coverage of the project.

`make doc` Generates the HTML documentation for the project.

`make opendoc` Opens the HTML documentation for the project.

`make zip` Zips all necessary components of the project.

## Project Structure

```txt
bin:                      Contains project executables
|-- dune:                 (Dune configuration)
|-- main.ml:              (Main entrypoint for emulator)
|-- read.ml:              (Parallel entrypoint for hex reading)

data:                     Contains project NES ROMs
|-- contra.nes:           (Contra ROM)
|-- donkey.nes:           (Donkey Kong ROM)
|-- mario.nes:            (Super Mario Bros ROM)
|-- tetris.nes:           (Tetris ROM)
|-- zelda.nes:            (The Legend Of Zelda ROM)

lib:                      Contains project libraries
|-- cpu:                  Contains CPU library
|   |-- cpu.ml:           (Module representing the CPU state)
|   |-- cpu.mli:          (Interface for CPU module)
|   |-- instructions.ml:  (Module representing OPCodes and instructions)
|-- ram:                  Contains RAM library
|   |-- ram.ml:           (Module representing the RAM state)
|-- type:                 Contains type library
|   |-- uInt.ml:          (Generic signature for unsigned integers)
|   |-- uInt8.ml:         (Module representing unsigned 8-bit integers)
|   |-- uInt16.ml:        (Module representing unsigned 16-bit integers)
|   |-- uIntSet.ml:       (Generic signature for the bounds of unsigned integers)
|   |-- uIntVar.ml:       (Variant type for unsigned integers)
|   |-- uIntX.ml:         (Functor for transforming bounds into unsigned integers)
|-- dune                  (Dune configuration)

test:                     Contains project tests
|-- cpu:                  Tests for CPU library
|   |-- cpuTest.ml:       (Test cases for CPU state)
|-- ram:                  Tests for RAM library
|   |-- ramTest.ml:       (Test cases for RAM state)
|-- type:                 Tests for type library
|   |-- uInt8Test.ml:     (Test cases for unsigned 8-bit integers)
|   |-- uInt16Test.ml:    (Test cases for unsigned 16-bit integers)
|   |-- uIntXTest.ml:     (Functor of test cases for unsigned integers)
|-- dune:                 (Dune configuration)
|-- main.ml:              (Main entrypoint for testing)

util:                     Contains project utilities
|-- exclude.lst:          (List of files to exclude from zip file)
|-- install.sh:           (Script to install switch and dependencies)
|-- opencov.sh:           (Script to open HTML coverage report)
|-- opendoc.sh:           (Script to open HTML documentation)
|-- remove.sh:            (Script to remove switch and dependencies)

.gitignore:               (List of files to ignore)
.ocamlformat:             (OCamlFormat configuration)
.ocp-indent:              (OcpIndent configuration)
dune:                     (Dune configuration)
dune-project:             (Dune project configuration)
INSTALL.md:               (Installation instructions and information)
makefile:                 (Makefile for using the project)
```

## Credits

Ocamulator was created by Mohammad Khan, Pablo Raigoza, and Evan Vera.
However, we did have a lot of help from the following sources:

- ROM files were downloaded from [Emulator Games](https://www.emulatorgames.net/roms/nintendo/).
- Previously made NES emulators like [Caml Boy](https://linoscope.github.io/writing-a-game-boy-emulator-in-ocaml/)
  and [Nes Emu](https://yizhang82.dev/nes-emu-overview) were used as reference.
- The [NES Dev Wiki](https://www.nesdev.org/wiki/Nesdev) led us to different resources that helped us
  understand the NES architecture.
- Inspiration for project structure and utilities came from previous CS 3110 assignments and labs.
