# Ocamulator

Ocamulator is a NES emulator written in OCaml. It was written from scratch
and was created by Mohammad Khan, Pablo Raigoza, and Evan Vera. Installation and
usage instructions are below along with a more detailed description of the project.

## Installation/Usage

`git clone git@github.com:Sayeem2004/3110Project.git` Cloning the repository.

`cd 3110Project` Entering the repository directory.

`make install` Creating a new opam switch and installing the dependencies.

`make build` Compiling the project.

`make test` Running the test cases.

`make run_read` To run the hex reading portion of the project. When prompted for
a file, enter the name of the file you want to run in ./data, with the correct
file extension. When prompted from "Step: ", press "Enter" to keep stepping CPU
or type "quit" or "exit" to end.

`make run_main` To run the main emulator of the project (currently a work in
progress, and does nothing).

## Goals

- [X] Use Git and GitHub to facilitate our development process.
- [X] Develop the appropriate structure and utilities for a project of this size.
- [X] Find and download example project ROMs from the internet.
- [X] Implement a type system for unsigned integers of different sizes.
- [X] Appropriately test the type system to ensure correctness.
- [X] Create functionality to initialize RAM and CPU state.
- [X] Produce a CLI to display the RAM and CPU states.
- [X] Parse a ROM file and extract relevant instructions.
- [ ] Appropriately test the ROM parsing to ensure correctness.
- [X] Write functionality to handle instructions and modify RAM and CPU state.
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

info:                     Contains project information
|-- Ocamulator.yml:       (Project description for public repository)

lib:                      Contains project libraries
|-- cpu:                  Contains CPU library
|   |-- cpu.ml:           (Module representing the CPU state)
|   |-- cpu.mli:          (Interface for CPU module)
|   |-- decode.ml:        (Module representing the decoding of instructions)
|   |-- decode.mli:       (Interface for decode module)
|   |-- instructions.ml:  (Module representing OPCodes and instructions)
|   |-- instructions.mli: (Interface for instructions module)
|   |-- opcode.ml:        (Module representing the opcode of an instruction)
|   |-- opcode.mli:       (Interface for opcode module)
|   |-- ram.ml:           (Module representing the RAM state)
|   |-- ram.mli:          (Interface for RAM module)
|-- type:                 Contains type library
|   |-- uInt.ml:          (Generic signature for unsigned integers)
|   |-- uInt.mli:         (Interface for unsigned integer module)
|   |-- uInt8.ml:         (Module representing unsigned 8-bit integers)
|   |-- uInt8.mli:        (Interface for unsigned 8-bit integer module)
|   |-- uInt16.ml:        (Module representing unsigned 16-bit integers)
|   |-- uInt16.mli:       (Interface for unsigned 16-bit integer module)
|   |-- uIntSet.ml:       (Generic signature for bounds of unsigned integers)
|   |-- uIntSet.mli:      (Interface for unsigned integer set module)
|   |-- uIntX.ml:         (Functor for converting bounds into unsigned integers)
|   |-- uIntX.mli:        (Interface for unsigned integer functor module)
|-- dune                  (Dune configuration)

test:                     Contains project tests
|-- cpu:                  Tests for CPU library
|   |-- cpuTest.ml:       (Test cases for CPU state)
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
README.md:                (Project description and information)
```

## Credits

Ocamulator was created by Mohammad Khan, Pablo Raigoza, and Evan Vera.
However, we did have a lot of help from the following sources:

- ROM files were downloaded from
  [Emulator Games](https://www.emulatorgames.net/roms/nintendo/).
- Previously made NES emulators like
  [Caml Boy](https://linoscope.github.io/writing-a-game-boy-emulator-in-ocaml/)
  and [Nes Emu](https://yizhang82.dev/nes-emu-overview) were used as reference.
- The [NES Dev Wiki](https://www.nesdev.org/wiki/Nesdev) led us to different
  resources that helped us understand the NES architecture.
- Inspiration for project structure and utilities came from previous CS 3110
  assignments and labs.
