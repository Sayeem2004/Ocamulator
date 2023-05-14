# Ocamulator

Ocamulator is a NES emulator written in OCaml. It was written from scratch
and was created by Mohammad Khan, Pablo Raigoza, and Evan Vera. Installation and
usage instructions are below along with a more detailed description of the project.

## Installation/Usage

`git clone git@github.com:Sayeem2004/Ocamulator.git` Cloning the repository.

`cd Ocamulator` Entering the repository directory.

`make install` Creating a new opam switch and installing the dependencies.

`make build` Compiling the project.

`make test` Running the test cases.

`make simulate` Running the ROM simulation part of the project. When prompted
enter the name of the file you want to run in ./data/rom with the correct file
extension. This will start stepping through the ROM simulation and display the
CPU state. Press "Enter" to keep stepping CPU or type "quit" or "exit" to end.

`make execute` Running the ROM execution part of the project. When prompted
enter the name of the file you want to run in ./data/rom with the correct file
extension. This will start executing the ROM and display the game graphics.
Close the window to end the execution.

## Goals

- [X] Use Git and GitHub to facilitate our development process.
- [X] Develop the appropriate structure and utilities for a project of this size.
- [X] Find and download example project ROMs from the internet.
- [X] Implement a type system for unsigned integers of different sizes.
- [X] Appropriately test the type system to ensure correctness.
- [X] Create functionality to initialize RAM and CPU state.
- [X] Produce a CLI to display the RAM and CPU states.
- [X] Parse a ROM file and extract relevant instructions.
- [X] Appropriately test the ROM parsing to ensure correctness.
- [X] Write functionality to handle instructions and modify RAM and CPU state.
- [X] Appropriately test the RAM and CPU states to ensure correctness.
- [X] Produce a UI to display the RAM and CPU states after modification.
- [X] Successfully run parser and instruction handler on ROM files.
- [X] Implement PPU functionality and connect it to CPU and RAM.
- [X] Appropriately test the PPU to ensure correctness.
- [X] Display pixel rendering and window functionality.
- [X] Parse a ROM file and extract relevant PPU information and sprites.
- [X] Appropriately test the ROM parsing of PPU to ensure correctness.
- [X] Implement keyboard functionality and connect it to CPU and RAM.
- [X] Modify display to change based on keyboard input.
- [X] Appropriately test keyboard functionality to ensure correctness.
- [X] Test the emulator on a variety of smaller ROM files.
- [X] Share emulator with friends and have a friendly competition.
- [ ] Display the title screen of Super Mario Bros. ROM file.
- [ ] Emulate and complete Super Mario Bros. ROM file.
- [ ] Emulate other games just as complicated as Super Mario Bros.

## Makefile Commands

`make build` Compiles and builds the project.

`make clean` Purges temporary files from the project.

`make test` Runs the test cases for the project.

`make install` Creates a new opam switch and installs the dependencies.

`make remove` Removes the opam switch and dependencies.

`make execute` Runs the main ROM execution part of the project.

`make simulate` Runs the ROM simulation part of the project.

`make format` Formats all `.ml` and `.mli` files in the project.

`make cloc` Counts the number of lines of code in the project.

`make bisect` Generates and opens an HTML report of the test coverage of the project.

`make doc` Generates and opens the HTML documentation for the project.

`make zip` Zips all necessary components of the project.

## Project Structure

```txt
data:                            Contains project data.
|-- opcode:                      Contains test cases for all 256 opcodes.
|-- rom:                         Contains example NES ROMs to run.
|-- exclude.lst:                 List of file to exclude from zip file.
|-- ocamulator.yml:              Project description for public repository.

src:                             Contains project source code.
|-- bin:                         Contains project executables.
|   |-- dune:                    Dune configuration.
|   |-- execute.ml:              Entry point for ROM execution.
|   |-- simulate.ml:             Entry point for ROM simulation.
|-- lib:                         Contains project libraries.
|   |-- cpu:                     Contains CPU related modules.
|   |   |-- cpu.ml:              Module representing the CPU state.
|   |   |-- ram.ml:              Module representing the RAM state.
|   |-- opcode:                  Contains opcode related modules.
|   |   |-- decode.ml:           Module representing the decoding of instructions.
|   |   |-- instruction.ml:      Module representing instruction implementations.
|   |   |-- opcode.ml:           Module representing the execution of opcodes.
|   |-- type:                    Contains type related modules.
|   |   |-- uInt8.ml             Module representing unsigned 8-bit integers.
|   |   |-- uInt16.ml            Module representing unsigned 16-bit integers.
|   |   |-- uIntX.ml             Module representing generic unsigned integers.
|   |-- util                     Contains utility related modules.
|   |   |-- alias.ml             Module representing infix aliases for unsigned integers.
|   |   |-- util.ml              Module representing random utility functions.
|   |-- dune:                    Dune configuration.
|-- test:                        Contains project tests.
|   |-- cpu:                     Contains tests for CPU related modules.
|   |   |-- cpuTest.ml:          Test cases for CPU state.
|   |   |-- ramTest.ml:          Test cases for RAM state.
|   |-- opcode:                  Contains tests for opcode related modules.
|   |   |-- decodeTest.ml:       Test cases for instruction decoding.
|   |   |-- instructionTest.ml:  Test cases for instruction implementations.
|   |   |-- opcodeTest.ml:       Test cases for opcode execution.
|   |-- type:                    Contains tests for type related modules.
|   |   |-- uInt8Test.ml:        Test cases for unsigned 8-bit integers.
|   |   |-- uInt16Test.ml:       Test cases for unsigned 16-bit integers.
|   |   |-- uIntXTest.ml:        Functor of test cases for unsigned integers.
|   |-- util:                    Contains tests for utility related modules.
|   |   |-- aliasTest.ml:        Test cases for infix aliases for unsigned integers.
|   |   |-- utilTest.ml:         Test cases for random utility functions.
|-- .ocamlformat:                OCamlFormat configuration.
|-- .ocp-indent:                 OcpIndent configuration.
|-- dune:                        Dune configuration.
|-- dune-project:                Dune project configuration.
|-- Ocamulator.opam:             Opam package description.

util:                            Contains project utilities
|-- gen:                         Contains project generation utilities.
|   |-- gen.cpp:                 Parses hexdump and converts to opcodes.
|   |-- gen.rs:                  Parses opcodes and converts to NES file.
|   |-- makefile:                Makefile for using the project generation utilities.
|   |-- scrape.py:               Scrapes opcode tests from the web.
|-- script:                      Contains project scripts.
|   |-- install.sh:              Script to install switch and dependencies.
|   |-- opencov.sh:              Script to open HTML coverage report.
|   |-- opendoc.sh:              Script to open HTML documentation.
|   |-- remove.sh:               Script to remove switch and dependencies.

.gitignore:                      List of files for git to ignore.
makefile:                        Makefile for using the project.
README.md:                       Project description and information.
```

## Credits

Ocamulator was created by Mohammad Khan, Pablo Raigoza, and Evan Vera.
However, we did have a lot of help from the following sources:

- Previously made NES emulators like [Caml Boy](https://linoscope.github.io/writing-a-game-boy-emulator-in-ocaml/) and [Nes Emu](https://yizhang82.dev/nes-emu-overview) were used as reference.
- The [NES Dev Wiki](https://www.nesdev.org/wiki/Nesdev) led us to different resources that helped us understand the NES architecture.
- Opcode tests were scraped from Tom Harte's [Processor Tests](https://github.com/TomHarte/ProcessorTests)
- NES assemblers like [Easy 6502](https://skilldrick.github.io/easy6502/) were used to convert games from assembly to hexdump.
- Smaller NES games were downloaded from spacerace's [6502](https://github.com/spacerace/6502/tree/master/target-src) directory.
- Inspiration for project structure and utilities came from previous CS 3110 assignments and labs.
