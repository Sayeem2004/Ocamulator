scrape:
	@python scrape.py
	@prettier --write ../../data/opcode &> /dev/null

hex:
	@g++ gen.cpp
	@./a.out < in > out

nes:
	@rustc gen.rs
	@./gen

clean:
	@rm -f a.out gen
	@rm *.nes
