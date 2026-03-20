# Makefile for VASM demo code

.PHONY : default run clean

default : vasm.o

vasm.o : vasm.asm
	vasm vasm.asm -o vasm.o

vasm.vobj : vasm.asm
	vasm vasm.asm  -Fvobj -o vasm.vobj

dump : vasm.vobj
	vobjdump vasm.vobj

run : vasm.bin
	atari800 -r vasm.bin

clean :
	$(RM) vasm.o vasm.vobj vasm.bin
