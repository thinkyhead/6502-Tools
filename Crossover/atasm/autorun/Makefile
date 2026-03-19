# Makefile for AtASM demo "autorun.asm" code

ATARI800 := /opt/local/bin/atari800

.PHONY : default run clean

default : autorun.65o

autorun.65o : autorun.asm
	atasm autorun.asm

run : autorun.65o
	$(ATARI800) -r autorun.65o

clean :
	$(RM) *.65o
