# Makefile for AtASM demo "colors.asm" code

ATARI800 := /opt/local/bin/atari800

.PHONY : default run clean

default : colors.65o

colors.65o : colors.asm
	atasm colors.asm

run : colors.65o
	$(ATARI800) -r colors.65o

clean :
	$(RM) *.65o
