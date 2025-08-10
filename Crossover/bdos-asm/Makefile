# Makefile for ca65 demo "bdos" helper code

.PHONY : default view clean

default : bdos.bin

bdos.o : bdos.asm
	ca65 bdos.asm -l bdos.s -o bdos.o

bdos.bin : bdos.o
	ld65 -o bdos.bin -C bdos.cfg bdos.o

view : bdos.o
	nano bdos.s

clean :
	$(RM) *.o *~ *.map *.s bdos.bin
