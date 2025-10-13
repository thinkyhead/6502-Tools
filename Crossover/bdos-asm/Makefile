# Makefile for ca65 demo "bdos" helper code

.PHONY : default view clean

default : bdos.bin

bdos.o : bdos.asm
	ca65 --target atarixl --cpu 6502 bdos.asm -l bdos.s -o bdos.o

bdos.bin : bdos.o
	ld65 -o bdos.bin -C bdos.cfg bdos.o

view : bdos.o
	nano bdos.s

clean :
	$(RM) *.o *~ *.map *.s bdos.bin BDOS.LST

SHELL := /bin/bash
BDOS.LST : bdos.bin
	@echo '0 DIMU$$(30),T1$$(11),T2$$(11)' >BDOS.LST
	@echo -n '1 U$$="' >>BDOS.LST
	@../../Sublime/AtariTools/helper/atascii.py -p bdos.bin >>BDOS.LST
	@echo '"' >>BDOS.LST
	@echo '2 T1$$="Hello World":T2$$=T1$$' >>BDOS.LST
	@echo '3 V=USR(ADR(U$$),ADR(T2$$),11)' >>BDOS.LST
	@echo '4 ?T1$$,T2$$' >>BDOS.LST
