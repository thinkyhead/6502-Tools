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
	$(RM) *~ *.o *.map *.s bdos.bin TEST.ULST

ATASCII := '$(HOME)/Library/Application Support/Sublime Text 3/Packages/AtariTools/helper/atascii.py'
OUT := TEST.ULST
TEST.ULST : bdos.bin
	@echo '0 DIMT1$$(11),T2$$(11)' >$(OUT)
	@inv_bin=$$( $(ATASCII) -p bdos.bin ); echo "1 INV=ADR(\"$$inv_bin\")" >> $(OUT)
	@echo '2 T1$$="Hello World":T2$$=T1$$' >>$(OUT)
	@echo '3 V=USR(INV,ADR(T2$$),11)' >>$(OUT)
	@echo '4 ?T1$$,T2$$' >>$(OUT)
