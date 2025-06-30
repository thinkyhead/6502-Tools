
.PHONY : test copy default clean

default : ataridemo.xex

view : ataridemo.xex
	hexedit ataridemo.xex

test : ataridemo.xex
	atari800 -tatari -r ataridemo.xex

ataridemo.o : ataridemo.asm
	ca65 ataridemo.asm -l ataridemo.s -o ataridemo.o

ataridemo.xex : ataridemo.o
	cl65 -tatari -o ataridemo.xex -C ataridemo.lnk ataridemo.o

clean :
	$(RM) *.o *~ *.map *.s ataridemo.xex
