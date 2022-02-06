
.PHONY : test copy default clean

default : bdos.bin

bdos.o : bdos.asm
	ca65 bdos.asm -l bdos.s -o bdos.o

bdos.bin : bdos.o
	ld65 -o bdos.bin -C bdos.lnk bdos.o

clean :
	$(RM) *.o *~ *.map *.s bdos.bin
