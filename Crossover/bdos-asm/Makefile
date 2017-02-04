
.PHONY : test copy default clean

default : bdos.bin

bdos.bin : bdos.o bdos.lnk
	ld65 -o bdos.bin -C bdos.lnk bdos.o

bdos.o : bdos.asm
	ca65 -l bdos.s bdos.asm -o bdos.o

clean :
	$(RM) *.o *~ *.map *.s bdos.bin
