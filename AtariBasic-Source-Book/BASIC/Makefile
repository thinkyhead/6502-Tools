
.PHONY : clean check

INCDIR    = ../include

INC = $(INCDIR)/atari.inc $(INCDIR)/gtia.inc \
      $(INCDIR)/pokey.inc $(INCDIR)/pia.inc $(INCDIR)/antic.inc

CA65FLAGS = -l

IMAGES    = ataribas-revA.rom ataribas-revB.rom ataribas-revC.rom

images : $(IMAGES)

%-revA.o : %.asm
	ca65 -DBASIC_REVISION=1 $(CA65FLAGS) $^ -o $@
	@mv ataribas.lst ataribas-revA.lst

%-revB.o : %.asm
	ca65 -DBASIC_REVISION=2 $(CA65FLAGS) $^ -o $@
	@mv ataribas.lst ataribas-revB.lst

%-revC.o : %.asm
	ca65 -DBASIC_REVISION=3 $(CA65FLAGS) $^ -o $@
	@mv ataribas.lst ataribas-revC.lst

%.rom : %.o
	ld65 -C ataribas.lnk $< -o $@

%.dump : %.rom
	od -tx1 -Ax -v $^ > $@


$(IMAGES) : ataribas.lnk $(OBJ) $(INC)

check : $(IMAGES)
	md5sum -cv MD5SUM

clean :
	$(RM) *.o *~ *.lst *.rom *.dump
