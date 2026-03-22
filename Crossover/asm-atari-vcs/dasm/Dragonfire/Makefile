# Makefile for 2600 Dragonfire "DFIRE.ASM" DASM code

GAME=DFIRE

.PHONY : default clean

default : $(GAME).bin

$(GAME).bin : $(GAME).ASM
	dasm $(GAME).ASM -f3 -o$(GAME).bin -E2

run : $(GAME).bin
	open -a Stella $(GAME).bin

clean :
	$(RM) $(GAME).bin
