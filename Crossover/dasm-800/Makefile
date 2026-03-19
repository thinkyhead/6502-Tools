# Makefile for Atari 800 "cartridge.dasm" DASM code

GAME=cartridge
RUNARGS=-win-width 1600 -win-height 1200
CARTTYPE=1 	# 1 = Standard 8K ; 2 = 16K ; 4 = 32K 5200

.PHONY : default clean

default : $(GAME).car

$(GAME).car : $(GAME).dasm
	dasm $(GAME).dasm -f3 -o$(GAME).car -I$(VCS) -E2

run : $(GAME).car
	atari800 -atari -nobasic -cart-type $(CARTTYPE) -cart $(GAME).car $(RUNARGS)

clean :
	$(RM) $(GAME).car
