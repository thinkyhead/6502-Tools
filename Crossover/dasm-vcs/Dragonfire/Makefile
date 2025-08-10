# Makefile for 2600 Dragonfire "DFIRE.ASM" DASM code

GAME=DFIRE
#VCS := /usr/local/include/vcs
#VCS := $(HOME)/.vscode/extensions/zehfernando.vscode-dasm-4.0.0/assets
VCS := $(HOME)/bin/vcs
STELLA := /Applications/_Applications/Emulation/Consoles/Atari\ 2600/Stella/Stella.app/Contents/MacOS/Stella

.PHONY : default clean

default : $(GAME).bin

$(GAME).bin : $(GAME).ASM
	dasm $(GAME).ASM -f3 -o$(GAME).bin -I$(VCS) -E2

run : $(GAME).bin
	$(STELLA) -uselauncher 0 -tv_phos 1 $(GAME).bin

clean :
	$(RM) $(GAME).bin
