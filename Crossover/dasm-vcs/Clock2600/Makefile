# Makefile for 2600 Clock "clock003.s" DASM code

GAME=clock003
#VCS := /usr/local/include/vcs
#VCS := $(HOME)/.vscode/extensions/zehfernando.vscode-dasm-4.0.0/assets
VCS := $(HOME)/bin/vcs
STELLA := /Applications/_Applications/Emulation/Consoles/Atari\ 2600/Stella/Stella.app/Contents/MacOS/Stella

.PHONY : default clean

default : $(GAME).bin

$(GAME).bin : $(GAME).s
	dasm $(GAME).s -f3 -o$(GAME).bin -I$(VCS) -E2

run : $(GAME).bin
	$(STELLA) -uselauncher 0 -tv_phos 1 $(GAME).bin

clean :
	$(RM) $(GAME).bin
