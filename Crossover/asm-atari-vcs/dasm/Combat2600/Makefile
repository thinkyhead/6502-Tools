# Makefile for 2600 Combat "dicombat.asm" DASM code

GAME=dicombat

#VCS := /opt/homebrew/opt/dasm/machines/atari2600
#VCS := $(HOME)/bin/vcs
VCS := /usr/local/include
#VCS := $(HOME)/.vscode/extensions/zehfernando.vscode-dasm-4.0.0/assets
#VCS := $(HOME)/.vscode/extensions/chunkypixel.atari-dev-studio-0.11.9/out/bin/compilers/bB/includes

.PHONY : default clean

default : $(GAME).bin

$(GAME).bin : $(GAME).asm
	dasm $(GAME).asm -f3 -o$(GAME).bin -I$(VCS) -E2

run : $(GAME).bin
	open -a Stella $(GAME).bin

clean :
	$(RM) $(GAME).bin
