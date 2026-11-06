# Makefile for Raiders of the Lost Ark "raiders.s" DASM code

GAME=raiders

VCS := /opt/homebrew/opt/dasm/machines/atari2600
#VCS := $(HOME)/bin/vcs
#VCS := /usr/local/include/vcs
#VCS := $(HOME)/.vscode/extensions/zehfernando.vscode-dasm-4.0.0/assets
#VCS := $(HOME)/.vscode/extensions/chunkypixel.atari-dev-studio-0.11.9/out/bin/compilers/bB/includes

.PHONY : default clean

default : $(GAME).bin

$(GAME).bin : $(GAME).s
	dasm $(GAME).s -f3 -o$(GAME).bin -I$(VCS) -E2

run : $(GAME).bin
	open -a Stella $(GAME).bin

clean :
	$(RM) $(GAME).bin
