# Makefile for "soccer.asm" DASM code
# Soccer game for Atari 2600, modified version of 8bitworkshop.com 19th collision example

GAME=soccer

VCS := /opt/homebrew/opt/dasm/machines/atari2600
#VCS := $(HOME)/bin/vcs
#VCS := /usr/local/include/vcs
#VCS := $(HOME)/.vscode/extensions/zehfernando.vscode-dasm-4.0.0/assets
#VCS := $(HOME)/.vscode/extensions/chunkypixel.atari-dev-studio-0.11.9/out/bin/compilers/bB/includes

#STELLA := "${STELLA_APP}/Contents/MacOS/Stella"

.PHONY : default clean

default : $(GAME).bin

$(GAME).bin : $(GAME).asm
	dasm $(GAME).asm -f3 -o$(GAME).bin -I$(VCS) -E2

run : $(GAME).bin
	@open -a Stella $(GAME).bin --args -tv.phosblend 20 -tv.filter 1 -tv.scanlines 40 -tv.scanmask thin

clean :
	$(RM) $(GAME).bin
