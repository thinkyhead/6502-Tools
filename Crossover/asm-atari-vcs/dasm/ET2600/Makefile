# Makefile for 2600 E.T. "ET.asm" DASM code

VCS := /usr/local/include
#VCS := $(HOME)/bin/vcs
#VCS := /opt/homebrew/opt/dasm/machines/atari2600
#VCS := $(HOME)/.vscode/extensions/zehfernando.vscode-dasm-4.0.0/assets
#VCS := $(HOME)/.vscode/extensions/chunkypixel.atari-dev-studio-0.11.9/out/bin/compilers/bB/includes

.PHONY : default clean all pal run runpal

default : ET.bin

# NTSC Versions

ET.bin : ET.asm
	dasm ET.asm -f3 -oET.bin -I$(VCS) -E2

# PAL Versions

ET-pal.bin : ET.asm
	dasm ET.asm -f3 -oET-pal.bin -I$(VCS) -E2 -DCOMPILE_VERSION=1

pal: ET-pal.bin
all: ET.bin ET-pal.bin

run : ET.bin
	open -a Stella ET.bin

runpal : ET-pal.bin
	open -a Stella ET-pal.bin

clean :
	$(RM) ET*.bin
