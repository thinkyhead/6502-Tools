# Makefile for 2600 E.T. "ET.asm" DASM code

VCS := /usr/local/include
#VCS := $(HOME)/bin/vcs
#VCS := /opt/homebrew/opt/dasm/machines/atari2600
#VCS := $(HOME)/.vscode/extensions/zehfernando.vscode-dasm-4.0.0/assets
#VCS := $(HOME)/.vscode/extensions/chunkypixel.atari-dev-studio-0.11.9/out/bin/compilers/bB/includes

.PHONY : default clean all p0 p1 p0-pal p1-pal run run1 runpal runpal1

default : ET.bin

# NTSC Versions

ET.bin : ET.asm
	dasm ET.asm -f3 -oET.bin -I$(VCS) -E2

ET-p1.bin : ET.asm
	dasm ET.asm -f3 -oET-p1.bin -I$(VCS) -E2 -DCOMPILE_BUGFIXES=1

p0: ET.bin
p1: ET-p1.bin

# PAL Versions

ET-pal.bin : ET.asm
	dasm ET.asm -f3 -oET-pal.bin -I$(VCS) -E2 -DCOMPILE_VERSION=1

ET-p1-pal.bin : ET.asm
	dasm ET.asm -f3 -oET-p1-pal.bin -I$(VCS) -E2 -DCOMPILE_BUGFIXES=1 -DCOMPILE_VERSION=1

p0-pal: ET-pal.bin
p1-pal: ET-p1-pal.bin

all: p0 p1 p0-pal p1-pal

run : ET.bin
	open -a Stella ET.bin

run1 : p1
	open -a Stella ET-p1.bin

runpal : ET-pal.bin
	open -a Stella ET-pal.bin

runpal1 : p1
	open -a Stella ET-p1-pal.bin

clean :
	$(RM) ET*.bin
