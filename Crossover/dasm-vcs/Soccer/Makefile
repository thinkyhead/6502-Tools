# Makefile for "soccer.asm" DASM code
# Soccer game for Atari 2600, modified version of 8bitworkshop.com 19th collision example
#
# -tv.filter     <0-5>
# -tv.phosphor   <byrom|always|autoon|auto>
# -tv.phosblend  <0-100>
# -tv.scanlines  <0-100>
# -tv.scanmask   <standard|thin|pixel|mame>
# -tv.sharpness  <-1.0 - 1.0>  Set TV effects custom sharpness
# -tv.resolution <-1.0 - 1.0>  Set TV effects custom resolution
# -tv.artifacts  <-1.0 - 1.0>  Set TV effects custom artifacts
# -tv.fringing   <-1.0 - 1.0>  Set TV effects custom fringing
# -tv.bleed      <-1.0 - 1.0>  Set TV effects custom bleed
# -joydeadzone   <0-29>
# -adeadzone     <0-29>
# -launcherfont  <small|low_medium|medium|large|large12|large14|large16>
# -dialogfont    ""
# -romdir        "/Applications/_Applications/Emulation/Consoles/Atari 2600/Stella"
#
# open -a Stella --args -launcherfont large12 -romdir `pwd`

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
	@open -a Stella $(GAME).bin --args -tv.phosblend 20 -tv.filter 1 -tv.scanlines 40 -tv.scanmask thin -display 2

clean :
	$(RM) $(GAME).bin
