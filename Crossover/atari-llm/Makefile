# Makefile for building Atari 8-bit project with ca65 and running in atari800
# Options to build cartridge or EXE executable

TARGET = ataricode
ASM = ataricode.asm
CFG = ataricode.cfg
XEX = $(TARGET).xex
CAR = $(TARGET).car

.PHONY: all clean test

all: $(XEX)

$(TARGET).o: $(ASM)
	ca65 -o $(TARGET).o $(ASM)

# Executable (in RAM) at location specified in header
# Same as .COM / .EXE / .BIN format for Atari DOS
$(XEX): $(TARGET).o $(CFG)
	ld65 -C $(CFG) -o $(XEX) $(TARGET).o --mapfile $(TARGET).map

test: $(XEX)
	atari800 -atari -nobasic -run $(XEX) -win-width 1600 -win-height 1200

# Cartridge ROM at $C000 - ROMSIZE through $BFFF
$(CAR): $(TARGET).o $(CFG)
	ld65 -C $(CFG) -o $(CAR) $(TARGET).o --mapfile $(TARGET).map

cartest: $(CAR)
	atari800 -atari -nobasic -cart $(CAR) -cart-type 2 -win-width 1600 -win-height 1200

clean:
	rm -f $(TARGET).o $(XEX) $(CAR) $(TARGET).map
