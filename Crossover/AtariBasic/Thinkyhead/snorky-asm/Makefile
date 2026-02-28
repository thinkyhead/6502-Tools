# Makefile for "Snorky" helper code

.PHONY : default view-init view-move view-guy view-cube snorky-gfx mclean clean

default : all-bin

all-bin : snorky-init.bin snorky-move.bin snorky-guy.bin snorky-cube.bin snorky-gfx

# Snorky Init Code

snorky-init.o : snorky-init.asm
	ca65 --target atarixl --cpu 6502 snorky-init.asm -l snorky-init.s -o snorky-init.o

snorky-init.bin : snorky-init.o
	ld65 -o snorky-init.bin -C snorky.cfg snorky-init.o

view-init : snorky-init.o
	nano snorky-init.s

# Snorky Test Sprite Code

snorky-move.o : snorky-move.asm
	ca65 --target atarixl --cpu 6502 snorky-move.asm -l snorky-move.s -o snorky-move.o

snorky-move.bin : snorky-move.o
	ld65 -o snorky-move.bin -C snorky.cfg snorky-move.o

view-move : snorky-move.o
	nano snorky-move.s

# Snorky Guy Sprite Code

snorky-guy.o : snorky-guy.asm
	ca65 --target atarixl --cpu 6502 snorky-guy.asm -l snorky-guy.s -o snorky-guy.o

snorky-guy.bin : snorky-guy.o
	ld65 -o snorky-guy.bin -C snorky.cfg snorky-guy.o

view-guy : snorky-guy.o
	nano snorky-guy.s

# Snorky Cube Code

snorky-cube.o : snorky-cube.asm
	ca65 --target atarixl --cpu 6502 snorky-cube.asm -l snorky-cube.s -o snorky-cube.o

snorky-cube.bin : snorky-cube.o
	ld65 -o snorky-cube.bin -C snorky.cfg snorky-cube.o

view-cube : snorky-cube.o
	nano snorky-cube.s

# Snorky Graphics Data

snorky-gfx: snorky-gfx1.bin snorky-gfx2.bin

snorky-gfx1.o : snorky-gfx1.asm
	ca65 --target atarixl --cpu 6502 snorky-gfx1.asm -l snorky-gfx1.s -o snorky-gfx1.o

snorky-gfx1.bin : snorky-gfx1.o
	ld65 -o snorky-gfx1.bin -C snorky.cfg snorky-gfx1.o

snorky-gfx2.o : snorky-gfx2.asm
	ca65 --target atarixl --cpu 6502 snorky-gfx2.asm -l snorky-gfx2.s -o snorky-gfx2.o

snorky-gfx2.bin : snorky-gfx2.o
	ld65 -o snorky-gfx2.bin -C snorky.cfg snorky-gfx2.o

mclean :
	$(RM) *~ *.o *.map *.s *.bin

clean : mclean
	$(RM) TEST.LST

ATASCII := ../../../../Sublime/AtariTools/helper/atascii.py
OUT := TEST.LST

TEST.LST : all-bin
	@printf '%s\n' \
		'0 N0=0:N1=1:N10=10:JOY=2:GOS.20' \
		'1 E=USR(MOVE,P1,7,X,OY,Y):OY=Y' \
		'2 S=PEEK(632):G.S' \
		'5 X=X+N1:Y=Y+N1:G.N1' \
		'6 X=X+N1:Y=Y-N1:G.N1' \
		'7 X=X+N1:G.N1' \
		'9 X=X-N1:Y=Y+N1:G.N1' \
		'10 X=X-N1:Y=Y-N1:G.N1' \
		'11 X=X-N1:G.N1' \
		'13 Y=Y+N1:G.N1' \
		'14 Y=Y-N1:G.N1' \
		'15 POK.77,N0:G.JOY' >$(OUT)

# 	Use shorter variable names to get a few more bytes to fit
	@init_bin=$$( $(ATASCII) -p snorky-init.bin ); echo "20 INIT=ADR(\"$$init_bin\")" >>$(OUT)
	@move_bin=$$( $(ATASCII) -p snorky-move.bin ); echo "21 MOVE=ADR(\"$$move_bin\")" >>$(OUT)
	@guy_bin=$$( $(ATASCII) -p snorky-guy.bin ); echo "22 SGUY=ADR(\"$$guy_bin\")" >>$(OUT)
	@cube_bin=$$( $(ATASCII) -p snorky-cube.bin ); echo "23 CUBE=ADR(\"$$cube_bin\")" >>$(OUT)
	@gfx1_bin=$$( $(ATASCII) -p snorky-gfx1.bin ); echo "24 GGUY=ADR(\"$$gfx1_bin\")" >>$(OUT)
	@gfx2_bin=$$( $(ATASCII) -p snorky-gfx2.bin ); echo "25 GFX2=ADR(\"$$gfx2_bin\")" >>$(OUT)

	@printf '%s\n' \
		'30 RAMTOP=PEEK(106):PMB=RAMTOP-16:POK.106,PMB:GR.15+16' \
		'31 PMBASE=PMB*256:P0=PMBASE+1024:P1=P0+256:E=USR(INIT,PMBASE)' \
		'32 X=128:Y=100:OY=Y:POK.704,168:POK.705,104' \
		'33 POK.P1+Y,8:POK.P1+Y+N1,28:POK.P1+Y+2,62:POK.P1+Y+3,28:POK.P1+Y+4,8:POK.53249,X-4' \
		'40 M1=80:M2=M1:F.CY=N10 TO170STEPN10:F.CX=M1 TOM2 STEP8:E=USR(CUBE,CX,CY):N.CX:M1=M1-4:M2=M2+4:N.CY' \
		'41 E=USR(CUBE,80,N10,85):E=USR(CUBE,72,30,170):E=USR(CUBE,88,30,255)' \
		'50 E=USR(SGUY,GGUY,N0,N0,P0,88,30)' \
		'60 RET.' >>$(OUT)
