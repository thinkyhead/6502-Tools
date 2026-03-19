;
; Soccer game for Atari 2600, modified version of 8bitworkshop.com 19th collision example
;
	processor 6502
	include "vcs.h"
	include "macro.h"
	include "xmacro.h"

	seg.u Variables
	org  $80
counter		byte	; increments each frame
yplyr0		byte	; player y pos
yplyr1		byte	; player y pos
yball		byte	; ball y pos
yball_top	byte	; ball y pos
yball_bot	byte	; ball y pos

ply0_gate_col	byte	;
ply1_gate_col	byte	;
ply0_ball_col	byte	;
ply1_ball_col	byte	;
ply0_sprofs	byte	; sprite data offset
ply1_sprofs	byte	; sprite data offset
ply0_ysprofs	byte	; temp sprite offset
ply1_ysprofs	byte	; temp sprite offset
ply0_score	byte	;
ply1_score	byte	;
score		byte	;

plf_color	byte	;
yballvel	byte	; ball Y velocity
captured	byte	; ball capture flag
avol0	byte	; shadow register for AVOL0

; Color constants
PLCOLOR		equ $0f
GNDCOLOR	equ $c0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Enable ball if it is on this scanline (in X register)
; Modifies A.
	MAC DRAW_BALL
	lda #%00000000

	cpx yball_top
	bmi .noball
	cpx yball_bot
	bpl .noball
	lda #%00000010	; for ENAM0 the 2nd bit is enable
.noball

	sta ENABL	; enable ball
	ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	seg Code
	org $f000

; Initialize and set initial offsets of objects.
Start	CLEAN_START
	lda #0
	sta ply0_score
	sta ply1_score
        lda #127
        sta score

	lda #50
	sta yplyr0	; player Y position, top to bottom
	lda #120
	sta yplyr1	; player Y position, top to bottom

	lda #PLCOLOR
	sta plf_color
; Set player 0 horizontal position
	lda #84
	ldx #0
	jsr SetHorizPos2
; Set player 1 horizontal position
	lda #84
	ldx #1
	jsr SetHorizPos2


; Set ball position
	lda #80
	sta yball
	lda #86
	ldx #4
	jsr SetHorizPos2

	sta WSYNC
	sta HMOVE

; Set ball initial velocity
	lda #4
	sta yballvel

; Next frame loop
NextFrame
	VERTICAL_SYNC

; calculate ball render height
	lda yball
	adc 3
	sta yball_bot
	sbc 6
	sta yball_top

; in case the ball is on screen
	lda ColorBlue	; load 1st entry of color data
	sta COLUP0	; set sprite 0 color
	lda ColorRed	; load 1st entry of color data
	sta COLUP1	; set sprite 0 color
; Set up playfield
	lda #GNDCOLOR	; set the background color
	sta COLUBK
	lda plf_color	; set the playfield color
	sta COLUPF
	lda #%00100001	; playfield reflection and ball size/priority
	sta CTRLPF
	lda #0	; blank out the playfield
	sta PF0
	sta PF1
	sta PF2

        ; Reset collisions
	sta ply0_gate_col
	sta ply1_gate_col

; 37 lines of VBLANK
	ldx #37
Underscan
	sta WSYNC
	dex
	bne Underscan


	ldx #0 ; Line counter

	lda #$00
	sta PF0
	sta PF1
	sta PF2

	lda #GNDCOLOR	; set the background color
	sta COLUBK
DrawGate0
	sta WSYNC

	cpx #20


	bne SkipDrawTopBorder0
	lda CXBLPF
	sta CXCLR
	sta ply0_gate_col
	lda #$0f
	sta PF0
	lda #$ff
	sta PF1
SkipDrawTopBorder0

	lda #Gate,x
	sta PF2

	DRAW_BALL
	inx
	cpx #26


	bne DrawGate0

	lda #0
	sta PF0
	lda #$80
	sta PF1
	lda #0
	sta PF2



	lda yplyr0
	sta ply0_ysprofs

DrawPlayer0
	txa
	sbc ply0_ysprofs

	cmp #SpriteHeight
	bcc InSprite0
	lda #0	; no sprite, draw the padding
InSprite0
	tay
	lda ColorRed,y	; load color data
	pha	; push color data onto stack
	tya
	clc	; clear carry flag
	adc ply0_sprofs	; add animation offset (not for color though)
	tay
	lda Frame0,y	; load bitmap data

	sta WSYNC	; wait for next scanline (as late as possible!)
	sta GRP0	; set sprite 0 pixels
	pla	; pull bitmap data from stack
	sta COLUP0	; set sprite 0 color



	DRAW_BALL	; draw the ball on this line?


	inx
	cpx #95
	bne DrawPlayer0	; repeat next scanline until finished


DrawDivider
	sta WSYNC
	sleep 22
	lda plf_color	; set the background color
	sta COLUBK

	sleep 37
	lda #GNDCOLOR	; set the background color
	sta COLUBK

	inx

	lda yplyr1
	sta ply1_ysprofs

DrawPlayer1
	txa
	sbc ply1_ysprofs

	cmp #SpriteHeight
	bcc InSprite1
	lda #0	; no sprite, draw the padding
InSprite1
	tay
	lda ColorBlue,y	; load color data
	pha	; push color data onto stack
	tya
	clc	; clear carry flag
	adc ply1_sprofs	; add animation offset (not for color though)
	tay
	lda Frame0,y	; load bitmap data

	sta WSYNC	; wait for next scanline (as late as possible!)
	sta GRP1	; set sprite 0 pixels
	pla	; pull bitmap data from stack
	sta COLUP1	; set sprite 0 color

	DRAW_BALL	; draw the ball on this line?

	inx
	cpx #166
	bne DrawPlayer1	; repeat next scanline until finished

	lda CXP0FB
	sta ply0_ball_col
	lda CXP1FB
	sta ply1_ball_col

	sta WSYNC
	lda #$00
	sta PF0
	lda #$FF
	sta PF1
	lda #$01
	sta PF2
	inx

DrawGate1
	sta WSYNC

	cpx #172
	bne SkipDrawTopBorder1

	lda #$00

	sta PF1

	sta CXCLR

SkipDrawTopBorder1

	stx counter

	lda #192
	clc
	sbc counter
	tay

	lda #Gate,y
	sta PF2

	DRAW_BALL
	inx
	cpx #192
	bne DrawGate1

	sta WSYNC
	lda #0
	sta PF0
	sta PF1
	sta PF2

; =================== LOGIC =============================

	TIMER_SETUP 29
	lda CXBLPF
	sta ply1_gate_col

        ; Ignore collisions with the gate if ball moving to opposite side
        ; Avoids duplicate scoring
	ldx 0
        lda yballvel
        cmp 0
        bmi skipGate0Collision
	stx ply0_gate_col
skipGate0Collision

        lda yballvel
        cmp 0
        bpl skipGate1Collision
	stx ply1_gate_col
skipGate1Collision

; Disable ball
	lda #0
	sta ENABL

; Check for collisions
	lda #%01000000
	bit ply0_ball_col
	bne Player0Collision
	bit ply1_ball_col	; collision between player 0 and ball?
	bne Player1Collision


	lda #%10000000
	bit ply0_gate_col	; collision between playfield and ball?
	bne RedGateCollision
	bit ply1_gate_col	; collision between playfield and ball?
	bne BlueGateCollision

	jmp NoCollision
; Now we bounce the ball depending on where it is


Player0Collision
; Is the button pressed? if so, just capture the ball
	lda INPT4	;read button input
	bmi Player0ButtonNotPressed	;skip if button not pressed

	lda #1
	sta captured	; set capture flag
	lda #SpriteHeight
	sta ply0_sprofs	; use different bitmap when captured
	bne NoCollision
Player0ButtonNotPressed
	lda #0
	sta captured	; clear capture flag
	sta ply0_sprofs	; use regular bitmap
	ldx #3
	jmp StoreVel


Player1Collision
; Is the button pressed? if so, just capture the ball
	lda INPT5	;read button input
	bmi Player1ButtonNotPressed	;skip if button not pressed
	lda #2
	sta captured	; set capture flag
	lda #SpriteHeight
	sta ply1_sprofs	; use different bitmap when captured
	bne NoCollision
Player1ButtonNotPressed
	lda #0
	sta captured	; clear capture flag
	sta ply1_sprofs	; use regular bitmap
	ldx #$fd	; top half, bounce up
	jmp StoreVel


RedGateCollision
	inc score
	ldx #$01
	jmp DetermineWinner

BlueGateCollision
	dec score
	ldx #$ff

DetermineWinner
	lda score
	cmp #127
	beq WinnerNone
	cmp #127
	bcs WinnerBlue
WinnerRed
	lda #$3f
	jmp WinnerEnd
WinnerBlue
	lda #$ac
	jmp WinnerEnd
WinnerNone
	lda #$0f
WinnerEnd
	sta plf_color

StoreVel
; Store final velocity
	stx yballvel
; Make a little sound
	txa
	adc #45
	sta AUDF0	; frequency
	lda #6
	sta avol0	; shadow register for volume


NoCollision
; Clear collision registers for next frame
	sta CXCLR
; Ball captured? if so, no motion
	lda captured
	bne ballCaptured
; Move ball vertically
	lda yball
	clc
	adc yballvel
	sta yball
ballCaptured
	sta WSYNC
	sta HMOVE	; apply the move(s)
	sta HMCLR


; Joystick movement - Player 0
	lda #%00010000	;Up?
	bit SWCHA
	bne Pl0SkipMoveUp
	ldx yplyr0
	cpx #26
	bcc Pl0SkipMoveUp
	dex
	stx yplyr0
	lda #1
	bit captured	; captured? move the ball too
	beq Pl0SkipMoveUp
	dec yball
Pl0SkipMoveUp
	lda #%00100000	;Down?
	bit SWCHA
	bne Pl0SkipMoveDown
	ldx yplyr0
	cpx #93-SpriteHeight
	bcs Pl0SkipMoveDown
	inx
	stx yplyr0
	lda #1
	bit captured	; captured? move the ball too
	beq Pl0SkipMoveDown
	inc yball
Pl0SkipMoveDown
	ldx #0	; assume speed is 0 if no movement
	lda #%01000000	;Left?
	bit SWCHA
	bne Pl0SkipMoveLeft
	ldx #$10	;a 1 in the left nibble means go left
Pl0SkipMoveLeft
	lda #%10000000	;Right?
	bit SWCHA
	bne Pl0SkipMoveRight
	ldx #$F0	;a -1 in the left nibble means go right...
Pl0SkipMoveRight
	stx HMP0	; set the move for player 0
	lda #1
	bit captured	; captured? move the ball too

	beq Pl0NoCaptureMove
	stx HMBL	; set ball move register
Pl0NoCaptureMove


; Joystick movement - Player 1
	lda #%00000001	;Up?
	bit SWCHA
	bne Pl1SkipMoveUp
	ldx yplyr1
	cpx #96
	bcc Pl1SkipMoveUp
	dex
	stx yplyr1
	lda #2
	bit captured	; captured? move the ball too
	beq Pl1SkipMoveUp
	dec yball
Pl1SkipMoveUp
	lda #%00000010	;Down?
	bit SWCHA
	bne Pl1SkipMoveDown
	ldx yplyr1
	cpx #163-SpriteHeight
	bcs Pl1SkipMoveDown
	inx
	stx yplyr1
	lda #2
	bit captured	; captured? move the ball too
	beq Pl1SkipMoveDown
	inc yball
Pl1SkipMoveDown
	ldx #0	; assume speed is 0 if no movement
	lda #%00000100	;Left?
	bit SWCHA
	bne Pl1SkipMoveLeft
	ldx #$10	;a 1 in the left nibble means go left
Pl1SkipMoveLeft
	lda #%00001000	;Right?
	bit SWCHA
	bne Pl1SkipMoveRight
	ldx #$F0	;a -1 in the left nibble means go right...

Pl1SkipMoveRight
	stx HMP1	; set the move for player 1
	lda #2
	bit captured	; captured? move the ball too

	beq Pl1NoCaptureMove
	stx HMBL	; set ball move register
Pl1NoCaptureMove


; Play audio from shadow register
	ldx avol0
	beq NoAudio
	dex	; decrement volume every frame
	stx AUDV0	; store in volume hardware register
	stx avol0	; store in shadow register
	lda #4
	sta AUDC0	; shift counter mode 3 for weird bounce sound
NoAudio

; Wait until our timer expires and then WSYNC, so then we'll have
; passed 30 scanlines. This handy macro does this.
	TIMER_WAIT
; Goto next frame
	jmp NextFrame


; SetHorizPos2 - Sets the horizontal position of an object.
; The X register contains the index of the desired object:
;  X=0: player 0
;  X=1: player 1
;  X=2: missile 0
;  X=3: missile 1
;  X=4: ball
; This routine does a WSYNC both before and after, followed by
; a HMOVE and HMCLR. So it takes two scanlines to complete.
SetHorizPos2
	sta WSYNC	; start a new line
	sec	; set carry flag
DivideLoop
	sbc #15	; subtract 15
	bcs DivideLoop	; branch until negative
	eor #7	; calculate fine offset
	asl
	asl
	asl
	asl
	sta RESP0,x	; fix coarse position
	sta HMP0,x	; set fine offset

	sta WSYNC
	sta HMOVE	; apply the previous fine position(s)
	sta HMCLR	; reset the old horizontal position(s)
	rts	; return to caller

; Height of our sprite in lines
SpriteHeight equ 19

; Bitmap data "standing" position
Frame0
	.byte #%00000000;$00
	.byte #%00000000;$00
	.byte #%00111000;$F4
	.byte #%11111110;$F2
	.byte #%01111100;$18
	.byte #%01010100;$18
	.byte #%01111100;$18
	.byte #%01111100;$16
	.byte #%01000100;$16
	.byte #%00111000;$16
	.byte #%00111000;$C2
	.byte #%01111100;$C2
	.byte #%10111010;$C2
	.byte #%10111010;$C2
	.byte #%00111000;$86
	.byte #%00101000;$86
	.byte #%00101000;$86
	.byte #%01101100;$F6
	.byte #%00000000;$00

; Bitmap data "throwing" position
; Increased the size a bit to make sure
; ball doesnt stop colliding while holding
Frame1
	.byte #%00000000;$00
	.byte #%01111100;$00
	.byte #%00111000;$F4
	.byte #%11111110;$F2
	.byte #%01111100;$18
	.byte #%01010100;$18
	.byte #%01111100;$18
	.byte #%01111100;$16
	.byte #%01101100;$16
	.byte #%00111000;$16
	.byte #%00111001;$C2
	.byte #%01111101;$C2
	.byte #%10111101;$C2
	.byte #%10111010;$C2
	.byte #%00111000;$86
	.byte #%00101000;$86
	.byte #%00101000;$86
	.byte #%01101100;$F6
	.byte #%11101110;$00


; Color data for each line of sprite
ColorBlue
	.byte #$00;
	.byte GNDCOLOR;
	.byte #$AC;
	.byte #$AC;
	.byte #$18;
	.byte #$18;
	.byte #$18;
	.byte #$16;
	.byte #$16;
	.byte #$16;
	.byte #$AC;
	.byte #$AC;
	.byte #$AC;
	.byte #$AC;
	.byte #$86;
	.byte #$86;
	.byte #$86;
	.byte #$F6;
	.byte GNDCOLOR;
ColorRed
	.byte #$00;
	.byte GNDCOLOR;
	.byte #$30;
	.byte #$30;
	.byte #$18;
	.byte #$18;
	.byte #$18;
	.byte #$16;
	.byte #$16;
	.byte #$16;
	.byte #$30;
	.byte #$30;
	.byte #$30;
	.byte #$30;
	.byte #$86;
	.byte #$86;
	.byte #$86;
	.byte #$F6;
	.byte GNDCOLOR;

; Playfield top border bitmap
Gate
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%10101011
	.byte #%01010101
	.byte #%10101011
	.byte #%01010101
	.byte #%10101011
	.byte #%01010101
	.byte #%10101011
	.byte #%01010101
	.byte #%10101011
	.byte #%01010101
	.byte #%10101011
	.byte #%01010101
	.byte #%10101011
	.byte #%01010101
	.byte #%10101011
	.byte #%01010101
	.byte #%10101011
	.byte #%01010101
	.byte #%10101011
	.byte #%00000001
	.byte #%00000001
	.byte #%00000001
	.byte #%00000000

; Epilogue
	org $fffc
	.word Start
	.word Start
