;
; Testing ChatGPT ability to generate Atari code:
;
; You are an expert in coding for the Atari 8-bit family of computers. Please write a program in 6502 Assembly Language in a format compatible with ca65 which does the following:
; - Load at the 16K cartridge starting address.
; - Initialize the screen with a display list.
;   - The DL should start with one Mode 2 text line, with black background color.
;   - The rest should be 180 lines of 4 color 160 pixels-per-line mode, with black background color.
;     - Q: Comment the code w/r/t considerations for using Atari OS line drawing routines with a custom DL.
;   - The DL will require 7360 bytes for pixels and 40 bytes for the top text line. Reserve this space with the assembler at the appropriate aligned address at the end of the code. Provide labels for the top text line and the first pixel row.
;   - The DL should invoke a simple vertical-blank interrupt (VBI) halfway down the screen.
;     - The VBI should change the color of the background to dark blue.
; - Initialize the Player-Missiles.
; - Use the Player-Missiles to display 5 colored circles that start out moving in random directions and bounce off the edges of the screen. The 4 missiles will be combined into the 5th "player."
; - In the top (text) line of the display, display "1:    2:    3:    4:    5:   " leaving space between the numbered labels.
; - Each time a circle bounces off the edge:
;   - Increment a counter for that circle.
;   - Draw the counter value at the top of the screen next to the appropriate label.

; Atari 8-bit 16K Cartridge Program
; Compatible with ca65 and atari8k.cfg

.include "atari.inc"

; --- Segments ---

.segment "CARTHDR"
	.word RESET              ; cartridge entry point at $BFFE

.segment "CODE"

RESET:
;	brk                      ; open the monitor
	sei
	lda #0
	sta NMIEN                ; disable interrupts
	sta DMACTL               ; disable DMA temporarily
	sta GRACTL               ; disable PMG

	jsr SetupDisplayList
	jsr SetupPMG
	jsr InitCircles
	jsr SetupDLI

	lda #%00111010           ; enable DMA: DL + PMG
	sta DMACTL

	lda #%00000011           ; enable PMG for player + missile
	sta GRACTL

	cli
Forever:
	jmp Forever

; --- Display List Setup ---

SetupDisplayList:
	lda #<DisplayList
	sta SDLSTL
	lda #>DisplayList
	sta SDLSTL+1

	rts

; --- DLI Setup ---

SetupDLI:
	lda #<DLI
	sta VDSLST
	lda #>DLI
	sta VDSLST+1

	lda NMIEN
	ora #%10000000           ; enable DLIs
	sta NMIEN
	rts

SetupVBI:
	lda #<MainVBI
	sta VVBLKI
	lda #>MainVBI
	sta VVBLKI+1

	lda NMIEN
	ora #%01000000           ; enable VBIs
	sta NMIEN
	rts

; --- DLI Handler - Change background color ---

.proc DLI
	pha
	lda #$06                 ; dark blue
	sta COLBK
	pla
	rti
.endproc

; --- Vertical Blank Interrupt - Run game logic ~60Hz ---

.proc MainVBI
	pha
	txa
	pha
	tya
	pha

	jsr MoveCircles
	jsr DrawCircles
	jsr UpdateTopLine

	lda #$00	     ; reset background color
	sta COLBK

	pla
	tay
	pla
	tax
	pla
	rti
.endproc

; --- PMG Setup ---

SetupPMG:
	lda #>PMG_MEM
	sta PMBASE               ; high byte of PMG area

	lda #0
	ldx #0
	ldy #0
ClearPMG:
	sta PMG_MEM,x
	inx
	bne ClearPMG
	iny
	cpy #4
	bne ClearPMG
	rts

; --- Draw all Circle PMs at their current positions ---

.proc DrawCircles
	ldx #0
_loop:
	; --- calculate PMG memory address ---
	lda circle_y,x
	clc
	adc #4              ; offset up to center
	tay

	lda circle_x,x
	sta temp1           ; for HPOS

	; --- store horizontal position ---
        txa
        cmp #4
        bne _setMissile
        lda temp1
        sta HPOSP0        ; player 0 position
        jmp _drawShape

_setMissile:
        lda temp1
        sta HPOSM0,x      ; missile 0–3 position

_drawShape:
	ldy circle_y,x
	ldx #0
	ldy circle_y,x
_dloop:
	lda CircleShape,x
	sta PMG_MEM + $300,y    ; Player 0 shape area
	inx
	iny
	cpx #6
	bne _dloop

	ldx temp1
	inx
	cpx #5
	bne _loop
	rts
.endproc

; --- Draw a Digit. A = counter, X = line offset ---

.proc DrawDigit
	pha
	lsr
	lsr
	lsr
	lsr
	clc
	adc #'0'
	sta TopTextLine,x
	inx
	pla
	and #$0F
	clc
	adc #'0'
	sta TopTextLine,x
	inx
	rts
.endproc

; --- Update the top line ---

.proc UpdateTopLine
	ldx #0
_loop:
	lda DigitLabels,x
	sta TopTextLine,x       ; copy "1:", "2:", etc.
	inx
	lda #$20
	sta TopTextLine,x       ; space after digit
	inx

	lda bounce_ctr,x
	jsr DrawDigit
	inx
	cpx #30
	bcc _loop
	rts
.endproc

DigitLabels:
	.byte '1', ':', ' ', '2', ':', ' ', '3', ':', ' ', '4', ':', ' ', '5', ':', ' '

; --- Circle Init ---

InitCircles:
	lda #$2C     ; red
	sta COLPM0
	lda #$3A     ; orange
	sta COLPM1
	lda #$0C     ; green
	sta COLPM2
	lda #$1A     ; blue
	sta COLPM3

	ldx #0
@loop:
	lda InitX,x
	sta circle_x,x
	lda InitY,x
	sta circle_y,x
	lda InitDX,x
	sta circle_dx,x
	lda InitDY,x
	sta circle_dy,x
	lda #0
	sta bounce_ctr,x
	inx
	cpx #5
	bne @loop
	rts

InitX:   .byte 20, 60, 100, 140, 180
InitY:   .byte 40, 80, 120, 100, 60
InitDX:  .byte 1, 2, 1, 2, 1
InitDY:  .byte 1, 1, 2, 2, 1

; --- MoveCircles ---

.proc MoveCircles
	ldx #0
_loop:
	lda circle_x,x
	clc
	adc circle_dx,x
	sta circle_x,x

	cmp #0
	bcc _bounceX
	cmp #192                 ; right edge limit for PMG (1 color byte)
	bcs _bounceX
	jmp _nox

_bounceX:
	lda circle_dx,x
	eor #$FF
	clc
	adc #1                  ; negate
	sta circle_dx,x

	inc bounce_ctr,x

_nox:
	lda circle_y,x
	clc
	adc circle_dy,x
	sta circle_y,x

	cmp #0
	bcc _bounceY
	cmp #192
	bcs _bounceY
	jmp _noy

_bounceY:
	lda circle_dy,x
	eor #$FF
	clc
	adc #1
	sta circle_dy,x

	inc bounce_ctr,x

_noy:
	inx
	cpx #5
	bne _loop
	rts
.endproc

; --- Display List ---

.segment "RODATA"
	.align $100

DisplayList:
	.byte $70                            ; 1 blank line
	.byte $42, <TopTextLine, >TopTextLine ; Mode 2 + LMS
.repeat 180, I
	.byte $44                            ; Mode 4 (no LMS for now)
.endrep
	.byte $41, <DisplayList, >DisplayList ; jump to self

; --- Screen Memory ---

TopTextLine:
	.res 40                              ; mode 2: 40 bytes
PixelStart:
	.res 7360                            ; 180 * 40

CircleShape:
	.byte %00011000
	.byte %00111100
	.byte %01111110
	.byte %01111110
	.byte %00111100
	.byte %00011000

; --- PMG Memory ---

.segment "PMG"
	.org $D000
PMG_MEM:
	.res 1024

; --- ZP and RAM ---

.segment "ZEROPAGE"
temp1:   .res 1
temp2:   .res 1

.segment "BSS"
circle_x:   .res 5
circle_y:   .res 5
circle_dx:  .res 5
circle_dy:  .res 5
bounce_ctr: .res 5
