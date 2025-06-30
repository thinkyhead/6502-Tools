;
; test.s - 6502 Assembler Test Code
;          for the VASM Versatile Assembler
;
	.org $0602
start:
	lda #<data
	sta $00
	lda #>data
	sta $01

	lda #0
	ldx #25

loop: 	adc #2
	dex
	bne loop

	rts

data: 	.word start

	.word $0201
	.word $0302
	.word $0403
	.word $0504
	.word $0605
	.word $0706
