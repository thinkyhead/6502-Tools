;
; vasm.s - 6502 Assembler Test Code
;          for the VASM Versatile Assembler
;
	*= $0602
start
	lda #<data
	sta $00
	lda #>data
	sta $01

	lda #0
	ldx #25

loop	adc #2
	dex
	bne loop

	rts

data	dw start

	dw $0201
	dw $0302
	dw $0403
	dw $0504
	dw $0605
	dw $0706
