		; **********************************
		; **                              **
		; **  ATARI BASIC ROM: A000-BFFF  **
		; **                              **
		; **********************************

LOMEM		=	$80	; pointer to BASIC's low memory
VNTP		=	$82	; start of variable name table
VNTD		=	$84	; end of variable name table
VVTP		=	$86	; start of variable value table
STMTAB		=	$88	; start of the statement table
STMCUR		=	$8A	; current statement pointer
STARP		=	$8C	; strings/arrays pointer
RUNSTK		=	$8E	; runtime stack
MEMTOP		=	$90	; memtop
POKADR		=	$95	; address of last POKE location
DATAD		=	$B6	; data element being read
DATALN		=	$B7	; data statement line number
STOPLN		=	$BA	; line where the program was stopped
ERRSAVE		=	$C3	; number of the error code
PTABW		=	$C9	; number of columns between tab stops

		.org $A000
		.segment "A000BFFF"

		.include "../include/atari.inc"
		.include "../include/pokey.inc"


		.macro	BasicString stringval
		.repeat	.strlen (stringval)-1,i
		.byte	.strat (stringval,i)
		.endrep
		.byte	.strat (stringval,.strlen(stringval)-1) | $80
		.endmacro

		.macro WordBasicString wordval,stringval
		.word wordval
		BasicString stringval
		.endmacro

A000:		lda	$CA
		bne	@1
		lda	WARMST
		bne	L3
@1:		ldx	#$FF
		txs
		cld

A00C:		ldx	MEMLO		; execute NEW
		ldy	MEMLO+1
		stx	LOMEM
		sty	LOMEM+1
		lda	#0
		sta	$92
		sta	$CA
		iny
		txa

		ldx	#$82
@2:		sta	0,X
		inx
		sty	0,X
		inx
		cpx	#$92
		bcc	@2

		ldx	#$86
		ldy	#1
		jsr	A87A
		ldx	#$8C
		ldy	#3
		jsr	A87A
		lda	#0
		tay
		sta	(VNTD),Y
		sta	(STMCUR),Y
		iny
		lda	#$80
		sta	(STMCUR),Y
		iny
		lda	#3
		sta	(STMCUR),Y
		lda	#10		;  tab stops -> 10
		sta	PTABW
L3:		jsr	B8F1
A050:		jsr	BD45
A053:		jsr	BD5B
		lda	$92
		beq	A05D
		jsr	BD9D
A05D:		jsr	BD62

A060:		lda	$CA
		bne	A000
		ldx	#$FF
		txs
		jsr	__DA51
		lda	#$5D
		sta	$C2
		.if	BASIC_REVISION = 1
		jsr	BA92
		jsr	A9F2
		bne	A060
		.else
		jsr	BDED
		jsr	A9F2
		beq	A060
		.endif

		lda	#0
		sta	CIX
		sta	$9F
		sta	$94
		sta	$A6
		sta	$B3
		sta	$B0
		sta	$B1

		lda	VNTD
		sta	$AD
		lda	VNTD+1
		sta	$AE

		jsr	SKIP_SPACES
		jsr	A19A
		jsr	A2C4
		lda	FR0+1
		bpl	@4
		sta	$A6
@4:		jsr	SKIP_SPACES
		ldy	CIX
		sty	$A8
		lda	(INBUFF),Y
		cmp	#$9B
		bne	A0B1
		bit	$A6
		bmi	A060
		jmp	A186

A0B1:		lda	$94
		sta	$A7
		jsr	A2C4
		jsr	SKIP_SPACES
		lda	#>A49F
		ldy	#<A49F
		ldx	#2
		jsr	A454
		stx	CIX
		lda	$AF
		jsr	A2C4
		jsr	SKIP_SPACES
		jsr	A1BE
		bcc	L7
		ldy	$9F
		lda	(INBUFF),Y
		cmp	#$9B
		bne	@5
		iny
		sta	(INBUFF),Y
		dey
		lda	#$20
@5:		ora	#$80
		sta	(INBUFF),Y
		lda	#$40
		ora	$A6
		sta	$A6
		ldy	$A8
		sty	CIX
		ldx	#3
		stx	$A7
		inx
		stx	$94
		lda	#$37
L6:		jsr	A2C4
A0FB:		ldy	CIX
		lda	(INBUFF),Y
		inc	CIX
		cmp	#$9B
		bne	L6
		jsr	A2C4
L7:		lda	$94
		ldy	$A7
		sta	(LOMEM),Y
		ldy	CIX
		dey
		lda	(INBUFF),Y
		cmp	#$9B
		bne	A0B1
		ldy	#2
		lda	$94
		sta	(LOMEM),Y
		jsr	A9A2
		lda	#0
		bcs	@8
		jsr	A9DC
@8:		sec
		sbc	$94
		beq	@10
		bcs	@9
		eor	#$FF
		tay
		iny
		ldx	#$8A
		jsr	A87A
		lda	$97
		sta	STMCUR
		lda	$98
		sta	STMCUR+1
		bne	@10

@9:		.if	BASIC_REVISION = 1
		pha
		jsr	A9D0
		pla
		tay
		.else
		tay
		jsr	A9D0
		.endif

		ldx	#$8A
		jsr	A8F8
@10:		ldy	$94
@11:		dey
		lda	(LOMEM),Y
		sta	(STMCUR),Y
		tya
		bne	@11
		bit	$A6
		bvc	L13
		lda	$B1
		asl
		asl
		asl

		.if	BASIC_REVISION = 1
		tay
		.endif

		ldx	#$88
		jsr	A8F7
		sec
		lda	VNTD
		sbc	$AD
		tay
		lda	VNTD+1
		sbc	$AE
		ldx	#$84
		jsr	A8FA
		bit	$A6
		bpl	@12
		jsr	B5AA
		jmp	A060
@12:		jsr	B58E		

A17E:		jmp	A060
L13:		bpl	A17E
		jmp	A95E

A186:		jsr	A9A2
		bcs	A17E
		jsr	A9DC

		.if	BASIC_REVISION = 1
		pha
		jsr	A9D0
		pla
		tay
		.else
		tay
		jsr	A9D0
		.endif

		ldx	#$8A
		jsr	A8F8
		jmp	A060

A19A:		jsr	AFP
		bcc	@2
@1:		lda	#0
		sta	CIX
		ldy	#$80
		bmi	@3
@2:		jsr	AD41
		ldy	FR0+1
		bmi	@1
		lda	FR0
@3:		sty	$A1
		sta	$A0
		jsr	A2C4
		lda	$A1
		sta	FR0+1
		jmp	A2C4

A1BE:		ldy	#1
		lda	(POKADR),Y
		sta	$9E
		sta	$483
		dey
		lda	(POKADR),Y
		sta	$9D
		sta	$482
		.if	BASIC_REVISION = 1
		lda	#0
		sta	$A9
		.else
		sty	$A9
		.endif
		lda	$94
		sta	$481
		lda	CIX
		sta	$480
A1DB:		jsr	A293
		bmi	@2
		cmp	#1
		bcc	@4
		bne	@1
		jsr	@4

		.if	BASIC_REVISION = 1
		bcc	A1DB
		jmp	A25E
		.else
		jmp	A259
		.endif

@1:		cmp	#5
		bcc	A245
		jsr	A29B

		.if	BASIC_REVISION = 1
		bcc	A1DB
		jmp	A25E
		.else
		jmp	A259
		.endif

@2:		sec

		.if	BASIC_REVISION = 1
		ldx	#0
		.endif

		sbc	#$C1
		bcs	@3
		ldx	#$FF
@3:		clc
		adc	$9D
		pha
		txa
		adc	$9E
		pha
		jmp	A21B

@4:		jsr	A293
		pha
		jsr	A293
		pha
		bcc	A21B
		pla
		tay
		pla
		tax
		tya
		pha
		txa
		pha
A21A:		rts

A21B:		ldx	$A9
		inx
		inx
		inx
		inx
		beq	@1
		stx	$A9
		lda	CIX
		sta	$480,X
		lda	$94
		sta	$481,X
		lda	$9D
		sta	$482,X
		lda	$9E
		sta	$483,X
		pla
		sta	$9E
		pla
		sta	$9D
		jmp	A1DB
@1:		jmp	B918

A245:		ldx	$A9

		.if	BASIC_REVISION = 1
		bne	@1
		rts
		.else
		beq	A21A
		.endif

@1:		lda	$482,X
		sta	$9D
		lda	$483,X
		sta	$9E
		dex
		dex
		dex
		dex
		stx	$A9
A259:		bcs	A25E
		jmp	A1DB

A25E:		jsr	A293
		bmi	A25E
		cmp	#2
		bcs	A26F
		jsr	A28C
		jsr	A28C
		bne	A25E
A26F:		cmp	#3
		beq	A245
		bcs	A25E
		lda	CIX
		cmp	$9F
		bcc	A27D
		sta	$9F
A27D:		ldx	$A9
		lda	$480,X
		sta	CIX
		lda	$481,X
		sta	$94
		jmp	A1DB

A28C:		inc	$9D
		bne	A292
		inc	$9E
A292:		rts

A293:		jsr	A28C
		ldx	#0
		lda	($9D,X)
		rts

A29B:		cmp	#$0F
		beq	A2B6
		bcs	A2E1

		.if	BASIC_REVISION > 1
		cmp	#$0D
		bne	A2AB
		jsr	A28C
		jmp	A2E4
		.endif

A2AB:		pla
		pla

		lda	#<(A605-1)
		pha
		lda	#>(A605-1)
		pha

		jmp	A21B

A2B6:		jsr	A28C
		ldy	#0
		lda	($9D),Y
		ldy	$94
		dey
		sta	(LOMEM),Y
		clc
A2C3:		rts

A2C4:		ldy	$94
		sta	(LOMEM),Y
		inc	$94

		.if	BASIC_REVISION = 1
		beq	@1
		rts
		.else
		bne	A2C3
		.endif

@1:		jmp	B918

A2CF:		ldx	#$FF
		txs
		lda	$94
		ldy	$A7
		sta	(LOMEM),Y
		jmp	A0B1

A2DB:		ldx	#$FF
		txs
		jmp	A0FB

A2E1:		jsr	SKIP_SPACES
A2E4:		lda	CIX
		cmp	$B3
		beq	@1
		sta	$B3
		lda	#>A7DE
		ldy	#<A7DE
		ldx	#0
		jsr	A454
		bcs	@5
		stx	$B2

		.if	BASIC_REVISION = 1
		clc
		.endif

		lda	$AF
		adc	#$10
		sta	$B0
@1:		ldy	#0
		lda	($9D),Y
		cmp	$B0
		beq	@4
		cmp	#$44

		.if	BASIC_REVISION = 1
		bne	@2
		lda	$B0
		cmp	#$44
		bcs	@3
@2:		sec
		rts
@3:		lda	$B0
		.else
		bne	@6
		lda	$B0
		cmp	#$44
		bcc	@6
		.endif

@4:		jsr	A2C4
		ldx	$B2
		stx	CIX
		clc
		rts

@5:		lda	#0
		sta	$B0
@6:		sec
		rts

A320:		lda	#0
		beq	A326

A324:		lda	#$80
A326:		sta	$D2
		jsr	SKIP_SPACES
		lda	CIX
		sta	$AC
		jsr	A3E8
		bcs	@2
		jsr	A2E1
		lda	$B0
		beq	@1
		ldy	$B2
		lda	(INBUFF),Y
		cmp	#$30
		bcc	@2
@1:		inc	CIX
		jsr	A3E8
		bcc	@1
		jsr	__DBAF
		bcc	@1
		lda	(INBUFF),Y
		cmp	#$24
		beq	@3
		bit	$D2
		bpl	@4
@2:		sec
		rts

@3:		bit	$D2
		bpl	@2
		iny
		bne	@5
@4:		lda	(INBUFF),Y
		cmp	#$28
		bne	@5
		iny
		lda	#$40
		ora	$D2
		sta	$D2
@5:		lda	$AC
		sta	CIX
		sty	$AC
		lda	VNTP+1
		ldy	VNTP
		ldx	#0
		jsr	A454
@6:		bcs	@7
		cpx	$AC
		beq	@11
		jsr	A482
		jmp	@6

@7:		sec
		lda	$AC
		sbc	CIX
		sta	CIX
		tay
		ldx	#$84
		jsr	A87A
		lda	$AF
		sta	$D3
		ldy	CIX
		dey
		ldx	$AC
		dex
@8:		lda	$580,X
		sta	($97),Y
		dex
		dey
		bpl	@8
		ldy	CIX
		dey
		lda	($97),Y
		ora	#$80
		sta	($97),Y
		ldy	#8
		ldx	#$88
		jsr	A87A
		inc	$B1
		ldy	#2
		lda	#0
@9:		sta	$D2,Y
		iny
		cpy	#8
		bcc	@9
		dey
@10:		lda	$D2,Y
		sta	($97),Y
		dey
		bpl	@10
@11:		bit	$D2
		bvc	@12
		dec	$AC
@12:		lda	$AC
		sta	CIX
		lda	$AF
		bmi	@13
		ora	#$80
		.if	BASIC_REVISION = 1
		jsr	A2C4
		clc
		rts
		.else
		clc
		jmp	A2C4
		.endif

@13:		jmp	B92C

A3E8:		ldy	CIX
		lda	(INBUFF),Y
A3EC:		cmp	#$41
		bcc	A3F3
		cmp	#$5B
		rts
A3F3:		sec
		rts

A3F5:		jsr	SKIP_SPACES
		lda	CIX
		sta	$AC
		jsr	AFP
		bcc	@1
		lda	$AC
		sta	CIX
		rts

@1:		lda	#$E
		jsr	A2C4

		.if	BASIC_REVISION = 1
		ldy	$94
		.else
		iny
		.endif

		ldx	#0
@2:		lda	FR0,X
		sta	(LOMEM),Y
		iny
		inx
		cpx	#6
		bcc	@2
		sty	$94
		clc
		rts

A41C:		jsr	SKIP_SPACES
		ldy	CIX
		lda	(INBUFF),Y
		cmp	#$22

		.if	BASIC_REVISION = 1
		beq	@1
		sec
		rts
		.else
		bne	A3F3
		.endif

@1:		lda	#15
		jsr	A2C4
		lda	$94
		sta	$AB
		jsr	A2C4
@2:		inc	CIX
		ldy	CIX
		lda	(INBUFF),Y
		cmp	#$9B
		beq	@4
		cmp	#$22
		beq	@3
		jsr	A2C4
		jmp	@2

@3:		inc	CIX
@4:		clc
		lda	$94
		sbc	$AB
		ldy	$AB
		sta	(LOMEM),Y
A452:		clc
		rts

A454:		stx	$AA
		ldx	#$FF
		stx	$AF
A45A:		sta	POKADR+1
		sty	POKADR
		inc	$AF
		ldx	CIX
		ldy	$AA
		lda	(POKADR),Y
		beq	err
		lda	#0
		php
A46B:		lda	$580,X
		and	#$7F
		cmp	#$2E
		beq	A48F
A474:		eor	(POKADR),Y
		asl
		beq	A47B
		pla
		php
A47B:		iny
		inx
		bcc	A46B
		plp
		.if	BASIC_REVISION = 1
		beq	A48D
		.else
		beq	A452
		.endif
A482:		clc
		tya
		adc	POKADR
		tay
		lda	POKADR+1
		adc	#0
		bne	A45A
A48D:		.if	BASIC_REVISION = 1
		clc
		rts
		.endif
err:		sec
		rts
	
A48F:		lda	#2
		cmp	$AA
		bne	A474
A495:		lda	(POKADR),Y
		bmi	A49C
		iny
		bne	A495
A49C:		sec
		bcs	A47B

A49F:		WordBasicString A7C3-1,"REM"
		WordBasicString A7C6-1,"DATA"
		WordBasicString A6EF-1,"INPUT"
		WordBasicString A6B8-1,"COLOR"
		WordBasicString A72D-1,"LIST"
		WordBasicString A71E-1,"ENTER"
		WordBasicString A6BB-1,"LET"
		WordBasicString A78F-1,"IF"
		WordBasicString A6CD-1,"FOR"
		WordBasicString A6E5-1,"NEXT"
		WordBasicString A6B8-1,"GOTO"
		WordBasicString A6B8-1,"GO TO"
		WordBasicString A6B8-1,"GOSUB"
		WordBasicString A6B8-1,"TRAP"
		WordBasicString A6B9-1,"BYE"
		WordBasicString A6B9-1,"CONT"
		WordBasicString A75A-1,"COM"
		WordBasicString A71B-1,"CLOSE"
		WordBasicString A6B9-1,"CLR"
		WordBasicString A6B9-1,"DEG"
		WordBasicString A75A-1,"DIM"
		WordBasicString A6B9-1,"END"
		WordBasicString A6B9-1,"NEW"
		WordBasicString A714-1,"OPEN"
		WordBasicString A71E-1,"LOAD"
		WordBasicString A71E-1,"SAVE"
		WordBasicString A73B-1,"STATUS"
		WordBasicString A744-1,"NOTE"
		WordBasicString A744-1,"POINT"
		WordBasicString A712-1,"XIO"
		WordBasicString A75D-1,"ON"
		WordBasicString A757-1,"POKE"
		WordBasicString A6F7-1,"PRINT"
		WordBasicString A6B9-1,"RAD"
		WordBasicString A6F0-1,"READ"
		WordBasicString A6EA-1,"RESTORE"
		WordBasicString A6B9-1,"RETURN"
		WordBasicString A721-1,"RUN"
		WordBasicString A6B9-1,"STOP"
		WordBasicString A6B9-1,"POP"
		WordBasicString A6F7-1,"?"
		WordBasicString A6E3-1,"GET"
		WordBasicString A6B5-1,"PUT"
		WordBasicString A6B8-1,"GRAPHICS"
		WordBasicString A757-1,"PLOT"
		WordBasicString A757-1,"POSITION"
		WordBasicString A6B9-1,"DOS"
		WordBasicString A757-1,"DRAWTO"
		WordBasicString A755-1,"SETCOLOR"
		WordBasicString A6DD-1,"LOCATE"
		WordBasicString A753-1,"SOUND"
		WordBasicString A6FB-1,"LPRINT"
		WordBasicString A6B9-1,"CSAVE"
		WordBasicString A6B9-1,"CLOAD"

		.word	A6BB-1
		.byte	$00,$80		; silent "LET"
	
		.byte	$00,$2A
		BasicString "ERROR-  "

		.if	BASIC_REVISION > 1
STOPPED_STR:	BasicString "STOPPED "
		.endif	
	
A605:		.if	BASIC_REVISION = 1
		.byte   $2B,$BF,$2C,$DE,$02,$C6,$BA,$02
		.else
		.byte	$CD,$C4,$02,$C2,$03,$2B,$BA,$2C,$DB,$02
		.endif

		.byte	$CD,$D8,$03,$25,$0F,$35,$02,$26,$0F,$36,$02,$28,$03

		.if	BASIC_REVISION = 1
		.word	$2FD
		.else
		.word	$2FE
		.endif
		.byte	$E8,$02,$01
		.word	A3F5-1
		.byte	$02,$00
		.word	A679-1
		.byte	$03,$C4
		.if	BASIC_REVISION = 1
		.word	$29E
		.else
		.word	$29C
		.endif
		.byte	$03,$23,$02,$25,$02,$26,$02,$24,$02,$27,$02,$1D,$02,$1F,$02,$1E
		.byte	$02,$20,$02,$21,$02,$22,$02,$2A,$02,$29,$03

A646:		.byte	$01
		.word	A320-1
		.byte	$C2,$03
		.if	BASIC_REVISION > 1
		.byte	$0D
		.endif
		.byte	$2B,$0F,$38,$0E,$C4,$2C,$02
		.byte	$03

A654:		.byte	$12,$0F,$3C,$0E,$02,$03,$44,$D2,$02,$00
		.word	A7CE-1
		.byte	$D3,$02,$C2,$03,$3F,$2B,$0F,$3A,$00
		.word	A7D5-1
		.byte	$2C,$03,$2B,$0F,$3A,$0E,$2C,$03,$2B,$0F,$3A,$C7,$2C
		.byte	$03

A679:		.byte	$C4,$E3,$C2
		.byte	$03

A67D:		.byte	$C8,$02,$CB,$02,$01

		.word	A41C-1

		.byte	$03,$00

		.word	A7D1-1

		.byte	$A5,$03,$01
		.word	A324-1
		.byte	$C2,$03,$2B,$0F,$37,$0E,$C4,$2C,$02,$03,$12,$0F,$3C,$0E,$02,$03
		.byte	$1D,$0F,$2F,$02,$1E,$0F,$30,$02,$1F,$0F,$31,$02,$20,$0F,$32,$02
		.byte	$21,$0F,$33,$02,$22,$0F,$34
		.byte	$03

		; PUT statement

A6B5:		.byte	$1C
		.byte	$0E,$12

		; COLOR, GOTO, GO TO, GOSUB, TRAP, GRAPHICS statement

A6B8:		.byte	$0E

		; BYE, CONT, CLR, DEG, END, NEW, RAD, RETURN,
		; STOP, POP, DOS, CSAVE, CLOAD statement

A6B9:		.byte	$FA,$03

		; LET, silent LET statement

A6BB:		.byte	$00
		.word	A646-1
		.byte	$22,$0F,$2D,$0E,$F1,$02,$86,$22,$0F,$2E,$00
		.word	A67D-1
		.byte	$E8,$03

		; FOR statement

A6CD:		.byte	$01
		.word	A320-1
		.byte	$22,$0F,$2D,$0E,$19,$0E,$C3,$DC,$03,$1A,$0E,$02,$03

		; LOCATE statement

A6DD:		.byte	$0E,$12
		.byte	$0E,$12
		.byte	$C4,$03

		; GET statement

A6E3:		.byte	$DD,$12

		; NEXT statement

A6E5:		.byte	$01
		.word	A320-1
		.byte	$CB,$03

		; RESTORE statement

A6EA:		.byte	$0E,$C8,$02,$C6,$03

		; INPUT statement

A6EF:		.if	BASIC_REVISION = 1
		.byte	$F8
		.else
		.byte	$F7
		.endif

		; READ statement

A6F0:		.byte	$DB,$C2,$03,$14,$02,$16
		.byte	$03

		; PRINT, ? statement

A6F7:		.byte	$C9,$BB,$02

		.if	BASIC_REVISION = 1
		.byte	$ED
		.else
		.byte	$EC
		.endif

		; LPRINT statement

A6FB:		.byte	$00
		.word	A79B-1

		.byte	$B5,$03,$1C,$0E,$03,$01
		.word	A320-1
		.byte	$02,$01

		.word	A324-1

		.byte	$03,$B8
		.if	BASIC_REVISION = 1
		.byte	$C3,$02,$03,$12,$BB,$02
		.else
		.byte	$C2,$03,$12,$BC,$02
		.endif
		.byte	$03

		; XIO statement

A712:		.byte	$0E,$12

		; OPEN statement

A714:		.if	BASIC_REVISION = 1
		.byte	$AB
		.else
		.byte	$AC
		.endif
		.byte	$12,$F9,$12
		.if	BASIC_REVISION = 1
		.byte	$F3,$99
		.else
		.byte	$F3,$9A
		.endif
		.byte	$03

		; CLOSE statement

A71B:		.if	BASIC_REVISION = 1
		.byte	$A4,$96
		.else
		.byte	$A5,$97
		.endif
		.byte	$03

		; ENTER, LOAD, SAVE statement

A71E:		.if	BASIC_REVISION = 1
		.byte	$ED,$93,$03
		.else
		.byte	$ED,$94,$03
		.endif

		; RUN statement

A721:		.byte	$EA
		.if	BASIC_REVISION = 1
		.byte	$90,$02,$8E,$03,$99,$12,$02,$96
		.else
		.byte	$91,$02,$8F,$03,$9A,$12,$02,$97
		.endif
		.byte	$15,$02,$03

		; LIST statement

A72D:		.byte	$DE
		.if	BASIC_REVISION = 1
		.byte	$84
		.else
		.byte	$85
		.endif
		.byte	$02,$DB,$12,$C4,$02,$C2,$03,$00
		.word	A7BB-1
		.byte	$F4,$03

		; STATUS statement

A73B:		.if	BASIC_REVISION = 1
		.byte	$C3,$F1,$03,$81,$12,$00
		.else
		.byte	$C3,$F1,$03,$82,$12,$00
		.endif
		.word	A646-1
		.byte	$03

		; NOTE, POINT statement

A744:		.byte	$BA,$12,$00
		.word	A646-1
		.byte	$E4,$03

		.byte	$00
		.word	A67D-1
		.byte	$03

		.byte	$0E,$12
		.byte	$0E,$03

		; SOUND statement

A753:		.byte	$0E,$12

		; SETCOLOR statement

A755:		.byte	$0E,$12

		; POKE, PLOT, POSITION, DRAWTO statement

A757:		.byte	$B8,$D5,$03

		; COM, DIM statement
	
A75A:		.if	BASIC_REVISION = 1
		.byte	$EC,$D2
		.else
		.byte	$ED,$D2
		.endif
		.byte	$03

		; ON statement
	
A75D:		.byte	$0E,$C4,$C7,$CD,$03,$17,$02,$18
		.byte	$03,$0E,$C2,$03,$12,$BC,$02,$03
		.byte	$14,$02,$16,$03,$01
		.word	A320-1

		.if	BASIC_REVISION > 1
		.byte	$0D
		.endif

		.byte	$2B,$0F,$39,$0E,$00
		.word	A654-1
		.byte	$2C,$02,$01

		.word	A324-1

		.byte	$2B,$0F,$3B,$0E,$2C,$03
		.if	BASIC_REVISION = 1
		.byte	$AB
		.else
		.byte	$AA
		.endif
		.byte	$C3,$02,$03,$12,$BB,$02,$03

		; IF statement

A78F:		.byte	$0E,$1B,$C3
		.if	BASIC_REVISION = 1
		.byte	$9C
		.else
		.byte	$9B
		.endif
		.byte	$03,$01

		.word	A3F5-1
		.byte	$02,$01
		.word	A2CF-1

A79B:		.byte	$C9,$02,$D4,$C3,$02,$03,$C3,$02,$03,$C3,$C8,$03,$0E,$02,$00
		.word	A67D-1
		.byte	$03,$C4,$B3,$02,$03,$C6,$C2,$03,$BD,$02,$03,$12,$02,$15,$03

A7BB:		.byte	$0E,$C3,$02,$03,$12,$0E,$02,$03

		; REM statement

A7C3:		.byte	$01
		.word	A2DB-1

		; DATA statement

A7C6:		.byte	$01
		.word	A2DB-1
A7CE:		.byte	$40,$02,$41,$02,$43,$02,$42,$03
A7D1:		.byte	$3D,$02,$3E,$03
A7D5:		.byte	$0E,$C2,$03,$12,$0F,$3C,$BA,$02,$03

A7DE:		.byte	$02+$80
		.byte	$00+$80

		BasicString ","
		BasicString "$"
		BasicString ":"
		BasicString ";"
		.byte	$1B+$80			; Escape
		BasicString "GOTO"
		BasicString "GOSUB"
		BasicString "TO"
		BasicString "STEP"

		BasicString "THEN"
		BasicString "#"
		BasicString "<="
		BasicString "<>"
		BasicString ">="
		BasicString "<"
		BasicString ">"
		BasicString "="
		BasicString "^"
		BasicString "*"
		BasicString "+"
		BasicString "-"
		BasicString "/"

		BasicString "NOT"
		BasicString "OR"
		BasicString "AND"
		BasicString "("
		BasicString ")"
		BasicString "="
		BasicString "="
		BasicString "<="
		BasicString "<>"
		BasicString ">="
		BasicString "<"
		BasicString ">"
		BasicString "="
		BasicString "+"
		BasicString "-"

		.byte	$A8,$80,$80,$A8,$A8,$AC

		BasicString "STR$"
		BasicString "CHR$"
		BasicString "USR"
		BasicString "ASC"
		BasicString "VAL"
		BasicString "LEN"
		BasicString "ADR"
		BasicString "ATN"
		BasicString "COS"
		BasicString "PEEK"
		BasicString "SIN"
		BasicString "RND"
		BasicString "FRE"
		BasicString "EXP"
		BasicString "LOG"
		BasicString "CLOG"
		BasicString "SQR"
		BasicString "SGN"
		BasicString "ABS"
		BasicString "INT"
		BasicString "PADDLE"
		BasicString "STICK"
		BasicString "PTRIG"
		BasicString "STRIG"

		.byte	0

A87A:		lda	#0
A87C:		sty	$A4
		sta	$A5

		.if	BASIC_REVISION > 1
		tya
		.endif

		sec

		.if	BASIC_REVISION = 1
		lda	MEMTOP
		adc	$A4
		tay
		lda	MEMTOP+1
		.else
		adc	MEMTOP
		tay
		lda	MEMTOP+1
		.endif
	
		adc	$A5
		cmp	HIMEM+1
		bcc	@2
		bne	@1
		cpy	HIMEM
		bcc	@2
		beq	@2
@1:		jmp	B930

@2:		sec
		lda	MEMTOP
		sbc	0,X
		sta	$A2
		lda	MEMTOP+1
		sbc	1,X
		sta	$A3
		clc
		adc	1,X
		sta	$9A
		lda	0,X
		sta	$99
		sta	$97
		adc	$A4
		sta	$9B
		lda	1,X
		sta	$98
		adc	$A5
		adc	$A3
		sta	$9C
@3:		lda	0,X
		adc	$A4
		sta	0,X
		lda	1,X
		adc	$A5
		sta	1,X
		inx
		inx
		cpx	#$92
		bcc	@3
		sta	APPMHI+1
		lda	MEMTOP
		sta	APPMHI
		ldx	$A3
		inx
		ldy	$A2
		bne	A8EC

		.if	BASIC_REVISION = 1
		beq	A8F3
		.elseif	BASIC_REVISION = 2
		dex
		bne	A8EC
		rts
		.else
		nop
		beq	A8F3
		nop
		.endif

A8E3:		dey
		dec	$9A
		dec	$9C
A8E8:		lda	($99),Y
		sta	($9B),Y
A8EC:		dey
		bne	A8E8
		lda	($99),Y
		sta	($9B),Y
A8F3:		dex
		bne	A8E3
		rts

A8F7:		.if	BASIC_REVISION > 1
		tay
		.endif

A8F8:		lda	#0
A8FA:		sty	$A4
		sta	$A5
		sec
		lda	MEMTOP
		sbc	0,X
		eor	#$FF
		tay
		iny
		sty	$A2
		lda	MEMTOP+1
		sbc	1,X
		sta	$A3
		lda	0,X
		sbc	$A2
		sta	$99
		lda	1,X
		sbc	#0
		sta	$9A
		stx	$9B
A91D:		sec
		lda	0,X
		sbc	$A4
		sta	0,X
		lda	1,X
		sbc	$A5
		sta	1,X
		inx
		inx
		cpx	#$92
		bcc	A91D
		sta	APPMHI+1
		lda	MEMTOP
		sta	APPMHI
		ldx	$9B
		lda	0,X
		sbc	$A2
		sta	$9B
		lda	1,X
		sbc	#0
		sta	$9C
A944:		ldx	$A3
		inx
		ldy	$A2
		.if	BASIC_REVISION = 1
		bne	@2
		beq	@3
@1:		inc	$9A
		inc	$9C
@2:		lda	($99),Y
		sta	($9B),Y
		iny
		bne	@2
@3:		dex
		bne	@1
		rts
A95E:		jsr	B819
A961:		jsr	A9F2
		.byte	$D0,$35
A94F:
A953:
		.else
		bne	A953
		dex
		.if	BASIC_REVISION > 1
		bne	A953
		.endif
		rts

A94F:		inc	$9A
		inc	$9C
A953:		lda	($99),Y
		sta	($9B),Y
		iny
		bne	A953
		dex
		bne	A94F
		rts

A95E:		jsr	B819
A961:		jsr	A9F2
		beq	A99C
		.endif
		ldy	$A7
		cpy	$9F
		bcs	A989
		lda	(STMCUR),Y
		sta	$A7
		tya
		iny
		lda	(STMCUR),Y
		iny
		sty	$A8
		jsr	JJTAB1

		.if	BASIC_REVISION > 1
		nop
		.endif

		jmp	A961

JJTAB1:		asl
		tax
		lda	JTAB1,X
		pha
		lda	JTAB1+1,X
		pha
		rts

A989:		ldy	#1
		lda	(STMCUR),Y
		bmi	A99F
		lda	$9F
		jsr	A9D0
		jsr	A9E1
		bpl	A95E
		jmp	B78C
A99C:		jmp	B792
A99F:		jmp	A05D

A9A2:		lda	STMCUR
		sta	$BE
		lda	STMCUR+1
		sta	$BF
		lda	STMTAB+1
		ldy	STMTAB
		sta	STMCUR+1
		sty	STMCUR
A9B2:		ldy	#1
		lda	(STMCUR),Y
		cmp	$A1
		bcc	A9C7
		bne	A9C6
		dey
		lda	(STMCUR),Y
		cmp	$A0
		bcc	A9C7
		bne	A9C6
		clc
A9C6:		rts

A9C7:		jsr	A9DC
		jsr	A9D0
		jmp	A9B2

A9D0:		clc			; STMCUR += accu
		adc	STMCUR
		sta	STMCUR

		.if	BASIC_REVISION = 1
		tay
		.endif

		lda	STMCUR+1
		adc	#0
		sta	STMCUR+1
		rts

A9DC:		ldy	#2
		lda	(STMCUR),Y
		rts
	
A9E1:		ldy	#1
		lda	(STMCUR),Y

		.if	BASIC_REVISION = 1
		rts
		.endif

A9E5:		rts			; execute REM, DATA
	
A9E6:		jsr	BD45		; execute BYE
		jmp	SELFSV

A9EC:		jsr	BD45		; execute DOS
		jmp	(DOSVEC)

A9F2:		.if	BASIC_REVISION = 1
		ldy	#0
		lda	BRKKEY
		bne	@1
		ldy	#$FF
		sty	BRKKEY
		.else
		ldy	BRKKEY
		bne	@2
		dec	BRKKEY
		.endif
@1:		tya
@2:		rts

JTAB1:		.dbyt	A9E5-1,A9E5-1,B33E-1,BA1F-1,B4B5-1,BAC5-1,AADA-1,B778-1
		.dbyt	B67D-1,B700-1,B6D5-1,B6D5-1,B6D2-1,B7D8-1,A9E6-1,B7B5-1
		.dbyt	B206-1,BC22-1,B766-1,B28D-1,B206-1,B78C-1,A00C-1,BBF2-1
		.dbyt	BAFB-1,BB6D-1,BC2F-1,BC3D-1,BC54-1,BBEC-1,B7E4-1,B278-1
		.dbyt	B3DA-1,B291-1,B2AE-1,B296-1,BDA8-1,B74C-1,B792-1,B83E-1
		.dbyt	B3DA-1,BC85-1,BC78-1,BA46-1,BA6C-1,BA0C-1,A9EC-1,BA27-1
		.dbyt	B9AD-1,BC9E-1,B9D3-1,B496-1,BBD1-1,BB64-1,AADA-1,B912-1

JTAB2:		.dbyt	ACA3-1,ACAC-1,ACC2-1,ACB2-1,ACB9-1,ACC9-1,B15E-1,AC83-1
		.dbyt	BDFA-1,AC7A-1,AC8C-1,ACE4-1,ACD9-1,ACCF-1,RTS1-1,AD66-1
		.dbyt	AD4A-1,AE8E-1,ACA3-1,ACAC-1,ACC2-1,ACB2-1,ACB9-1,ACC9-1
		.dbyt	RTS2-1,AC95-1,AE11-1,AD71-1,AD6D-1,AD66-1,AD6D-1,AD64-1
		.dbyt	B034-1,B052-1,B0A5-1,AFFD-1,AFEB-1,AFB5-1,B007-1,B118-1
		.dbyt	B10F-1,AFCC-1,B106-1,B076-1,AFD6-1,B14A-1,B121-1,B13D-1
		.dbyt	B153-1,AD04-1,B099-1,B0C8-1,B00D-1,B011-1,B015-1,B019-1

AADA:		jsr	AB26		; execute LET
AADD:		jsr	AB36
		bcs	AAE7
		jsr	ABB2

		.if	BASIC_REVISION = 1
		jmp	AADD
		.else
		bmi	AADD
		.endif

AAE7:		sta	$AB
		tax
		lda	AC35-$10,X
		lsr
		lsr
		lsr
		lsr
		sta	$AC
AAF3:		ldy	$A9
		lda	(LOMEM),Y
		tax
		lda	AC35-$10,X
		and	#%00001111
		cmp	$AC
		bcc	AB0E
		tax
		beq	RTS1
AB04:		lda	(LOMEM),Y
		inc	$A9
		jsr	JJTAB2
AB12:		jmp	AAF3
AB0E:		lda	$AB
		dey
		sta	(LOMEM),Y
		sty	$A9
		jmp	AADD

		.if	BASIC_REVISION = 1
RTS1:		rts
		.endif

JJTAB2:		sec
		sbc	#29
		asl
		tax
		lda	JTAB2,X
		pha
		lda	JTAB2+1,X
		pha
		rts

AB26:		ldy	#$FF
		lda	#$11
		sta	(LOMEM),Y
		sty	$A9
		iny
		sty	$B0
		sty	$AA
		sty	$B1

		.if	BASIC_REVISION > 1
RTS1:
RTS2:
		.endif

		rts

AB36:		ldy	$A8
		inc	$A8
		lda	(STMCUR),Y
		bmi	AB81
		cmp	#$F
		bcc	AB45
		beq	AB57
		rts

AB45:		ldx	#0
AB47:		iny
		lda	(STMCUR),Y
		sta	FR0,X
		inx
		cpx	#6
		bcc	AB47

		iny
		lda	#0
		tax
		beq	AB79
AB57:		iny
		lda	(STMCUR),Y
		ldx	#$8A
AB5C:		sta	$D6
		sta	$D8
		iny
		tya
		clc
		adc	$0,X
		sta	FR0
		lda	#0
		sta	$D7
		sta	$D9
		adc	$1,X
		sta	FR0+1
		tya
		adc	$D6
		tay
		ldx	#0
		lda	#$83
AB79:		sta	$D2
		stx	$D3
		sty	$A8
		clc
		rts

AB81:		jsr	AC1E
AB84:		lda	($9D),Y
		sta	$D2,Y
		iny
		cpy	#8
		bcc	AB84
		clc
		rts

AB90:		jsr	ABE9
AB93:		lda	#2
		bit	$D2
		bne	@rts
		ora	$D2
		sta	$D2
		ror
		bcc	@1
		clc
		lda	FR0
		adc	STARP
		sta	FR0
		tay
		lda	FR0+1
		adc	$8D
		sta	FR0+1
@rts:		rts
@1:		jsr	B922

ABB2:		inc	$AA
		lda	$AA
		asl
		asl
		asl
		cmp	$A9
		bcs	ABCA
		tay
		dey
		ldx	#$7
@1:		lda	$D2,X
		sta	(LOMEM),Y
		dey
		dex
		bpl	@1
ABC9:		rts
ABCA:		jmp	B920

ABCD:		jsr	ABD7
		lda	FR0+1
	
		.if	BASIC_REVISION = 1
		bmi	@1
		rts
@1:		jmp	B926
		.else
		bpl	ABC9
		jmp	B926
		.endif

ABD7:		jsr	AADA
ABDA:		jsr	ABE9
		jmp	AD41
ABE0:		jsr	ABCD
		bne	@1
		rts
@1:		jsr	B92E
ABE9:		lda	$AA
		dec	$AA
		asl
		asl
		asl
		tay
		dey
		ldx	#7
@loop_x:	lda	(LOMEM),Y
		sta	$D2,X
		dey
		dex
		bpl	@loop_x
		rts

ABFD:		jsr	ABE9
		jsr	FMOVE
		jmp	ABE9
AC06:		jsr	AADA

		.if	BASIC_REVISION = 1
		jsr	ABE9
		rts
		.else
		jmp	ABE9
		.endif

AC0C:		lda	$D3
		jsr	AC1E
		ldx	#0
@1:		lda	$D2,X
		sta	($9D),Y
		iny
		inx
		cpx	#8
		bcc	@1
		rts

AC1E:		ldy	#0
		sty	$9E
		asl
		asl
		rol	$9E
		asl
		rol	$9E
		clc
		adc	VVTP
		sta	$9D
		lda	VVTP+1
		adc	$9E
		sta	$9E
		rts
	
AC35:		.byte	$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$88,$88,$88
		.byte	$88,$88,$88,$CC,$AA,$99,$99,$AA

		.if	BASIC_REVISION = 1
		.byte	$77
		.else
		.byte	$DD
		.endif

		.byte	$55,$66,$F2,$4E,$F1,$F1,$EE
		.byte	$EE,$EE,$EE,$EE,$EE,$DD,$DD,$F2
		.byte	$F2,$F2,$F2,$F2,$43,$F2,$F2,$F2
		.byte	$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2
		.byte	$F2,$F2,$F2,$F2,$F2,$F2,$F2,$F2
		.byte	$F2,$F2,$F2,$F2,$F2

		.if	BASIC_REVISION = 1
BDFA:		jsr	ABFD
		jsr	AD26
		jmp	ABB2
		.endif

AC7A:		jsr	ABFD
		jsr	AD2C
		jmp	ABB2
AC83:		jsr	ABFD
		jsr	AD32
		jmp	ABB2
AC8C:		jsr	ABFD
		jsr	AD38
		jmp	ABB2
AC95:		jsr	ABE9

		lda	FR0

		.if	BASIC_REVISION > 1
		beq	@1
		.endif

		eor	#$80
		sta	FR0
@1:		jmp	ABB2

		.if	BASIC_REVISION = 1
RTS2:		rts
		.endif

ACA3:		jsr	AD11
		bmi	ACF0
		beq	ACF0
		bpl	ACEB
ACAC:		jsr	AD11

		.if	BASIC_REVISION = 1
		beq	ACEB
		bne	ACF0
		.else
		jmp	ACE0
		.endif

ACB2:		jsr	AD11
		bmi	ACF0
		bpl	ACEB

ACB9:		jsr	AD11
		bmi	ACEB
		beq	ACEB
		bpl	ACF0

ACC2:		jsr	AD11
		bmi	ACEB
		bpl	ACF0

ACC9:		jsr	AD11
		.if	BASIC_REVISION = 1
		beq	ACF0
		bne	ACEB
		.else
		jmp	ACE9
		.endif

ACCF:		jsr	ABFD
		lda	FR0
		and	FR1

		.if	BASIC_REVISION = 1
		beq	ACEB
		bne	ACF0
		.else
		jmp	ACE0
		.endif
		
ACD9:		jsr	ABFD
		lda	FR0
		ora	FR1
ACE0:		beq	ACEB
		bne	ACF0

ACE4:		jsr	ABE9
		lda	FR0
ACE9:		beq	ACF0
ACEB:		lda	#0
		tay
		beq	ACF4
ACF0:		lda	#$40
ACF2:		ldy	#1
ACF4:		sta	FR0
		sty	FR0+1
		ldx	#$D6
		ldy	#4
		jsr	__DA48
		sta	$D2
AD01:		jmp	ABB2

AD04:		jsr	ABE9		; evaluate SGN()
		lda	FR0
		beq	AD01
		bpl	ACF0
		lda	#$C0
		bmi	ACF2
AD11:		ldy	$A9
		dey
		lda	(LOMEM),Y
		cmp	#$2F
		bcc	@1
		jmp	AF6C

@1:		jsr	ABFD
AD20:		jsr	AD2C
		lda	FR0
		rts

AD26:		jsr	FADD
		bcs	AD3E
		rts

AD2C:		jsr	FSUB
		bcs	AD3E
		rts

AD32:		jsr	FMUL
		bcs	AD3E
		rts

AD38:		jsr	FDIV
		bcs	AD3E
		rts

AD3E:		jsr	B91E
AD41:		jsr	FPI
		bcs	AD47
		rts

AD47:		jsr	B92E
AD4A:		lda	$A9
		cmp	#$FF
		bne	AD5F
		jsr	ABFD

		ldx	#5	; copy FR1 -> FR0
AD55:		lda	FR1,X
		sta	FR0,X
		dex
		bpl	AD55
		jmp	AC0C

AD5F:		lda	#$80
		sta	$B1
		rts

AD64:		inc	$B0
AD66:		ldy	$A9
		pla
		pla
		jmp	AB04

AD6D:		lda	#$40
		sta	$B1
AD71:		bit	$B1
		bpl	AD7B
		lda	$AA
		sta	$AF
		dec	$AA
AD7B:		lda	#0
		tay
		cmp	$B0
		beq	AD8D
		dec	$B0
		jsr	ABDA
		lda	FR0+1
		bmi	ADAE
		ldy	FR0
AD8D:		sta	$98
		sty	$97
		jsr	ABDA
		lda	FR0
		sta	$F5
		lda	FR0+1
		bmi	ADAE
		sta	$F6
		jsr	ABE9
		bit	$B1
		bvc	ADAA
		lda	#0
		sta	$B1
		rts
ADAA:		ror	$D2
		bcs	ADB1
ADAE:		jsr	B922
ADB1:		lda	$F6
		cmp	$D7
		bcc	ADBF
		bne	ADAE
		lda	$F5
		cmp	$D6
		bcs	ADAE
ADBF:		lda	$98
		cmp	$D9
		bcc	ADCD
		bne	ADAE
		lda	$97
		cmp	$D8
		bcs	ADAE
ADCD:		jsr	AF48
		lda	$97
		ldy	$98
		jsr	AF3D
		jsr	AF31
		lda	FR0
		ldy	FR0+1
		jsr	AF3D
		lda	STARP
		ldy	STARP+1
		jsr	AF3D
		bit	$B1
		bpl	AE01
		lda	$AF
		sta	$AA
		jsr	ABE9
		ldy	#5
ADF5:		lda	FR0,Y
		sta	($F5),Y
		dey
		bpl	ADF5
		iny
		sty	$B1
		rts
AE01:		ldy	#5
AE03:		lda	($F5),Y
		sta	$0D4,Y
		dey
		bpl	AE03
		iny
		sty	$D2
		jmp	ABB2
AE11:		lda	$B0
		beq	AE1C
		jsr	AE81
		sty	$98
		sta	$97
AE1C:		jsr	AE81
		sec
		sbc	#1
		sta	$F5
		tya
		sbc	#0
		sta	$F6
		jsr	ABE9
		lda	$B1
		bpl	AE3B
		ora	$B0
		sta	$B1
		ldy	$D9
		lda	$D8
		jmp	AE3F
AE3B:		lda	$D6
		ldy	$D7
AE3F:		ldx	$B0
		beq	AE53
		dec	$B0
		cpy	$98
		bcc	AE7E
		bne	AE4F
		cmp	$97
		bcc	AE7E
AE4F:		ldy	$98
		lda	$97
AE53:		sec
		sbc	$F5
		sta	$D6
		tax
		tya
		sbc	$F6
		sta	$D7
		bcc	AE7E
		tay
		bne	AE66
		txa
		beq	AE7E
AE66:		jsr	AB93
		clc
		lda	FR0
		adc	$F5
		sta	FR0
		lda	FR0+1
		adc	$F6
		sta	FR0+1
		bit	$B1
		bpl	AE7B
		rts
AE7B:		jmp	ABB2
AE7E:		jsr	B92A
AE81:		jsr	ABDA
		lda	FR0
		ldy	FR0+1
		bne	AE8D
		tax
		beq	AE7E
AE8D:		rts

AE8E:		jsr	AB90
AE91:		lda	FR0
		sta	$99
		lda	FR0+1
		sta	$9A
		lda	FR0+2
		sta	$A2
		ldy	FR0+3
		sty	$A3
		ldy	$A9
		cpy	#$FF
		beq	AEB6
		lda	#$80
		sta	$B1
		jsr	AB04
		lda	$D7
		ldy	$D6
		rol	$B1
		bcs	AEBD
AEB6:		jsr	AB90
		lda	$D9
		ldy	$D8
AEBD:		cmp	$A3
		bcc	AEC7
		bne	AECB
		cpy	$A2
		bcs	AECB
AEC7:		sta	$A3
		sty	$A2
AECB:		clc
		lda	FR0
		adc	$A2
		tay
		lda	FR0+1
		adc	$A3
		tax
		sec
		tya
		sbc	STARP
		sta	$F9
		txa
		sbc	$8D
		sta	$FA
		sec
		lda	#0
		sbc	$A2
		sta	$A2
		sec
		lda	$99
		sbc	$A2
		sta	$99
		lda	$9A
		sbc	#0
		sta	$9A
		sec
		lda	FR0
		sbc	$A2
		sta	$9B
		lda	FR0+1
		sbc	#0
		sta	$9C
		jsr	A944
		lda	$D3
		jsr	AB81
		sec
		lda	$F9
		sbc	FR0
		tay
		lda	$FA
		sbc	FR0+1
		tax
		lda	#2
		and	$B1
		beq	AF2A
		lda	#0
		sta	$B1
		cpx	$D7
		bcc	AF29
		bne	AF2A
		cpy	$D6
		bcs	AF2A
AF29:		rts
AF2A:		sty	$D6
		stx	$D7
		jmp	AC0C
AF31:		asl	$F5
		rol	$F6
		ldy	$F6
		lda	$F5
		asl	$F5
		rol	$F6
AF3D:		clc
		adc	$F5
		sta	$F5
		tya
		adc	$F6
		sta	$F6
		rts
AF48:		lda	#0
		sta	$F7
		sta	$F8
		ldy	#$10
AF50:		lda	$F5
		lsr
		bcc	AF61
		clc
		ldx	#$FE
AF58:		lda	$F9,X
		adc	$DA,X
		sta	$F9,X
		inx
		bne	AF58
AF61:		ldx	#3
AF63:		ror	$F5,X
		dex
		bpl	AF63
		dey
		bne	AF50
		rts
AF6C:		jsr	AB90
		jsr	FMOVE
		jsr	AB90
AF75:		ldx	#$D6
		jsr	AFA7
		php
		ldx	#$E2
		jsr	AFA7
		beq	AF95
		plp
		beq	AF92
		ldy	#0
		lda	(FR0),Y
		cmp	(FR1),Y
		beq	AF99
		bcc	AF92
AF8F:		lda	#1
		rts
AF92:		lda	#$80
		rts
AF95:		plp
		bne	AF8F
		rts
AF99:		inc	FR0
		bne	AF9F
		inc	FR0+1
AF9F:		inc	FR1
		bne	AF75
		inc	FR1+1
		bne	AF75

AFA7:		lda	0,X
		bne	AFB1
		lda	1,X
		beq	AFB4
		dec	1,X
AFB1:		dec	0,X
		tay
AFB4:		rts

AFB5:		jsr	AB90		; evaluate LEN()
		lda	FR0+2
		ldy	FR0+3
AFBC:		sta	FR0
		sty	FR0+1
AFC0:		jsr	IFP
AFC3:		lda	#0
		sta	$D2
		sta	$D3
		jmp	ABB2

AFCC:		jsr	ABDA		; evaluate PEEK()
		ldy	#0
		lda	(FR0),Y
		jmp	AFBC

AFD6:		jsr	ABE9		; evaluate FRE()
		sec
		lda	HIMEM
		sbc	MEMTOP
		sta	FR0
		lda	HIMEM+1
		sbc	MEMTOP+1
		sta	FR0+1
		jmp	AFC0

AFEB:		jsr	BD7D		; evaluate VAL()
		lda	#0
		sta	CIX
		jsr	AFP
		jsr	BD9D
		bcc	AFC3
		jsr	B910

AFFD:		jsr	AB90		; evaluate ASC()
		ldy	#0
		lda	(FR0),Y
		jmp	AFBC

B007:		jsr	AB90		; evaluate ADR()
		jmp	AFC0

B00D:		lda	#0		; evaluate PADDLE()
		beq	B01B

B011:		lda	#8		; evaluate STICK()
		bne	B01B

B015:		lda	#12		; evaluate PTRIG()
		bne	B01B

B019:		lda	#20		; evaluate STRIG()

B01B:		pha
		jsr	ABDA
		lda	FR0+1
		bne	B031
		lda	FR0
		pla
		clc
		adc	FR0
		tax
		lda	$270,X
		ldy	#0
		beq	AFBC
B031:		jsr	B92E

B034:		jsr	ABE9		; evaluate STR$()
		jsr	FASC
		lda	INBUFF
		sta	FR0
		lda	INBUFF+1
		sta	FR0+1
		ldy	#$FF
@1:		iny
		lda	(INBUFF),Y
		bpl	@1
		and	#$7F
		sta	(INBUFF),Y
		iny
		sty	$D6
		bne	B069

B052:		jsr	ABE9		; evaluate CHR$()
		jsr	AD41
		lda	FR0
		sta	$5C0
		lda	#5
		sta	FR0+1
		lda	#$C0
		sta	FR0
		lda	#1
		sta	FR0+2
B069:		lda	#0
		sta	FR0+3
		sta	$D3
		lda	#$83
		sta	$D2
		jmp	ABB2

B076:		ldx	#<@FP65536	; evaluate RND()
		ldy	#>@FP65536
		jsr	FLD1R
		jsr	ABE9
		ldy	RANDOM
		sty	FR0
		ldy	RANDOM
		sty	FR0+1
		jsr	IFP
		jsr	AD38
		jmp	ABB2

@FP65536:	.byte	$42,$06,$55,$36,$00,$00

B099:		jsr	ABE9		; evaluate ABS()
		lda	FR0
		and	#$7F
		sta	FR0
		jmp	ABB2

B0A5:		jsr	@1		; evaluate USR()
		jsr	IFP
		jmp	ABB2

@1:		lda	$B0
		sta	$C6
@2:		jsr	ABDA
		dec	$C6
		bmi	@3
		lda	FR0
		pha
		lda	FR0+1
		pha
		jmp	@2
@3:		lda	$B0
		pha
		jmp	(FR0)

B0C8:		jsr	ABE9		; evaluate INT()
		jsr	@1
		jmp	ABB2
@1:		lda	FR0
		and	#$7F
		sec
		sbc	#$3F
		bpl	@2
		lda	#0
@2:		tax
		lda	#0
		tay
@3:		cpx	#5
		bcs	@4
		ora	FR0+1,X
		sty	FR0+1,X
		inx
		bne	@3
@4:		ldx	FR0
		bpl	@5
		tax
		beq	@5
		ldx	#$E0
		jsr	ZF1
		lda	#$C0
		sta	$E0
		lda	#1
		sta	$E1
		jsr	AD26
		rts
@5:		jmp	__DC00

B106:		jsr	ABE9		; evaluate SIN()
		jsr	BE05
		.if	BASIC_REVISION = 1
		bcs	ex_cs
		bcc	ex_cc
		.else
		jmp	B159
		.endif

B10F:		jsr	ABE9		; evaluate COS()
		jsr	BE0F
		.if	BASIC_REVISION = 1
		bcs	ex_cs
		bcc	ex_cc
		.else
		jmp	B159
		.endif

B118:		jsr	ABE9		; evaluate ATN()
		jsr	BED5
		.if	BASIC_REVISION = 1
		bcs	ex_cs
		bcc	ex_cc
		.else
		jmp	B159
		.endif

		.if	BASIC_REVISION = 1

B121:		jsr	ABE9		; evaluate LOG()
		jsr	LOG
		bcs	ex_cs
		bcc	ex_cc

B13D:		jsr	ABE9		; evaluate CLOG()
		jsr	LOG10
		bcs	ex_cs
		bcc	ex_cc

B14A:		jsr	ABE9		; evaluate EXP()
		jsr	EXP
		bcs	ex_cs
		bcc	ex_cc

B153:		jsr	ABE9		; evaluate SQR()
		jsr	BF43
		bcs	ex_cs
ex_cc:		jmp	ABB2
ex_cs:		jsr	B92E

B15E:		jsr	ABFD
		lda	FR0
		.byte	$d0,$0b
		lda	$E0
		beq	@4
		.byte	$10,$ed
		.byte	$30,$ee
@4:		jmp	ACF0
		.byte	$10,$30
		and	#$7F
		sta	FR0
		lda	$E0
		and	#$7F
		sec
		sbc	#$40
		.byte	$30,$dc
		ldx	#6
		cmp	#5
		bcc	@2
		ldy	#1
		bne	@1
@2:		sta	$F5
		sec
		lda	#5
		sbc	$F5
		tay
@1:		dex
		dey
		beq	@6
		lda	$E0,X
		.byte	$D0,$C2
		beq	@1
@6:		ldy	#$80
		lda	$E0,X
		lsr
		bcs	@5
@3:		ldy	#0
@5:		tya
		pha
		ldx	#5
@7:		lda	$E0,X
		pha
		dex
		bpl	@7
		jsr	LOG10
		.byte	$B0,$A8
		ldx	#0
		ldy	#5
@8:		pla
		sta	$E0,X
		inx
		dey
		bpl	@8
		jsr	AD32
		jsr	EXP10
		.byte	$b0,$09
		pla
		.byte	$10,$8f
		ora	FR0
		sta	FR0
		.byte	$d0,$89

		.else

B121:		jsr	ABE9		; evaluate CLOG()
		lda	FR0
		beq	B15B
		jsr	LOG
B12B:		bcs	B15B
		lda	FR0
		eor	#$3B
		bne	B16C
		lda	FR0+1
		and	#$F8
		bne	B16C
		sta	FR0
		beq	B16C

B13D:		jsr	ABE9		; evaluate CLOG()
		lda	FR0
		beq	B15B
		jsr	LOG10
		jmp	B12B

B14A:		jsr	ABE9		; evaluate EXP()
		jsr	EXP
		jmp	B159

B153:		jsr	ABE9		; evaluate SQR()
		jsr	BF43
B159:		bcc	B16C
B15B:		jsr	B92E

B15E:		jsr	ABFD
		lda	FR1
		beq	B16F
		rol
		ldy	FR0
		bne	B172
		bcs	B15B
B16C:		jmp	ABB2
B16F:		jmp	ACF0
B172:		ldx	#$D4
		jsr	BA76
		ror
		pha
		ldx	#$E0
		jsr	BA76
		tya
		bpl	B19F
		and	#$7F
		sta	FR0
		bcs	B18A
		pla
		bcc	B15B
B18A:		lda	FR1
		bpl	B18F
		clc
B18F:		php
		ldx	$F7
		cpx	#5
		bcs	B1A5
		lda	$E1,X
		ror
		bcc	B1A5
		lda	#$80
		bne	B1A7
B19F:		lda	$E0
		bpl	B1A4
		clc
B1A4:		php
B1A5:		lda	#0
B1A7:		pha
		ldx	#5

B1AA:		lda	FR1,X
		pha
		dex
		bpl	B1AA
		jsr	LOG10
		ldx	#0
		ldy	#5
B1B7:		pla
		sta	FR1,X
		inx
		dey
		bpl	B1B7
		jsr	AD32
		jsr	EXP10
		bcs	B203
		pla
		ora	FR0
		sta	FR0
		plp
		pla
		bpl	B16C
		bcc	B16C
		ldx	#$D4
		jsr	BA76
		bcs	B16C
		lda	FR0
		sec
		and	#$7F
		sbc	#$3F
		cmp	#6
		bcs	B200
		tax
		tay
		sed
		sec
B1E7:		lda	FR0,X
		adc	#0
		sta	FR0,X
		dex
		bne	B1E7
		cld
		bcc	B1F7
		inc	FR0
		inc	FR0+1
B1F7:		iny
		cpy	#6
		bcs	B200
		stx	FR0,Y
		bcc	B1F7
B200:		jmp	ABB2

		.endif
	
B203:		jsr	B91E

B206:		ldy	$A8		; execute COM, DIM
		cpy	$A7
		bcc	B20D
		rts
B20D:		jsr	AADA
		lda	$D2
		ror
		bcc	B218
B215:		jsr	B922
B218:		sec
		rol
		sta	$D2
		bmi	B24C
		ldy	$F5
		ldx	$F6
		iny
		bne	B228
		inx
		bmi	B215
B228:		sty	$D6
		stx	$D7
		sty	$F5
		stx	$F6
		ldy	$97
		ldx	$98
		iny
		bne	B23A
		inx
		bmi	B215
B23A:		sty	$D8
		stx	$D9
		jsr	AF48
		jsr	AF31
		ldy	$F5
		lda	$F6
		bmi	B215

		.if	BASIC_REVISION = 1
		jmp	B260
		.else
		bpl	B260
		.endif

B24C:		lda	#0
		sta	$D6
		sta	$D7
		ldy	$F5
		sty	$D8
		lda	$F6
		sta	$D9
		bne	B260
		cpy	#0
		beq	B215
B260:		ldx	#$8E
		jsr	A87C
		sec
		lda	$97
		sbc	STARP
		sta	FR0
		lda	$98
		sbc	$8D
		sta	FR0+1
		jsr	AC0C
		jmp	B206

B278:		jsr	ABD7		; execute POKE
		lda	FR0
		sta	POKADR
		lda	FR0+1
		sta	POKADR+1
		jsr	ABE0
		lda	FR0
		ldy	#0
		sta	(POKADR),Y
		rts

B28D:		lda	#6		; execute DEG

		.if	BASIC_REVISION = 1
		sta	$FB
		rts
		.else
		bne	B293
		.endif

B291:		lda	#0		; execute RAD
B293:		sta	$FB
		rts

B296:		lda	#0		; execute RESTORE
		sta	DATAD
		jsr	B904
		bcc	B2A2
		tay
		beq	B2A9
B2A2:		jsr	ABCD
		lda	FR0+1
		ldy	FR0
B2A9:		sta	DATALN+1
		sty	DATALN
		rts

B2AE:		lda	$A8		; execute READ
		pha
		jsr	B6F9
		lda	DATALN
		sta	$A0
		lda	DATALN+1
		sta	$A1
		jsr	A9A2
		lda	STMCUR
		sta	INBUFF
		lda	STMCUR+1
		sta	INBUFF+1
		jsr	BDA8
		pla
		sta	$A8
B2CD:		ldy	#0
		sty	CIX
		jsr	B32F
		sta	DATALN
		jsr	B32D
		sta	DATALN+1
		jsr	B32D
		sta	$F5
B2E0:		jsr	B32D
		sta	$F6
		jsr	B32D

		.if	BASIC_REVISION = 1
		cmp	#1
		.else
		eor	#1
		.endif

		beq	B312
		ldy	$F6
		cpy	$F5
		bcs	B2F7
		dey
		sty	CIX
		bcc	B2E0
B2F7:		sty	CIX
		dec	CIX
B2FB:		ldy	#1
		lda	(INBUFF),Y
		bmi	B33B
		sec
		lda	CIX
		adc	INBUFF
		sta	INBUFF
		lda	#0
		sta	DATAD
		adc	INBUFF+1
		sta	INBUFF+1
		bcc	B2CD

B312:		.if	BASIC_REVISION = 1
		lda	#0
		.endif

		sta	$F5
B314:		lda	$F5
		cmp	DATAD
		bcs	B325
B31A:		jsr	B32D
		bne	B31A
		bcs	B2FB
		inc	$F5
		bne	B314
B325:		lda	#$40
		sta	$A6
		inc	CIX

		.if	BASIC_REVISION = 1
		jmp	B35F
		.else
		bcs	B35F
		.endif

B32D:		inc	CIX
B32F:		ldy	CIX
		lda	(INBUFF),Y
		cmp	#$2C
		clc
		beq	B33A
		cmp	#$9B
B33A:		rts
B33B:		jsr	B928

B33E:		lda	#$3F		; execute INPUT
		sta	$C2
		jsr	AB36
		dec	$A8
		bcc	B34E
		jsr	BD07
		sta	$B4
B34E:		jsr	__DA51

		.if	BASIC_REVISION = 1
		jsr	BA89
		jsr	B378
		.else
		jsr	BDE4
		jsr	A9F2
		beq	B378
		.endif

		ldy	#0
		sty	$A6
		sty	CIX
B35F:		jsr	AB36
		inc	$A8
		lda	$D2
		bmi	B382
		jsr	AFP
		bcs	B37B
		jsr	B32F
		bne	B37B
		jsr	AC0C
		jmp	B3AD

B378:		.if	BASIC_REVISION = 1
		jsr	A9F2
		bne	@1
		rts
		.endif
@1:		jmp	B792

B37B:		lda	#0
		sta	$B4
		jsr	B924
B382:		jsr	AB26
		jsr	ABB2
		dec	CIX
		lda	CIX
		sta	$F5
		ldx	#$FF
B390:		inx
		jsr	B32D
		bne	B390
		bcs	B39C
		bit	$A6
		bvc	B390
B39C:		ldy	$F5
		lda	$A8
		pha
		txa
		ldx	#INBUFF
		jsr	AB5C
		pla
		sta	$A8
		jsr	AE91
B3AD:		bit	$A6
		bvc	B3C0
		inc	DATAD
		jsr	B904
		bcs	B3C5
		jsr	B32F
		bcc	B3D5
		jmp	B2FB
B3C0:		jsr	B904
		bcc	B3CD
B3C5:		jsr	__DA51
		lda	#0
		sta	$B4
		rts
B3CD:		jsr	B32F
		bcc	B3D5
		jmp	B34E
B3D5:		inc	CIX
		jmp	B35F

B3DA:		lda	PTABW		; execute PRINT, ?
		sta	$AF
		lda	#0
		sta	$94
B3E2:		ldy	$A8
		lda	(STMCUR),Y
		cmp	#$12
		beq	B449
		cmp	#$16
		.if	BASIC_REVISION = 1
		.byte	$f0,$7C
		.else
		beq	B467
		.endif
		cmp	#$14
		.if	BASIC_REVISION = 1
		.byte	$f0,$78
		.else
		beq	B467
		.endif
		cmp	#$15
		beq	B474
		cmp	#$1C
		beq	B46A
		jsr	AADA
		jsr	ABE9
		dec	$A8
		bit	$D2
		bmi	B428

		.if	BASIC_REVISION > 1
		lda	FR0+1
		cmp	#$10
		bcc	B412
		lda	$D9
		and	#$F0
		sta	$D9
		.endif

B412:		jsr	FASC
		lda	#0
		sta	CIX
B419:		ldy	CIX
		lda	(INBUFF),Y
		pha
		inc	CIX
		jsr	B48F
		pla
		bpl	B419
		bmi	B3E2
B428:		jsr	AB93
		lda	#0
		sta	CIX
B42F:		lda	$D6
		bne	B437
		dec	$D7
		bmi	B3E2
B437:		dec	$D6
		ldy	CIX
		lda	(FR0),Y
		inc	CIX
		bne	B443
		inc	FR0+1
B443:		jsr	B491
		jmp	B42F
B449:		ldy	$94
		iny
		cpy	$AF
		bcc	B459
		clc
		lda	PTABW
		adc	$AF
		sta	$AF
		bcc	B449
B459:		ldy	$94
		cpy	$AF
		bcs	B474
		lda	#$20
		jsr	B48F
		jmp	B459

		.if	BASIC_REVISION > 1
B467:		jmp	B485
		.endif

B46A:		jsr	BD07
		sta	$B5
		dec	$A8
		jmp	B3E2
B474:		inc	$A8

		.if	BASIC_REVISION = 1
B467:		jmp	B3E2
		ldy	$A8
		dey
		lda	(STMCUR),Y
		cmp	#$15
		beq	B48A
		cmp	#$12
		beq	B48A
		.else
		ldy	$A8
		lda	(STMCUR),Y
		cmp	#$16
		beq	B48A
		cmp	#$14
		beq	B48A
		jmp	B3E2
		.endif

B485:		lda	#$9B
		jsr	B491
B48A:		lda	#0
		sta	$B5
		rts
B48F:		and	#$7F
B491:		inc	$94
		jmp	BA99

B496:		lda	#<@printerdev	; execute LPRINT
		sta	INBUFF
		lda	#>@printerdev
		sta	INBUFF+1
		ldx	#7
		stx	$B5
		lda	#0
		ldy	#8
		jsr	BBD8
		jsr	BCBB
		jsr	B3DA
		jmp	BCF7

@printerdev:	.byte	"P:",$9B

B4B5:		ldy	#0		; execute LIST
		sty	$A0
		sty	$A1
		dey
		sty	$AD
		lda	#$7F
		sta	$AE
		sta	$2FE
		lda	#$9B
		jsr	BA99
		jsr	B6F9
@1:		ldy	$A8
		iny
		cpy	$A7
		bcs	@4
		lda	$A8
		pha
		jsr	AC06
		pla
		sta	$A8
		lda	$D2
		bpl	@2
		jsr	BACF
		jmp	@1

@2:		jsr	ABCD
		sta	$A1
		lda	FR0
		sta	$A0
		ldy	$A8
		cpy	$A7
		beq	@3
		jsr	ABCD
@3:		lda	FR0
		sta	$AD
		lda	FR0+1
		sta	$AE
@4:		jsr	A9A2
@5:		jsr	A9E1
		bmi	@7
		ldy	#1
		lda	(STMCUR),Y
		cmp	$AE
		bcc	@6
		bne	@7
		dey
		lda	(STMCUR),Y
		cmp	$AD
		bcc	@6
		bne	@7
@6:		jsr	B58E
		jsr	A9F2

		.if	BASIC_REVISION = 1
		bne	@7
		.else
		beq	@7
		.endif

		jsr	A9DC
		jsr	A9D0
		jmp	@5
@7:		lda	$B5
		beq	@8

		jsr	BCF7
		lda	#0
		sta	$B5
@8:		sta	$2FE
		jmp	BDA8

B53E:		stx	$AA
		jsr	@5
@1:		ldy	$AA
		dec	$AF
		bmi	@4
@2:		lda	(POKADR),Y
		bmi	@3
		iny
		bne	@2
@3:		iny
		jsr	@4
		jmp	@1

@4:		clc
		tya
		adc	POKADR
		sta	POKADR
		tay
		lda	POKADR+1
		adc	#0
@5:		sta	POKADR+1
		sty	POKADR
		rts

outstr:		ldy	#$FF
		sty	$AF
@1:		inc	$AF
		ldy	$AF
		lda	(POKADR),Y
		pha
		cmp	#$9B
		beq	@2
		and	#$7F
		beq	@3
@2:		jsr	BA99
@3:		pla
		bpl	@1
		rts

B581:		lda	#$20
		jsr	BA99
B586:		jsr	outstr
		lda	#$20
		jmp	BA99

B58E:		ldy	#0
		lda	(STMCUR),Y
		sta	FR0
		iny
		lda	(STMCUR),Y
		sta	FR0+1
		jsr	IFP
		jsr	FASC
		lda	INBUFF
		sta	POKADR
		lda	INBUFF+1
		sta	POKADR+1
		jsr	B586
B5AA:		ldy	#2
		lda	(STMCUR),Y
		sta	$9F
		iny
@loop_y:	lda	(STMCUR),Y
		sta	$A7
		iny
		sty	$A8
		jsr	B5C2
		ldy	$A7
		cpy	$9F
		bcc	@loop_y
		rts

B5C2:		jsr	B663
		cmp	#$36
		beq	B5E0
		jsr	B66F
		jsr	B663
		cmp	#$37
		beq	@1
		cmp	#2
		bcs	B5E0
@1:		jsr	B661
		jsr	BA99
		jmp	@1

B5E0:		jsr	B661
		bpl	@1
		and	#$7F
		sta	$AF
		ldx	#0
		lda	VNTP+1
		ldy	VNTP
		jsr	B53E
		jsr	outstr
		cmp	#$A8
		bne	B5E0
		jsr	B661
		jmp	B5E0
@1:		cmp	#15
		beq	@3
		bcs	@6
		jsr	AB45
		dec	$A8
		jsr	FASC
		lda	INBUFF
		sta	POKADR
		lda	INBUFF+1
		sta	POKADR+1
@2:		jsr	outstr
		jmp	B5E0
@3:		jsr	B661
		sta	$AF
		lda	#$22
		jsr	BA99
		lda	$AF
		beq	@5
@4:		jsr	B661
		jsr	BA99
		dec	$AF
		bne	@4
@5:		lda	#$22
		jsr	BA99
		jmp	B5E0
@6:		sec
		sbc	#$10
		sta	$AF
		ldx	#0
		lda	#>A7DE
		ldy	#<A7DE
		jsr	B53E
		jsr	B663
		cmp	#$3D
		bcs	@2
		ldy	#0
		lda	(POKADR),Y
		and	#$7F
		jsr	A3EC
		bcs	@2
		jsr	B581
		jmp	B5E0

B661:		inc	$A8
B663:		ldy	$A8
		cpy	$A7
		bcs	B66C
		lda	(STMCUR),Y
		rts

B66C:		pla
		pla
		rts

B66F:		sta	$AF
		ldx	#2
		lda	#>A49F
		ldy	#<A49F
		jsr	B53E
		jmp	B586

B67D:		jsr	B883		; execute FOR
		jsr	AADA
		lda	$D3
		ora	#$80
		pha
		jsr	B823
		lda	#$C
		jsr	B871
		jsr	AC06
		ldx	#$D4
		ldy	#0
		jsr	B888
		jsr	ZFR0
		lda	#1
		sta	FR0+1
		lda	#$40
		sta	FR0
		jsr	B904
		bcs	@1
		jsr	AC06
@1:		ldx	#$D4
		ldy	#6
		jsr	B888
		pla
B6B5:		pha
		lda	#4
		jsr	B871
		pla
		ldy	#0
		sta	($C4),Y
		lda	(STMCUR),Y
		iny
		sta	($C4),Y
		lda	(STMCUR),Y
		iny
		sta	($C4),Y
		ldx	$B3
		dex
		txa
		iny
		sta	($C4),Y
		rts

B6D2:		jsr	B6F9		; execute GOSUB

B6D5:		jsr	ABCD		; execute GOTO, GO TO
B6D8:		lda	FR0+1
		sta	$A1
		lda	FR0
		sta	$A0
B6E0:		jsr	A9A2
		bcs	B6EA
		pla
		pla
		jmp	A95E

B6EA:		jsr	B6F0
		jsr	B91C
B6F0:		lda	$BE
		sta	STMCUR
		lda	$BF
		sta	STMCUR+1
		rts
B6F9:		jsr	B883
B6FC:		lda	#0

		.if	BASIC_REVISION = 1
		jmp	B6B5
		.else
		beq	B6B5
		.endif

B700:		ldy	$A8		; execute NEXT
		lda	(STMCUR),Y
		sta	$C7
@1:		jsr	B83E
		bcs	B749
		beq	B749
		cmp	$C7
		bne	@1
		ldy	#6
		jsr	B897
		lda	$E0
		pha
		lda	$C7
		jsr	AB81
		jsr	AD26
		jsr	AC0C
		ldy	#0
		jsr	B897
		pla
		bpl	B732
		jsr	AD20
		bpl	B73A
		rts

B732:		jsr	AD20
		beq	B73A
		bmi	B73A
B739:		rts

		.if	BASIC_REVISION = 1

B73A:		lda	#$10
		jsr	B871
		jsr	YYY
		cmp	#8
		.byte	$d0,$32
		rts

B749:		jsr	B91A

BDA8:		jsr	B83E		; execute RETURN
		.byte	$b0,$16
		bne	BDA8
		jsr	YYY
		cmp	#12
		beq	@1
		cmp	#$1E
		beq	@1
		cmp	#4
		beq	@1
		cmp	#34
		bne	zzz
@1:		rts

		jsr	B914
YYY:		jsr	B816
		bcs	zzz
		ldy	$b2
		dey
		lda	($8A),Y
		sta	$A7
		iny
		lda	($8A),Y
		rts

zzz:		jsr	B6F0
		jsr	B916
B74C:		jsr	B904		; execute RUN
		bcs	B754
		jsr	BAF7
B754:	
		.else

B73A:		lda	#$10
		jsr	B871
		jsr	BDCB
		cmp	#8
		beq	B739
		jmp	BDC2
B749:		jsr	B91A
B74C:		jsr	B904		; execute RUN
		bcs	B754
		jsr	BAF7
B754:		nop

		.endif

		lda	#0
		sta	$A0
		sta	$A1
		jsr	B816
		jsr	A9E1
		bmi	B775
		jsr	B8F1

B766:		jsr	B8B9		; execute CLR
		jsr	B8A8
		lda	#0
		sta	DATALN
		sta	DATALN+1
		sta	DATAD
		rts

B775:		jmp	A050

B778:		jsr	AC06		; execute IF
		lda	FR0+1
		beq	B787
		jsr	B904

		.if	BASIC_REVISION = 1
		bcs	@1
		jmp	B6D5
@1:		rts
		.else
		bcs	B78B
		jmp	B6D5
		.endif

B787:		lda	$9F
		sta	$A7
B78B:		rts

B78C:		jsr	B7A6		; execute END
		jmp	A050

B792:		jsr	B7A6		; execute STOP
		jsr	BD79

		lda	#<STOPPED_STR
		sta	POKADR
		lda	#>STOPPED_STR
		sta	POKADR+1

		jsr	outstr
		jmp	B968
B7A6:		jsr	A9E1
		bmi	B7B2
		sta	STOPLN+1
		dey
		lda	(STMCUR),Y
		sta	STOPLN
B7B2:		jmp	BD5B

		.if	BASIC_REVISION = 1
STOPPED_STR:	BasicString "STOPPED "
		.endif

B7B5:		jsr	A9E1		; execute CONT
		bpl	B7B2
		lda	STOPLN
		sta	$A0
		lda	STOPLN+1
		sta	$A1
		jsr	A9A2
		jsr	A9E1
		bmi	B775
		jsr	A9DC
		jsr	A9D0
		jsr	A9E1
		bmi	B775
		jmp	B819

B7D8:		jsr	ABD7		; execute TRAP
		lda	FR0
		sta	$BC
		lda	FR0+1
		sta	$BD
		rts

B7E4:		jsr	B883		; execute ON
		jsr	ABE0
		lda	FR0
		.if	BASIC_REVISION = 1
		.byte	$f0,$20
		.else
		beq	B811
		.endif
		ldy	$A8
		dey
		lda	(STMCUR),Y
		cmp	#$17
		.if	BASIC_REVISION > 1
		php
		.endif
		beq	B7FB
		jsr	B6FC
B7FB:		lda	FR0
		sta	$B3
B7FF:		jsr	ABCD
		dec	$B3
		.if	BASIC_REVISION = 1
		.byte	$f0,$06
		.else
		beq	B812
		.endif
		jsr	B904
		bcc	B7FF
		.if	BASIC_REVISION = 1
B811:
B812:
		rts
		jmp	B6D8
		rts
		.else
		plp
		beq	B811
		jsr	B83E
B811:		rts
B812:		plp
		jmp	B6D8
		.endif

B816:		jsr	A9A2
B819:		ldy	#2
		lda	(STMCUR),Y
		sta	$9F
		iny
		sty	$A7
		rts
B823:		sta	$C7
		jsr	B87A
B828:		jsr	B83E
		bcs	B835
		beq	B835
		cmp	$C7
		.if	BASIC_REVISION = 1
		.byte	$f0,$0B
		.else
		beq	B83D
		.endif
		bne	B828

B835:		lda	$C4
		sta	MEMTOP
		lda	$C5
		sta	MEMTOP+1
B83D:		rts

		.if	BASIC_REVISION = 1
		rts
		.endif

B83E:		lda	RUNSTK+1	; execute POP
		cmp	MEMTOP+1
		.if	BASIC_REVISION = 1
		.byte	$90,$08
		.else
		bcc	B84A
		.endif
		lda	RUNSTK
		cmp	MEMTOP

		.if	BASIC_REVISION = 1
		.byte	$90,$02
B84A:		sec
		rts
		lda	#4
		jsr	B872
		.else
		bcs	B83D
B84A:		lda	#4
		ldx	#$90
		jsr	A8F7
		.endif
	
		ldy	#3
		lda	(MEMTOP),Y
		sta	$B2
		dey
		lda	(MEMTOP),Y
		sta	$A1
		dey
		lda	(MEMTOP),Y
		sta	$A0
		dey
		lda	(MEMTOP),Y
		beq	B86F
		pha
		lda	#12
	
		.if	BASIC_REVISION > 1
		ldx	#$90
		.endif

		.if	BASIC_REVISION = 1
		jsr	B872
		.else
		jsr	A8F7
		.endif

		pla
B86F:		clc
		rts

B872:		.if	BASIC_REVISION = 1
		tay
		ldx	#$90
		jmp	A8F7
		.endif

B871:		jsr	B87A
		tay
		ldx	#$90
		jmp	A87A
B87A:		ldx	MEMTOP
		stx	$C4
		ldx	MEMTOP+1
		stx	$C5
		rts
B883:		ldy	$A8
		sty	$B3
		rts

B888:		lda	#6
		sta	$C6
B88C:		lda	0,X
		sta	($C4),Y
		inx
		iny
		dec	$C6
		bne	B88C
		rts

B897:		lda	#$6
		sta	$C6
		ldx	#$E0
B89D:		lda	(MEMTOP),Y
		sta	$0,X
		inx
		iny
		dec	$C6
		bne	B89D
		rts

B8A8:		lda	STARP
		sta	RUNSTK
		sta	MEMTOP
		sta	APPMHI
		lda	STARP+1
		sta	RUNSTK+1
		sta	MEMTOP+1
		sta	APPMHI+1
		rts

B8B9:		ldx	VVTP
		stx	$F5
		ldy	VVTP+1
		sty	$F6
B8C1:		ldx	$F6
		cpx	STMTAB+1
		bcc	B8CE
		ldx	$F5
		cpx	STMTAB
		bcc	B8CE
		rts

B8CE:		ldy	#0
		lda	($F5),Y
		and	#$FE
		sta	($F5),Y
		ldy	#2
		ldx	#6
		lda	#0
B8DC:		sta	($F5),Y
		iny
		dex
		bne	B8DC
		lda	$F5
		clc
		adc	#8
		sta	$F5
		lda	$F6
		adc	#0
		sta	$F6
		bne	B8C1

		; fill B6..BB -> 0

B8F1:		.if	BASIC_REVISION = 1

		ldy	#0
		sty	$BA
		sty	$BB
		sty	$B9
		sty	$FB
		sty	$B6
		sty	$B7
		sty	$B8
		dey
		sty	$BD
		sty	$11
		.else
		ldx	#5
		ldy	#0
B8F5:		sty	$B6,X
		dex
		bpl	B8F5

		sty	$FB
		dey
		sty	$BD
		sty	BRKKEY
		.endif

		jmp	BD45

B904:		ldx	$A8
		inx
		cpx	$A7
		rts

B90A:		inc	$B9
B90C:		inc	$B9
B90E:		inc	$B9
B910:		inc	$B9
B912:		inc	$B9		; execute "ERROR-"
B914:		inc	$B9
B916:		inc	$B9
B918:		inc	$B9
B91A:		inc	$B9
B91C:		inc	$B9
B91E:		inc	$B9
B920:		inc	$B9
B922:		inc	$B9
B924:		inc	$B9
B926:		inc	$B9
B928:		inc	$B9
B92A:		inc	$B9
B92C:		inc	$B9
B92E:		inc	$B9
B930:		inc	$B9
		inc	$B9
B934:		lda	#0
		sta	$2FE
		jsr	B7A6
		lda	$BD
		bmi	@1
		sta	$A1
		lda	$BC
		sta	$A0
		lda	#$80
		sta	$BD
		lda	$B9
		sta	ERRSAVE
		lda	#0
		sta	$B9
		jmp	B6E0
@1:		jsr	BD79
		lda	#$37
		jsr	B66F
		lda	$B9
		sta	FR0
		lda	#0
		sta	FR0+1
		jsr	B993
B968:		jsr	A9E1
		bmi	@1
		lda	#<ATLINE_STR
		sta	POKADR
		lda	#>ATLINE_STR
		sta	POKADR+1
		jsr	outstr
		ldy	#1
		lda	(STMCUR),Y
		sta	FR0+1
		dey
		lda	(STMCUR),Y
		sta	FR0
		jsr	B993
@1:		jsr	BD79
		lda	#0
		sta	$B9
		.if	BASIC_REVISION > 1
		jsr	BD5B
		.endif
		jmp	A060

B993:		jsr	IFP
		jsr	FASC
		lda	INBUFF
		sta	POKADR
		lda	INBUFF+1
		sta	POKADR+1
		.if	BASIC_REVISION = 1
		jsr	outstr
		rts
		.else
		jmp	outstr
		.endif

ATLINE_STR:	BasicString " AT LINE "

B9AD:		jsr	ABE0		; execute SETCOLOR
		lda	FR0
		cmp	#5
		bcs	B9D0
		pha
		jsr	ABD7
		lda	FR0
		asl
		asl
		asl
		asl
		pha
		jsr	ABD7
		pla
		clc
		adc	FR0
		tay
		pla
		tax
		tya
		sta	$2C4,X
		rts

B9D0:		jsr	B92E

B9D3:		jsr	ABE0		; execute SOUND
		lda	FR0
		cmp	#4
		bcs	B9D0
		asl
		pha
		lda	#0
		sta	AUDCTL
		lda	#3
		sta	SKCTL
		jsr	ABD7
		pla
		pha
		tax
		lda	FR0
		sta	$D200,X
		jsr	ABD7
		lda	FR0
		asl
		asl
		asl
		asl
		pha
		jsr	ABD7
		pla
		tay
		pla
		tax
		tya
		clc
		adc	FR0
		sta	$D201,X
		rts

BA0C:		jsr	ABD7		; execute POSITION
		lda	FR0
		sta	$55
		lda	FR0+1
		sta	$56
		jsr	ABE0
		lda	FR0
		sta	$54
		rts

BA1F:		jsr	ABD7		; execute COLOR
		lda	FR0
		sta	$C8
		rts

BA27:		jsr	BA0C		; execute DRAWTO
		lda	$C8
		sta	$2FB
		lda	#$11
		ldx	#6
		jsr	BABE
		lda	#12
		sta	$34A,X
		lda	#0
		sta	$34B,X
		jsr	BD29
		jmp	BCBB

BA46:		ldx	#6		; execute GRAPHICS
		stx	$C1
		jsr	BCF7
		jsr	ABD7
		ldx	#<@screendev
		ldy	#>@screendev
		stx	INBUFF
		sty	INBUFF+1
		ldx	#6
		lda	FR0
		and	#$F0
		eor	#$1C
		tay
		lda	FR0
		jsr	BBD8
		jmp	BCBB

@screendev:	.byte	"S:",$9B

BA6C:		jsr	BA0C		; execute PLOT
		lda	$C8
		ldx	#6
		jmp	BA9B

BA76:		.if	BASIC_REVISION = 1

		ldx	$B4
		bne	BA92
		lda	#$9B
		jsr	BA99
BA89:		ldx	$B4
		bne	BA92
		lda	$C2
		jsr	BA99
BA92:		ldx	$B4
		lda	#5
		jsr	BABE
		jsr	BD0F
		jmp	BCBB

		.else
		sec
		lda	0,X
		and	#$7F
		sbc	#$40
		bcc	@rts
		sta	$F5
		sta	$F7
		txa
		adc	$F5
		inx
		inx
		inx
		inx
		inx
		inx
		stx	$F5
		tax
@loop:		inx
		cpx	$F5
		bcs	@rts
		lda	0,X
		beq	@loop
@rts:		rts

		.endif

BA99:		ldx	$B5
BA9B:		pha
		jsr	BAC0
		lda	$34A,X
		sta	ICAX1Z
		lda	$34B,X
		sta	ICAX2Z
		pla
		tay
		jsr	BAB2
		tya
		jmp	BCBE

BAB2:		lda	$347,X
		pha
		lda	$346,X
		pha
		tya
		ldy	#$92
		rts

BABE:		sta	$C0
BAC0:		stx	$C1
		jmp	BCAF

BAC5:		lda	#4		; execute ENTER
		jsr	BAD7
		sta	$B4
		jmp	A060

BACF:		lda	#8
		jsr	BAD7
		sta	$B5
		rts

BAD7:		pha
		ldy	#7
		sty	$C1
		jsr	BCAF
		lda	#12
		jsr	BD2B
		ldy	#3
		sty	$C0
		pla
		ldy	#0
		jsr	BC02
		lda	#7
		rts

		.if	BASIC_REVISION > 1
FP_two:		.byte	$40,$02,$00,$00,$00,$00
		.endif

BAF7:		lda	#$FF
		bne	W1

BAFB:		lda	#0	; execute LOAD
W1:		pha
		lda	#4
		jsr	BAD7
		pla
W2:		pha
		lda	#7
		sta	$C0
		sta	$CA
		jsr	BCAF
		ldy	#14
		jsr	BD15
		jsr	BCBB
		lda	$580
		ora	$581
		bne	@7
		ldx	#$8C
@3:		clc
		lda	LOMEM
		adc	$500,X
		.if	BASIC_REVISION > 1
		php
		clc
		.if	BASIC_REVISION > 2
		adc	#0
		.else
		adc	#$10
		.endif
		.endif
		tay
		lda	LOMEM+1
		adc	$501,X
		.if	BASIC_REVISION > 1
		plp
		adc	#0
		.endif
		cmp	HIMEM+1
		bcc	@5
		bne	@4
		cpy	HIMEM
		bcc	@5
@4:		jmp	B90E
@5:		sta	1,X
		sty	0,X
		dex
		dex
		cpx	#$82
		bcs	@3
		jsr	W9
		jsr	B766
		lda	#0
		sta	$CA
		pla
		beq	@6
		rts
@6:		jmp	A050

@7:		lda	#0
		sta	$CA
		jsr	B90A
	
		.if	BASIC_REVISION > 1

BB64:		lda	#4		; execute CLOAD
		jsr	BBB4
		lda	#0
		beq	W2
		.endif

BB6D:		lda	#8		; execute SAVE
		jsr	BAD7
BB72:		lda	#$B
		sta	$C0
		ldx	#$80
@8:		sec
		lda	0,X
		sbc	LOMEM
		sta	$500,X
		inx
		lda	0,X
		sbc	LOMEM+1
		sta	$500,X
		inx
		cpx	#$8E
		bcc	@8
		jsr	BCAF
		ldy	#$E
		jsr	BD15
		jsr	BCBB
W9:		jsr	BCAF
		lda	VNTP
		sta	INBUFF
		lda	VNTP+1
		sta	INBUFF+1
		ldy	$58D
		dey
		tya
		ldy	$58C
		jsr	BD17
		jsr	BCBB
		jmp	BCF7

BBB4:		.if	BASIC_REVISION = 1
BBD1:		lda	#8		; execute CSAVE
		jsr	ZZZ
		jmp	BB72

BB64:		lda	#4		; execute CLOAD
		jsr	ZZZ
		lda	#0
		jmp	W2
		.else
		nop
		nop
		.endif

ZZZ:		pha
		ldx	#<BBCE
		stx	INBUFF
		ldx	#>BBCE
		stx	INBUFF+1
		ldx	#7
		pla
		tay
		lda	#$80
		jsr	BBD8
		jsr	BCBB
		lda	#7
		rts

BBCE:		.byte	"C:",$9B

		.if	BASIC_REVISION > 1

BBD1:		lda	#8	; execute CSAVE
		jsr	BBB4
		bne	BB72
		.endif

BBD8:		pha
		lda	#3
		jsr	BABE
		pla
		sta	$34B,X
		tya
		sta	$34A,X
		jsr	BD1E
		jmp	__DA51

BBEC:		jsr	BD09	; execute XIO
		jmp	BBF4

BBF2:		lda	#3	; execute OPEN
BBF4:		sta	$C0
		jsr	BCA8
		jsr	BD09
		pha
		jsr	BD09
		tay
		pla
BC02:		pha
		tya
		pha
		jsr	AADA
		jsr	BD7D
		jsr	BCAF
		pla
		sta	$34B,X
		pla
		sta	$34A,X
		jsr	BD0F
		jsr	BD9D
		jsr	__DA51
		jmp	BCBB

BC22:		lda	#12		; execute CLOSE
BC24:		sta	$C0
		jsr	BCA8
BC29:		jsr	BD29
		jmp	BCBB

BC2F:		jsr	BCA8		; execute STATUS
		lda	#13
		jsr	BD2B
		jsr	BD00
		jmp	BD31

BC3D:		lda	#$26		; execute NOTE
		jsr	BC24
		lda	$34C,X
		ldy	$34D,X
		jsr	BD33
		jsr	BCAF
		lda	$34E,X
		jmp	BD31

BC54:		jsr	BCA8		; execute POINT
		jsr	ABCD
		jsr	BCAF
		lda	FR0
		sta	$34C,X
		lda	FR0+1
		sta	$34D,X
		jsr	ABCD
		jsr	BCAF
		lda	FR0
		sta	$34E,X
		lda	#$25
		sta	$C0
		.if	BASIC_REVISION = 1
		jmp	BC29
		.else
		bne	BC29
		.endif

BC78:		jsr	BCA8		; execute PUT
		jsr	ABD7
		lda	FR0
		ldx	$C1
		jmp	BA9B

BC85:		.if	BASIC_REVISION > 1	; execute GET
		jsr	__DA51
		.endif
		jsr	BCA8
BC8B:		lda	#7
		sta	$C0
		ldy	#1
		jsr	BD15
		jsr	BCBB
		ldy	#0
		lda	(INBUFF),Y
		jmp	BD31

BC9E:		jsr	BA0C		; execute LOCATE
		ldx	#6
		jsr	BAC0
		bne	BC8B
BCA8:		jsr	BD07
		sta	$C1
		beq	BCB8
BCAF:		lda	$C1
		asl
		asl
		asl
		asl
		tax
		.if	BASIC_REVISION = 1
		.byte	$30,$01
		.else
		bpl	BD06
		.endif
		.if	BASIC_REVISION = 1
		rts
		.endif
BCB8:		jsr	B90C
BCBB:		jsr	BD00
BCBE:		.if	BASIC_REVISION = 1
		.byte	$30,$01
		rts
		.else
		bpl	BD06
		.endif
		ldy	#0
		sty	$2FE
		cmp	#$80
		bne	BCD2
		sty	BRKKEY
		lda	$CA
		.if	BASIC_REVISION = 1
		.byte	$f0,$03
		.else
		beq	BD06
		.endif
		jmp	A000

		.if	BASIC_REVISION = 1
		rts
		.endif

BCD2:		ldy	$C1
BCD3:		cmp	#$88
		beq	BCE7
BCD8:		sta	$B9
		cpy	#7
		bne	BCE1
		jsr	BCF7
BCE1:		jsr	BD5B
		jmp	B934
BCE7:		cpy	#7
		bne	BCD8
		ldx	#$5D
		cpx	$C2
		bne	BCD8
		jsr	BCF7
		jmp	A053
BCF7:		jsr	BCAF
		beq	BD06
		lda	#12
		.if	BASIC_REVISION = 1
		jmp	BD2B
		.else
		bne	BD2B
		.endif
BD00:		jsr	BCAF
		lda	ICSTA,X
BD06:		rts

BD07:		inc	$A8
BD09:		jsr	ABCD
		lda	FR0
		rts
BD0F:		ldy	#$FF
		bne	BD15
		ldy	#0
BD15:		lda	#0
BD17:		sta	ICBLH,X
		tya
		sta	ICBLL,X
BD1E:		lda	INBUFF+1
		ldy	INBUFF
		sta	ICBAH,X
		tya
		sta	ICBAL,X
BD29:		lda	$C0
BD2B:		sta	ICCOM,X
		.if	BASIC_REVISION = 1
		jsr	CIOV
		rts
		.else
		jmp	CIOV
		.endif
BD31:		ldy	#0
BD33:		pha
		tya
		pha
		jsr	AC06
		pla
		sta	FR0+1
		pla
		sta	FR0
		jsr	IFP
		jmp	AC0C

BD45:		lda	#0
		ldx	#7
BD49:		sta	$D200,X
		dex
		bne	BD49
		ldy	#7
		sty	$C1
BD53:		jsr	BCF7
		dec	$C1
		bne	BD53
		rts

		.if	BASIC_REVISION > 1
BD5B:		lda	#0
		sta	$B4
		sta	$B5
		rts
		.endif

BD62:		ldx	#6
BD64:		stx	CIX
		lda	BD72,X
		jsr	BA99
		ldx	CIX
		dex
		bpl	BD64
		rts

BD72:		.byte	$9B,"YDAER",$9B

BD79:		ldx	#0
		beq	BD64

		.if	BASIC_REVISION = 1
BD5B:		lda	#0
		sta	$B4
		sta	$B5
		rts
		.endif

BD7D:		jsr	AB90
		lda	FR0
		sta	INBUFF
		lda	FR0+1
		sta	INBUFF+1
		ldy	FR0+2
		ldx	FR0+3
		beq	BD90
		ldy	#$FF
BD90:		lda	(INBUFF),Y
		sta	$97
		sty	$98
		lda	#$9B
		sta	(INBUFF),Y
		sta	$92
		rts
BD9D:		ldy	$98
		lda	$97
		sta	(INBUFF),Y
		lda	#0
		sta	$92
		rts

		.if	BASIC_REVISION = 1

		.byte	$20

		.else

BDA8:		jsr	B83E		; execute RETURN
		bcs	BDC8
		bne	BDA8
		jsr	BDCB
		cmp	#12
		beq	BDDA
		cmp	#$1E
		beq	BDDA
		cmp	#4
		beq	BDDA
		cmp	#$22
		beq	BDDA
BDC2:		jsr	B6F0
		jsr	B916
BDC8:		jsr	B914
BDCB:		jsr	B816
		bcs	BDC2
		ldy	$B2
		dey
		lda	(STMCUR),Y
		sta	$A7
		iny
		lda	(STMCUR),Y
BDDA:		rts

		ldx	$B4
		bne	BDED
		lda	#$9B
		jsr	BA99
BDE4:		ldx	$B4
		bne	BDED
		lda	$C2
		jsr	BA99
BDED:		ldx	$B4
		lda	#5
		jsr	BABE
		jsr	BD0F
		jmp	BCBB

BDFA:		jsr	ABFD
		jsr	AD26
		jmp	ABB2

		.endif
	
BE03:		sec
		rts

BE05:		lda	#4
		bit	FR0
		bpl	BE11
		lda	#2
		bne	BE11
BE0F:		lda	#1
BE11:		sta	$F0
		lda	FR0
		and	#$7F
		sta	FR0
		.if	BASIC_REVISION = 1
		lda	#$5F
		.else
		lda	#$BD
		.endif
		clc
		adc	$FB
		tax
		ldy	#$BE
		jsr	FLD1R
		jsr	FDIV
		bcc	@1
		rts
@1:		lda	FR0
		and	#$7F
		sec
		sbc	#$40
		bmi	@4
		cmp	#4
		bpl	BE03
		tax
		lda	FR0+1,X
		sta	$F1
		and	#$10
		beq	@2
		lda	#2
@2:		clc
		adc	$F1
		and	#3
		adc	$F0
		sta	$F0
		stx	$F1
		jsr	FMOVE
		ldx	$F1
		lda	#0
@3:		sta	$E2,X
		inx
		cpx	#3
		bcc	@3
		jsr	FSUB
@4:		lsr	$F0
		bcc	@5
		jsr	FMOVE
		ldx	#<BECF
		ldy	#>BECF
		jsr	FLD0R
		jsr	FSUB
@5:		ldx	#$E6
		ldy	#5
		jsr	FSTOR
		jsr	FMOVE
		jsr	FMUL
		bcs	BE03
		lda	#6
		ldx	#<BE9F
		ldy	#>BE9F
		jsr	PLYEVL
		ldx	#$E6
		ldy	#5
		jsr	FLD1R
		jsr	FMUL
		lsr	$F0
		bcc	@rts
		clc
		lda	FR0
		beq	@rts
		eor	#$80
		sta	FR0
@rts:		rts

BE9F:		.byte	$BD,$03,$55,$14,$99,$39
		.byte	$3E,$01,$60,$44,$27,$52
		.byte	$BE,$46,$81,$75,$43,$55
		.byte	$3F,$07,$96,$92,$62,$39
		.byte	$BF,$64,$59,$64,$08,$67
		.byte	$40,$01,$57,$07,$96,$32	;  1.57079632
		.byte	$40,$90,$00,$00,$00,$00	; 90.00000000
BEC9:		.byte	$3F,$01,$74,$53,$29,$25
BECF:		.byte	$40,$01,$00,$00,$00,$00	;  1.00000000

BED5:		lda	#0
		sta	$F0
		sta	$F1
		lda	FR0
		and	#$7F
		cmp	#$40
		bmi	@1
		lda	FR0
		and	#%10000000
		sta	$F0
		inc	$F1
		lda	#%01111111
		and	FR0
		sta	FR0
		ldx	#<__DFEA
		ldy	#>__DFEA
		jsr	__DE95
@1:		ldx	#$E6
		ldy	#5
		jsr	FSTOR
		jsr	FMOVE
		jsr	FMUL
		bcs	@rts
		lda	#11
		ldx	#<__DFAE
		ldy	#>__DFAE
		jsr	PLYEVL
		bcs	@rts
		ldx	#$E6
		ldy	#5
		jsr	FLD1R
		jsr	FMUL
		bcs	@rts
		lda	$F1
		beq	@2
		ldx	#$F0
		ldy	#$DF
		jsr	FLD1R
		jsr	FADD
		lda	$F0
		ora	FR0
		sta	FR0
@2:		lda	$FB
		beq	@rts
		ldx	#<BEC9
		ldy	#>BEC9
		jsr	FLD1R
		jsr	FDIV
@rts:		rts

BF41:		sec
		rts

BF43:		lda	#0
		sta	$F1
		lda	FR0
		bmi	BF41
		cmp	#$3F
		beq	@2
		clc
		adc	#1
		sta	$F1
		sta	$E0
		lda	#1
		sta	$E1
		ldx	#4
		lda	#0
@1:		sta	$E2,X
		dex
		bpl	@1
		jsr	FDIV
@2:		lda	#6
		sta	$EF
		ldx	#$E6
		ldy	#5
		jsr	FSTOR
		jsr	FMOVE
		ldx	#<FP_two
		ldy	#>FP_two
		jsr	FLD0R
		jsr	FSUB
		ldx	#$E6
		ldy	#5
		jsr	FLD1R
		jsr	FMUL
@3:		ldx	#$EC
		ldy	#5
		jsr	FSTOR
		jsr	FMOVE
		ldx	#$E6
		ldy	#5
		jsr	FLD0R
		jsr	FDIV
		ldx	#$EC
		ldy	#5
		jsr	FLD1R
		jsr	FSUB
		ldx	#<__DF6C
		ldy	#>__DF6C
		jsr	FLD1R
		jsr	FMUL
		lda	FR0
		beq	@4
		ldx	#$EC
		ldy	#5
		jsr	FLD1R
		jsr	FADD
		dec	$EF
		bpl	@3
@4:		ldx	#$EC
		ldy	#5
		jsr	FLD0R
		lda	$F1
		.if	BASIC_REVISION = 1
		.byte	$f0,$23
		.else
		beq	BFF0
		.endif
		sec
		sbc	#$40
		clc
		ror
		clc
		adc	#$40
		and	#$7F
		sta	$E0
		lda	$F1
		ror
		lda	#1
		bcc	@5
		lda	#$10
@5:		sta	$E1
		ldx	#4
		lda	#0
@6:		sta	$E2,X
		dex
		bpl	@6
 		jsr	FMUL

		.if	BASIC_REVISION > 1
		EntryPoint(BFF0)
		.endif
		rts

		.if	BASIC_REVISION = 1
FP_two:		.byte	$40,$02,$00,$00,$00,$00
		.byte	$30,$30,$20,$20,$60,$70,$70
		.byte	$00,$00,$00,$00,$30,$30,$3c,$7c,$7c,$7c,$64,$64,$67,$e7,$c0,$c0
		.byte	$80,$80,$e0,$e0,$00,$00,$00,$00,$30,$30,$38,$38,$38,$38,$fc,$ec
		.byte	$ec,$0c,$04,$06,$06,$02,$03,$03,$00,$00,$00,$00,$30,$30,$30,$b0
		.byte	$b0,$d8,$f8,$78,$38,$18,$18,$18,$10,$10,$1c,$1c,$00,$00,$00,$00
		.byte	$30,$30,$30,$30,$30,$30,$30,$30,$20,$e0,$e0,$80,$80,$00,$00,$00
		.endif

		.if	BASIC_REVISION = 1
		EntryPoint(BFF0)
		.byte	$00,$00,$00,$00,$cc,$b8,$a4,$90,$a0
BFF9:		rts
		.endif
	
		.if	BASIC_REVISION = 2
		.byte	$00,$00,$2C,$37,"inner"	; prehistoric debris!
		.endif

		.res	BFFA-*

		EntryPoint(BFFA)
		.word	A000
		EntryPoint(BFFC)
		.word	$500
		EntryPoint(BFFE)
		.if	BASIC_REVISION = 1
		.word	BFF9
		.else
		.word	BFF0
		.endif
	
		.end
