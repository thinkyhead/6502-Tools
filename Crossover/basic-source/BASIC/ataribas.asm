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

		.include "sourcebook.inc"

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

COLDSTART:	LDA	LOADFLG		;Y IN MIDDLE OF LOAD
		BNE	@COLD1		; DO COLDSTART
		LDA	WARMST		; IF WARM START
		BNE	WARMSTART	; THEN BRANCH
@COLD1:		LDX	#$FF		; SET ENTRY STACK
		TXS			; TO TOS
		CLD			; CLEAR DECIMAL MODE

;
; XNEW - Execute NEW
;

XNEW:		LDX	LMADR		;LOAD LOW
		LDY	LMADR+1		;MEM VALUE
		STX	LOMEM		; SET LOMEM
		STY	LOMEM+1
		LDA	#0		; RESET MODIFIED
		STA	MEOLFLG		; EOL FLAG
		STA	LOADFLG		; RESET LOAD FLAG
		INY			; ALLOW 256 FOR OUTBUFF
		TXA			;VNTP

		LDX	#VNTP		; GET ZPG DISPC TO VNTP
@CS1:		STA	0,X		; SET TABLE ADR LOW
		INX
		STY	0,X		; SET TABLE ADR HIGH
		INX
		CPX	#MEMTOP+2	; AT LIMIT
		BCC	@CS1		; BR IF NOT

		LDX	#VVTP		; EXPAND VNT BY ONE
		LDY	#1		; FOR END OF VNT
		JSR	EXPLOW		; ZERO BYTE
		LDX	#STARP		; EXPAND STMT TBL
		LDY	#3		; BY 3 BYTES
		JSR	EXPLOW		; GO DO IT

		LDA	#0		; SET 0
		TAY
		STA	(VNTD),Y	; INTO VVTP
		STA	(STMCUR),Y	; INTO STMCUR+0
		INY
		LDA	#$80		; $80 INTO
		STA	(STMCUR),Y	; STMCUR+1
		INY
		LDA	#$03		; $03 INTO
		STA	(STMCUR),Y	; STMCUR+2

		LDA	#10		; SET PRINT TAB
		STA	PTABW		; WIDTH TO 10

;
; Warm Start
;
		;	WARMSTART - BASIC RESTART
		;	            DOES NOT DESTROY CURRENT PGM
WARMSTART:	JSR	RUNINIT		; INIT FOR RUN
SNX1:		JSR	CLSALL		; GO CLOSE DEVICE 1-7
SNX2:		JSR	SETDZ		; SET E/L DEVICE 0
		LDA	MEOLFLG		; IF AN EOL INSERTED
		BEQ	SNX3
		JSR	RSTSEOL		; THEN UN-INSERT IT
SNX3:		JSR	PREADY		; PRINT READY MESSAGE

;
; Syntax
;

;
; Editor - Get Lines of Input
;

SYNTAX:		LDA	LOADFLG		; IF LOAD IN PROGRESS
		BNE	COLDSTART	; GO DO COLDSTART
		LDX	#$FF		; RESTORE STACK
		TXS
		JSR	INTLBF		; GO INT LBUFF
		LDA	#EPCHAR		; ]
		STA	PROMPT
		.if	BASIC_REVISION = 1
		JSR	GLGO		;
		JSR	TSTBRK		; TEST BREAK
		BNE	SYNTAX		; BR IF BREAK
		.else
		jsr	GLGO2
		jsr	TSTBRK
		beq	SYNTAX
		.endif

		LDA	#0		; INIT CURRENT
		STA	CIX		;INPUT INDEX TO ZERO
		STA	MAXCIX
		STA	COX		;OUTPUT INDEX TO ZERO
		STA	DIRFLG		;SET DIRECT SMT
		STA	SVONTX		; SET SAVE ONT CIX
		STA	SVONTC
		STA	SVVVTE		; VALUE IN CASE
		LDA	VNTD		; OF SYNTAX ERROR
		STA	SVVNTP
		LDA	VNTD+1
		STA	SVVNTP+1

		JSR	SKBLANK		; SKIP BLANKS
		JSR	GETLNUM		;CONVERT AND PUT IN BUFFER
		JSR	_SETCODE	; SET DUMMY FOR LINE LENGTH
		LDA	BININT+1
		BPL	@SYN0
		STA	DIRFLG
@SYN0:		JSR	SKBLANKS	; SKIP BLANKS
		LDY	CIX		;GET INDEX
		STY	STMSTRT		;SAVE INCASE OF SYNTAX ERROR
		LDA	(INBUFF),Y	;GET NEXT CHAR
		CMP	#CR		;IS IT CR
		BNE	SYN1		;BR NOT CR
		BIT	DIRFLG		; IF NO LINE NO.
		BMI	SYNTAX		; THEN NO. DELETE
		JMP	SDEL		;GO DELETE STMT

_XIF:
SYN1:		LDA	COX		;SAVE COX
		STA	STMLBD		;AS PM TO STMT LENGTH BYTE
		JSR	_SETCODE	; DUMMY FOR STMT LENGTH

		JSR	SKBLANK		;GO SKIP BLANKS
		LDA	#>SNTAB		; SET UP FOR STMT
		LDY	#<SNTAB		;NAME SEARCH
		LDX	#2
		JSR	SEARCH		;AND DO IT
		STX	CIX
		LDA	STENUM		;GET STMT NUMBER
		JSR	_SETCODE	;GO SET CODE
		JSR	SKBLANK
		JSR	SYNENT		;AND GO SYNTAX HIM
		BCC	SYNOK		;BR IF OK SYNTAX
					;ELSE SYNTAX ERROR
		LDY	MAXCIX		; GET MAXCIX
		LDA	(INBUFF),Y	; LOAD MAXCIX CHAR
		CMP	#CR		; WAS IT CR
		BNE	@SYN3A		; BR IF NOT CR
		INY			; MOVE CR RIGHT ONE
		STA	(INBUFF),Y
		DEY			; THEN PUT A
		LDA	#' '		; BLANK IN ITS PLACE
@SYN3A:		ORA	#$80		; SET MAXCIX CHAR
		STA	(INBUFF),Y	; TO FLASH

		LDA	#$40		;INDICATE SYNTAX ERROR
		ORA	DIRFLG
		STA	DIRFLG		; IN DIRFLG
		LDY	STMSTRT		;RESTORE STMT START
		STY	CIX
		LDX	#3		;SET FOR FIRST STMT
		STX	STMLBD
		INX			;INC TO CODE
		STX	COX		;AND SET COX
		LDA	#CERR		; GARBAGE CODE
SYN3:		JSR	_SETCODE	;GO SET CODE

_XDATA:		LDY	CIX		;GET INDEX
		LDA	(INBUFF),Y	;GET INDEX CHAR
		INC	CIX		;INC TO NXT
		CMP	#CR		;IS IT CR
		BNE	SYN3		;BR IF NOT
		JSR	_SETCODE

SYNOK:		LDA	COX		; GET DISPL TO END OF STMT
		LDY	STMLBD
		STA	(OUTBUFF),Y	;SET LENGTH BYTE
		LDY	CIX		;GET INPUT DISPL
		DEY
		LDA	(INBUFF),Y	;GET LAST CHAR
		CMP	#CR		;IS IT CR
		BNE	SYN1		;BR IF NOT

		LDY	#2		; SET LINE LENGTH
		LDA	COX		; INTO STMT
		STA	(OUTBUFF),Y

		JSR	GETSTMT		;GO GET STMT
		LDA	#0
		BCS	@8
		JSR	GETLL		;GO GET LINE LENGTH
@8:		SEC
		SBC	COX		;ACU=LENGTH[OLD-NEW]
		BEQ	@10		; BR NEW=OLD
		BCS	@9		;BR OLD<NEW
					;OLD<NEW
		EOR	#$FF		;COMPLEMENT RESULT
		TAY
		INY
		LDX	#STMCUR		;POINT TO STMT CURRENT
		JSR	EXPLOW		;GO EXPAND
		LDA	SVESA		;RESET STMCUR
		STA	STMCUR
		LDA	SVESA+1
		STA	STMCUR+1
		BNE	@10

@9:		.if	BASIC_REVISION = 1
		PHA			; CONTRACT LENGTH
		JSR	GNXTL
		PLA
		TAY
		.else
		tay
		jsr	GNXTL
		.endif

		LDX	#STMCUR		;POINT TO STMT CURRENT
		JSR	CONTLOW1	;GO CONTRACT

@10:		LDY	COX		; STMT LENGTH
@11:		DEY			; MINUS ONE
		LDA	(LOMEM),Y	; GET BUFF CHAR
		STA	(STMCUR),Y	;PUT INTO STMT TBL
		TYA			; TEST END
		BNE	@11		; BR IF NOT
		BIT	DIRFLG		;TEST FOR SYNTAX ERROR
		BVC	L13		;BR IF NOT
		LDA	SVVVTE		; CONTRACT VVT
		ASL
		ASL
		ASL

		.if	BASIC_REVISION = 1
		TAY
		.endif

		LDX	#ENDVVT
		JSR	CONTLOW
		SEC
		LDA	VNTD		; CONTRACT VNT
		SBC	SVVNTP
		TAY
		LDA	VNTD+1
		SBC	SVVNTP+1
		LDX	#VNTD
		JSR	CONTRACT
		BIT	DIRFLG		; IF STMT NOT DIRECT
		BPL	@12		; THE BRANCH
		JSR	LDLINE		; ELSE LIST DIRECT LINE
		JMP	SYNTAX		; THEN BACK TO SYNTAX
@12:		JSR	LLINE		; LIST ENTIRE LINE

SYN9:		JMP	SYNTAX
L13:		BPL	SYN9
		JMP	EXECNL		; GO TO PROGRAM EXECUTOR

SDEL:		JSR	GETSTMT		; GO GET LINE
		BCS	SYN9		; BR NOT FOUND
		JSR	GETLL		;GO GET LINE LENGTH

		.if	BASIC_REVISION = 1
		PHA			; Y
		JSR	GNXTL
		PLA
		TAY
		.else
		tay
		jsr	GNXTL
		.endif

		LDX	#STMCUR		;GET STMCUR DISPL
		JSR	CONTLOW1	; GO DELETE
		JMP	SYNTAX		;GO FOR NEXT LINE

;
; Get a Line Number
;
;	GETLNUM - GET A LINE NO FROM ASCLT IN INBUFF
;		  TO BINARY INTO OUTBUFF
GETLNUM:	JSR	CVAFP		; GO CONVERT LINE #
		BCC	@GLNUM		; BR IF GOOD LINE #

@GLN1:		LDA	#0		; SET LINE #
		STA	CIX
		LDY	#$80		; =$8000
		BMI	@SLNUM

@GLNUM:		JSR	CVFPI		; CONVERT FP TO INT
		LDY	BININT+1	; LOAD RESULT
		BMI	@GLN1		; BR IF LNO>32767
		LDA	BININT

@SLNUM:		STY	TSLNUM+1	; SET LINE # HIGH
		STA	TSLNUM		; AND LOW
		JSR	_SETCODE	; OUTPUT LOW
		LDA	TSLNUM+1	; OUTPUT HI
		STA	BININT+1
		JMP	_SETCODE	; AND RETURN
;
; SYNENT - PERFORM LINE PRE-COMPILE
;
SYNENT:		LDY	#1		; GET PC HIGH
		LDA	(POKADR),Y
		STA	CPC+1		;SET PGM COUNTERS
		STA	SPC+1
		DEY
		LDA	(POKADR),Y
		STA	CPC
		STA	SPC
		.if	BASIC_REVISION = 1
		LDA	#0		;SET STKLVL
		STA	STKLVL		;SET STKLVL
		.else
		sty	STKLVL
		.endif
		LDA	COX		;MOVE
		STA	SOX		;COX TO SOX
		LDA	CIX		;MOVE
		STA	SIX		;CIX TO SIX

; NEXT		GET NEXT SYNTAX CODE
;		AS LONG AS NOT FAILING

NEXT:		JSR	NXSC		; GET NEXT CODE

		BMI	@ERNTV		; BR IF REL-NON-TERMINAL

		CMP	#1		; TEST CODE=1
		BCC	@4		; BR CODE=0 [ABS-NON-TERMINAL]
		BNE	@TSTSUC		; BR CODE >1

		JSR	@4		; CODE=1 [EXTERNAL SUBROUTINE]
		.if	BASIC_REVISION = 1
		BCC	NEXT		; BR IF SUB REPORTS SUCCESS
		JMP	FAIL		; ELSE GO TO FAIL CODE
		.else
		jmp	FAIL2
		.endif

@TSTSUC:	CMP	#5		; TEST CODE = 5
		BCC	POP		; CODE = [2,3,or 4] POP UP TO
					; NEXT SYNTAX CODE
		JSR	TERMTST		; CODE=5 GO TEST TERMINAL

		.if	BASIC_REVISION = 1
		BCC	NEXT		; BR IF SUCCESS
		JMP	FAIL		; ELSE GO TO FAIL CODE
		.else
		jmp	FAIL2
		.endif

@ERNTV:		SEC			; RELATIVE NON TERMINAL

		.if	BASIC_REVISION = 1
		LDX	#0		; TOKEN MINUS
		.endif

		SBC	#$C1
		BCS	@ERN1		; BR IF RESULT PLUS
		LDX	#$FF		; ADD A MINUS
@ERN1:		CLC
		ADC	CPC		; RESULT PLUS CPC
		PHA			; IS NEW CPC-1
		TXA
		ADC	CPC+1
		PHA			; SAVE NEW PC HIGH
		JMP	_PUSH

@4:		JSR	NXSC		; GET NEXT CODE
		PHA			; SAVE ON STACK
		JSR	NXSC		; GET NEXT CODE
		PHA			; SAVE ON STACK
		BCC	_PUSH		; BR IF CODE =0
		PLA			; EXCHANGE TOP
		TAY			; 2 ENTRIES ON
		PLA			; CPU STACK
		TAX
		TYA
		PHA
		TXA
		PHA
NRTS1:		RTS			; ELSE GOTO EXTERNAL SRT VIA RTS

; PUSH		PUSH TO NEXT STACK LEVEL

_PUSH:		LDX	STKLVL		; GET STACK LEVEL
		INX			; PLUS 4
		INX
		INX
		INX
		BEQ	@SSTB		;BR STACK TOO  BIG
		STX	STKLVL		; SAVE NEW STACK LEVEL

		LDA	CIX		; CIX TO
		STA	SIX,X		; STACK IX
		LDA	COX		; COX TO
		STA	SOX,X		; STACK OX
		LDA	CPC		; CPC TO
		STA	SPC,X		; STACK CPC
		LDA	CPC+1
		STA	SPC+1,X

		PLA			; MOVE STACKED
		STA	CPC+1		; PC TO CPC
		PLA
		STA	CPC
		JMP	NEXT		; GO FOR NEXT

@SSTB:		JMP	ERLTL

; POP		LOAD CPC FROM STACK PC
;		AND DECREMENT TO PREVIOUS STACK LEVEL

POP:		LDX	STKLVL		; GET STACK LEVEL

		.if	BASIC_REVISION = 1
		BNE	@POP1		; BR NOT TOP OF STACK
		RTS			; TO SYNTAX CALLER
		.else
		beq	NRTS1
		.endif

@POP1:		LDA	SPC,X		; MOVE STACK PC
		STA	CPC		; TO CURRENT PC
		LDA	SPC+1,X
		STA	CPC+1

		DEX			; X=X-4
		DEX
		DEX
		DEX
		STX	STKLVL

FAIL2:		BCS	FAIL		; BR IF CALLER FAILING
		JMP	NEXT		; ELSE GO TO NEXT

; FAIL
;		TERMINAL FAILED
;		LOOK FOR ALTERNATIVE [OR] OR
;		A RETURN INDICATOR
;
FAIL:		JSR	NXSC		; GET NEXT CODE

		BMI	FAIL		; BR IF RNTV

		CMP	#2		; TEST CODE =2
		BCS	@TSTOR		; BR IF POSSIBLE OR

		JSR	_INCCPC		; CODE = 0 OR 1
		JSR	_INCCPC		; INC PC BY TWO
		BNE	FAIL		; AND CONTINUE FAIL PROCESS

@TSTOR:		CMP	#3		; TEXT CODE=3
		BEQ	POP		; BR CODE =3 [RETURN]
		BCS	FAIL		; CODE>3 [RNTV] CONTINUE

		LDA	CIX		; IF THIS CIX
		CMP	MAXCIX		; IS A NEW MAX
		BCC	@SCIX
		STA	MAXCIX		; THEN SET NEW MAX
@SCIX:		LDX	STKLVL		; CODE=2 [OR]
		LDA	SIX,X		; MOVE STACK INDEXES
		STA	CIX		; TO CURRENT INDEXES
		LDA	SOX,X
		STA	COX
		JMP	NEXT		; TRY FOR SUCCESS HERE

;
; Increment CPC
;
;		INCCPC - INC CPC BY ONE
;
_INCCPC:	INC	CPC
		BNE	@ICPCR
		INC	CPC+1
@ICPCR:		RTS

;
; NXSC - GET NEXT SYNTAX CODE
;
NXSC:		JSR	_INCCPC		; INC PC
		LDX	#0
		LDA	(CPC,X)		; GET NEXT CODE
		RTS			; RETURN

;
; TERMTST - TEST A TERMINAL CODE
;
TERMTST:	CMP	#_CHNG		; TEST CODE=F
		BEQ	ECHNG		; BR CODE = F
		BCS	SRCONT		; BR CODE > F

		.if	BASIC_REVISION > 1
		cmp	#$0D		; TEST CODE=D
		bne	@TT1
		jsr	_INCCPC		; NEXT CPC
		jmp	_SRCNT1
		.endif

@TT1:		PLA			; POP RTN ADR
		PLA
		LDA	#<(_EXP-1)	; PUSH EXP ADR
		PHA			; FOR SPECIAL
		LDA	#>(_EXP-1)	; EXP ANTV CALL
		PHA
		JMP	_PUSH		; GO PUSH

;
; ECHNG - EXTERNAL CODE TO CHANGE COX -1
;
ECHNG:		JSR	_INCCPC		; INC PC TO CODE
		LDY	#0
		LDA	(CPC),Y		; GET CODE
		LDY	COX		; GET COX
		DEY			; MINUS 1
		STA	(OUTBUFF),Y	; SET NEW CODE
		CLC			; SET SUCCESS
_ECRTS:		RTS			; RETURN

_SETCODE:	LDY	COX		;GET COX
		STA	(OUTBUFF),Y	;SET CHAR
		INC	COX		;INC COD

		.if	BASIC_REVISION = 1
		BEQ	@SCOVF		;BR IF NOT ZERO
		RTS			;DONE
		.else
		bne	_ECRTS
		.endif

@SCOVF:		JMP	ERLTL		;GO TO LINE TOO LONG ERR

;
; Exits for IF and REM
;
_EIF:		LDX	#$FF		; RESET STACK
		TXS
		LDA	COX		; SET STMT LENGTH
		LDY	STMLBD
		STA	(OUTBUFF),Y
		JMP	_XIF		; GO CONTINUE IF
_EREM:
_EDATA:		LDX	#$FF		; RESET STACK
		TXS
		JMP	_XDATA		;GO CONTINUE DATA

;
; SRCONT - SEARCH OP NAME TABLE AND TEST RESULT
;
SRCONT:		JSR	SKBLANK		; SKIP BLANKS
_SRCNT1:	LDA	CIX		; GET CURRENT INPUT INDEX
		CMP	SVONTX		; COMPARE WITH SAVED IX
		BEQ	@SONT1		; BR IF SAVED IX SAME
		STA	SVONTX		; SAVE NEW IX

		LDA	#>OPNTAB	; SET UP FOR ONT
		LDY	#<OPNTAB	; SEARCH
		LDX	#0
		JSR	SEARCH		; GO SEARCH
		BCS	@SONF		; BR NOT FOUND
		STX	SVONTL		; SAVE NEW CIX

		.if	BASIC_REVISION = 1
		CLC
		.endif

		LDA	STENUM		; ADD $10 TO
		ADC	#$10		; ENTRY NUMBER TO
		STA	SVONTC		; GET OPERATOR CODE

@SONT1:		LDY	#0
		LDA	($9D),Y		; GET SYNTAX REQ CODE
		CMP	SVONTC		; DOES IT MATCH THE FOUND
		BEQ	@SONT2		; BR IF MATCH
		CMP	#CNFNP		; WAS REQ NFNP

		.if	BASIC_REVISION = 1
		BNE	@SONTF		; BR IF NOT
		LDA	SVONTC		; GET WHAT WE GOT
		CMP	#CNFNP		; IS IT NFNA
		BCS	@SONTS		; BR IF IT IS

@SONTF:		SEC			; REPORT FAIL
		RTS
@SONTS:		LDA	SVONTC		; GET REAL CODE
		.else
		bne	@SONF1
		lda	SVONTC
		cmp	#CNFNP
		bcc	@SONF1
		.endif

@SONT2:		JSR	_SETCODE		; GO SET CODE
		LDX	SVONTL		; INC CIX BY
		STX	CIX
		CLC			; REPORT SUCCESS
		RTS			; DONE

@SONF:		LDA	#0		; SET ZERO AS
		STA	SVONTC		; SAVED CODE
@SONF1:		SEC
		RTS			; DONE

;
; _TNVAR - EXTERNAL SUBROUTINE FOR TNVAR & TSVAR
;
_TNVAR:		LDA	#0		; SET NUMERIC TEST
		BEQ	_TVAR

_TSVAR:		LDA	#$80		; SET STR TEST
_TVAR:		STA	TVTYPE		; SAVE TEST TYPE
		JSR	SKPBLANK	; SKIP LEADING BLANKS
		LDA	CIX		; GET INDEX
		STA	TVSCIX		; FOR SAVING

		JSR	_TSALPH		; GO TEST FIRST CHAR
		BCS	@TVFAIL		; BR NOT ALPHA
		JSR	SRCONT		; IF THIS IS A
		LDA	SVONTC		; RESVD NAME
		BEQ	@TV1		; BR NOT RSVDNAME
		LDY	SVONTL		; IF NEXT CHAR AFTER
		LDA	(INBUFF),Y	; RESERVED NAME
		CMP	#'0'		; NOT ALARM NUMERIC
		BCC	@TVFAIL		; THEN ERROR

@TV1:		INC	CIX		; INC TO NEXT CHAR
		JSR	_TSALPH		; TEST ALPHA
		BCC	@TV1		; BR IF ALPHA
		JSR	__DBAF		; TRY NUMBER
		BCC	@TV1		; BR IF NUMBER

		LDA	(INBUFF),Y	; GET OFFENDING CHAR
		CMP	#'$'		; IS IT $
		BEQ	@TVSTR		; BR IF $ [STRING]
		BIT	TVTYPE		; THIS A NVAR SEARCH
		BPL	@TVOK		; BR IF NVAR

@TVFAIL:	SEC			; SET FAIL CODE
		RTS			; DONE

@TVSTR:		BIT	TVTYPE		; TEST SVAR SEARCH
		BPL	@TVFAIL		; BR IF SVAR
		INY			; INC OVER $
		BNE	@TVOK2		; BR ALWAYS

@TVOK:		LDA	(INBUFF),Y	; GET NEXT CHAR
		CMP	#'('		; IS IT PAREN
		BNE	@TVOK2		; BR NOT PAREN
		INY			; INC OVER PARAN
		LDA	#$40		; OR IN ARRAY
		ORA	TVTYPE		; CODE TO TVTYPE
		STA	TVTYPE

@TVOK2:		LDA	TVSCIX		; GET SAVED CIX
		STA	CIX		; PUT BACK
		STY	TVSCIX		; SAVE NEW CIX

		LDA	VNTP+1		; SEARCH VNT
		LDY	VNTP		; FOR THIS GUY
		LDX	#0
		JSR	SEARCH
@TVRS:		BCS	@TVS0		; BR NOT FOUND
		CPX	TVSCIX		; FOUND RIGHT ONE
		BEQ	@TVSUC		; BR IF YES
		JSR	SRCNXT		; GO SEARCH MORE
		JMP	@TVRS		; TEST THIS RESULT

@TVS0:		SEC			; SIGH:
		LDA	TVSCIX		; VAR LENGTH IS
		SBC	CIX		; NEW CIX-OLD CIX
		STA	CIX

		TAY			; GO EXPAND VNT
		LDX	#VNTD		; BY VAR LENGTH
		JSR	EXPLOW
		LDA	STENUM		; SET VARIABLE NUMBER
		STA	TVNUM

		LDY	CIX		; AND
		DEY
		LDX	TVSCIX		; GET DISPL TO EQU+1
		DEX
@TVS1:		LDA	LBUFF,X		; MOVE VAR TO
		STA	(SVESA),Y
		DEX
		DEY
		BPL	@TVS1

		LDY	CIX		;TURN ON MSB
		DEY			;OF LAST CHAR
		LDA	(SVESA),Y	;IN VTVT ENTRY
		ORA	#$80
		STA	(SVESA),Y

		LDY	#8		; THEN EXPAND
		LDX	#STMTAB		; VVT BY 8
		JSR	EXPLOW
		INC	SVVVTE		; INC VVT EXP SIZE

		LDY	#2		; CLEAR VALUE
		LDA	#0		; PART OF
@TVS1A:		STA	TVTYPE,Y	; ENTRY
		INY
		CPY	#8
		BCC	@TVS1A
		DEY			; AND THEN
@TVS2:		LDA	TVTYPE,Y	; PUT IN VAR TABLE
		STA	(SVESA),Y	; ENTRY
		DEY
		BPL	@TVS2

@TVSUC:		BIT	TVTYPE		; WAS THERE A PAREN
		BVC	@TVNP		; BR IF NOT
		DEC	TVSCIX		; LET SYNTAX SEE PAREN

@TVNP:		LDA	TVSCIX		; GET NEW CIX
		STA	CIX		; TO CIX

		LDA	STENUM		; GET TABLE ENTRY NO
		BMI	@TVFULL		; BR IF > $7F
		ORA	#$80		; MAKE IT > $7F
		.if	BASIC_REVISION = 1
		JSR	_SETCODE		; SET CODE TO OUTPUT BUFFER
		CLC			; SET SUCCESS CODE
		RTS			; RETURN
		.else
		clc
		jmp	_SETCODE
		.endif

@TVFULL:	jmp	ERRVSF		; GO TO ERROR RTN

; TSTALPH	TEST CIX FOR ALPHA

_TSALPH:	LDY	CIX
		LDA	(INBUFF),Y
TSALPH:		CMP	#'A'
		BCC	_TAFAIL
		CMP	#'Z'+1
		RTS

_TAFAIL:	SEC
		RTS

; _TNCON	EXTERNAL SUBROUTINE TO CHECK FOR NUMBER

_TNCON:		JSR	SKBLANK
		LDA	CIX
		STA	TVSCIX
		JSR	CVAFP		; GO TEST AND CONV
		BCC	@TNC1		; BR IF NUMBER
		LDA	TVSCIX
		STA	CIX
		RTS			; RETURN FAIL

@TNC1:		LDA	#CBCD		; SET NUMERIC CONST
		JSR	_SETCODE

		.if	BASIC_REVISION = 1
		LDY	COX
		.else
		iny
		.endif
		LDX	#0
@TNC2:		LDA	FR0,X		; MOVE CONST TO STMT
		STA	(LOMEM),Y
		INY
		INX
		CPX	#6
		BCC	@TNC2
		STY	COX
		CLC
		RTS

;	EXT SRT TO CHECK FOR STR CONST

_TSCON:		JSR	SKBLANK
		LDY	CIX		; GET INDEX
		LDA	(INBUFF),Y	; GET CHAR
		CMP	#'"'		; IS IT DQUOTE

		.if	BASIC_REVISION = 1
		BEQ	@TSC1		; BR IF DQ
		SEC			; SET FAIL
		RTS			; RETURN
		.else
		bne	_TAFAIL
		.endif

@TSC1:		LDA	#CSTOK		; SET SCON CODE
		JSR	_SETCODE
		LDA	COX		; SET COX
		STA	TSCOX		; SAVE FOR LENGTH
		JSR	_SETCODE	; SET DUMMY FOR NOW

@TSC2:		INC	CIX		; NEXT INPUT CHAR
		LDY	CIX
		LDA	(INBUFF),Y
		CMP	#CR		; IS IT CR
		BEQ	@TSC4		; BR IF CR
		CMP	#'"'		; IS IT DQ
		BEQ	@TSC3		; BR IF DQ
		JSR	_SETCODE	; OUTPUT IT
		JMP	@TSC2		; NEXT

@TSC3:		INC	CIX		; INC CIX OVER DQ
@TSC4:		CLC
		LDA	COX		; LENGTH IS COX MINUS
		SBC	TSCOX		; LENGTH BYTE COX
		LDY	TSCOX
		STA	(LOMEM),Y	; SET LENGTH

_TSSUC:		CLC			; SET SUCCESS
		RTS			; DONE

; Search a Table
;
;		TABLE FORMAT:
;		GARBAGE TO SKIP [N]
;		ASCII CHAR      [N]
;		  WITH LEAST SIGNIFICANT BYTE HAVING
;		  MOST SIGNIFICANT BIT ON
;		LAST TABLE ENTRY MUST HAVE FIRST ASCII
;		  CHAR = 0
;
;		ENTRY PARMS:
;		  X = SKIP LENGTH
;		  A,Y = TABLE ADR [HIGH LOW]
;		  ARGUMENT = INBUFF + CIX
;		EXIT PARMS:
;		  CARRY = CLEAR IF FOUND
;		  X = FOUND ARGUMENT END CIX+1
;		  SRCADR = TABLE ENTRY ADR
;		  STENUM = TABLE ENTRY NUMBER
;
SEARCH:		STX	SRCSKP		; SAVE SKIP FACTOR
		LDX	#$FF		; SET ENTRY NUMBER
		STX	STENUM		; TO ZERO

_SRC1:		STA	SRCADR+1	; SET SEARCH ADR
		STY	SRCADR
		INC	STENUM		; INC ENTRY NUMBER
		LDX	CIX		; GET ARG DISPL
		LDY	SRCSKP		; GET SKIP LENGTH
		LDA	(SRCADR),Y	; GET FIRST CHAR
		BEQ	_SRCNF		; BR IF EOT
		LDA	#0		; SET STATUS = EQ
		PHP			; AND PUSH IT

_SRC2:		LDA	LBUFF,X		; GET INPUT CHAR
		AND	#$7F		; TURN OFF MSB
		CMP	#'.'		; IF WILD CARD
		BEQ	_SRC5		; THEN BR
_SRC2A:		EOR	(SRCADR),Y	; EX-OR WITH TABLE CHAR
		ASL			; SHIFT MSB TO CARRY
		BEQ	_SRC3		; BR IF [ARG=TAB] CHAR

		PLA			; POP STATUS
		PHP			; PUSH NEW STATUS

_SRC3:		INY			;INC TABLE INDEX
		INX			;INC ARG INDEX
		BCC	_SRC2		; IF TABLE MSB OFF, CONTINUE
					;ELSE END OF ENTRY
		PLP			;GET STATUS
		.if	BASIC_REVISION = 1
		BEQ	_SRCFND		;BR IF NO MIS MATCH
		.else
		beq	_TSSUC
		.endif

SRCNXT:		CLC
		TYA			;ACV=ENTRY LENGTH
		ADC	SRCADR		;PLUS START ADR [L]
		TAY			;TO Y
		LDA	SRCADR+1	;ETC
		ADC	#0
		BNE	_SRC1		;BR ALLWAYS

_SRCFND:	.if	BASIC_REVISION = 1
		CLC			; INDICATE FOUND
		RTS
		.endif

_SRCNF:		SEC			; INDICATE NOT FOUND
		RTS

_SRC5:		LDA	#2		; IF NOT
		CMP	SRCSKP		; STMT NAME TABLE
		BNE	_SRC2A		; THEN IGNORE
@SRC6:		LDA	(SRCADR),Y	;TEST MSB OF TABLE
		BMI	@SRC7		; IF ON DONE
		INY			; ELSE
		BNE	@SRC6		; LOOK AT NEXT CHAR
@SRC7:		SEC			; INDICATE MSB ON
		BCS	_SRC3		; AND RE-ENTER CODE

; Statement Name Table

;
; SNTAB- STATEMENT NAME TABLE
;	EACH ENTRY HAS SYNTAX TBLE ADR PTR
;	FOLLOWED BY STMT NAME
;
SNTAB:		WordBasicString _SREM-1,"REM"
		WordBasicString _SDATA-1,"DATA"
		WordBasicString _SINPUT-1,"INPUT"
		WordBasicString _SCOLOR-1,"COLOR"
		WordBasicString _SLIST-1,"LIST"
		WordBasicString _SENTER-1,"ENTER"
		WordBasicString _SLET-1,"LET"
		WordBasicString _SIF-1,"IF"
		WordBasicString _SFOR-1,"FOR"
		WordBasicString _SNEXT-1,"NEXT"
		WordBasicString _SGOTO-1,"GOTO"
		WordBasicString _SGOTO-1,"GO TO"
		WordBasicString _SGOSUB-1,"GOSUB"
		WordBasicString _STRAP-1,"TRAP"
		WordBasicString _SBYE-1,"BYE"
		WordBasicString _SCONT-1,"CONT"
		WordBasicString _SCOM-1,"COM"
		WordBasicString _SCLOSE-1,"CLOSE"
		WordBasicString _SCLR-1,"CLR"
		WordBasicString _SDEG-1,"DEG"
		WordBasicString _SDIM-1,"DIM"
		WordBasicString _SEND-1,"END"
		WordBasicString _SNEW-1,"NEW"
		WordBasicString _SOPEN-1,"OPEN"
		WordBasicString _SLOAD-1,"LOAD"
		WordBasicString _SSAVE-1,"SAVE"
		WordBasicString _SSTATUS-1,"STATUS"
		WordBasicString _SNOTE-1,"NOTE"
		WordBasicString _SPOINT-1,"POINT"
		WordBasicString _SXIO-1,"XIO"
		WordBasicString _SON-1,"ON"
		WordBasicString _SPOKE-1,"POKE"
		WordBasicString _SPRINT-1,"PRINT"
		WordBasicString _SRAD-1,"RAD"
		WordBasicString _SREAD-1,"READ"
		WordBasicString _SREST-1,"RESTORE"
		WordBasicString _SRET-1,"RETURN"
		WordBasicString _SRUN-1,"RUN"
		WordBasicString _SSTOP-1,"STOP"
		WordBasicString _SPOP-1,"POP"
		WordBasicString _SPRINT-1,"?"
		WordBasicString _SGET-1,"GET"
		WordBasicString _SPUT-1,"PUT"
		WordBasicString _SGR-1,"GRAPHICS"
		WordBasicString _SPLOT-1,"PLOT"
		WordBasicString _SPOS-1,"POSITION"
		WordBasicString _SDOS-1,"DOS"
		WordBasicString _SDRAWTO-1,"DRAWTO"
		WordBasicString _SSETCOLOR-1,"SETCOLOR"
		WordBasicString _SLOCATE-1,"LOCATE"
		WordBasicString _SOUND-1,"SOUND"
		WordBasicString _SLPRINT-1,"LPRINT"
		WordBasicString _SCSAVE-1,"CSAVE"
		WordBasicString _SCLOAD-1,"CLOAD"

		.word	_SILET-1
		.byte	$00,$80		; silent "LET"

		.byte	$00,$2A
		BasicString "ERROR-  "

		.if	BASIC_REVISION > 1
STOPPED_STR:	BasicString "STOPPED "
		.endif

;
; SYNTAX TABLES
;

; Syntax Table OP Codes
; $00 _ANTV	Absolute Non-Terminal Vector (ANTV) to sub-call another rule
; $01 _ESRT	External Subroutine Call (ESRT) to call a handler for more complex rules
; $02 _OR	ABML or
; $03 _RTN	(aka <END>) Return, marks the end of an ABML rule. Return pass or fail.
; $0E _VEXP	(aka <EXP>) Expression Non-Terminal Vector. Shorthand for ANTV AD(EXP)
; $0F _CHNG	Change Last Token to X. e.g., to rectify '=' as assign or compare.

; Helper to reverse-engineer JS offsets:
; python -c "import sys; print(hex(int(sys.argv[1],0)+(int(sys.argv[2],0) ^ 0x40) - 0x80))" 0xA616 0xD8

.define	JS(adr)	$80 + (((adr - *) & $7F) ^ $40)
.define	AD(adr)	(adr - 1)
.define	CHNG(val) _CHNG, val
.define	UNJS(val) (* + (val ^ $40) - $80)
.define	ANTV(adr) _ANTV, <AD(adr), >AD(adr)
.define	ESRT(adr) _ESRT, <AD(adr), >AD(adr)

; <EXP> = (<EXP>)<NOP> | <UNARY><EXP> | <NV><NOP>#

_EXP:		.if	BASIC_REVISION = 1
		.byte   CLPRN,JS(_EXP),CRPRN,JS(_NOP),_OR,JS(_UNARY),JS(_EXP),_OR
		.else
		.byte	JS(_UNARY),JS(_EXP2),_OR,JS(_EXP2),_RTN
_EXP2:		.byte	CLPRN,JS(_SFUN),CRPRN,JS(_NOP),_OR
		.endif
		.byte	JS(_NV),JS(_NOP),_RTN

; <UNARY> = + | - | NOT#

_UNARY:		.byte	CPLUS,CHNG(CUPLUS),_OR,CMINUS,CHNG(CUMINUS),_OR,CNOT,_RTN

; <NV> = <NFUN> | <NVAR> | <NCON> | <STCOMP>#

_NV:		.byte	JS(_NFUN),_OR,JS(_NVAR),_OR,ESRT(_TNCON),_OR,ANTV(_STCOMP),_RTN

; <NOP> = <OP><EXP> | &#

_NOP:		.byte	JS(_OP),JS(_EXP),_OR,_RTN

; <OP> = ** | + | - | * | / | <= | >= | <> | > | < | = | AND | OR#

_OP:		.byte	CEXP,_OR,CPLUS,_OR,CMINUS,_OR,CMUL,_OR,CDIV,_OR,CLE,_OR,CGE,_OR
		.byte	CNE,_OR,CGT,_OR,CLT,_OR,CEQ,_OR,CAND,_OR,COR,_RTN

; <NVAR> = <TNVAR><NMAT>#

_NVAR:		.byte	ESRT(_TNVAR),JS(_NMAT),_RTN

; <NMAT> = (<EXP><NMAT2>) | &#

_NMAT:		.if	BASIC_REVISION > 1
		.byte	$0D
		.endif
		.byte	CLPRN,CHNG(CALPRN),_VEXP,JS(_NMAT2),CRPRN,_OR,_RTN

; <NMAT2> = ,<EXP> | &#

_NMAT2:		.byte	CCOM,CHNG(CACOM),_VEXP,_OR,_RTN

; <NFUN> = <NFNP><NFP> | <NFSP><SFP> | <NFUSR>#

_NFUN:		.byte	$44,$D2,_OR,ANTV(A7CE)
		.byte	$D3,_OR,$C2,_RTN

; <NFUSR> = USR(<PUSR>)#

_NFUSR:		.byte	$3F,$2B,CHNG(CFLPRN),ANTV(A7D5),CRPRN,_RTN

; <NFP> = (<EXP>)#

_NFP:		.byte	$2B,CHNG(CFLPRN),_VEXP,CRPRN,_RTN

; <SFP> = <STR>)#

_SFP:		.byte	$2B,CHNG(CFLPRN),$C7,CRPRN,_RTN

; <STCOMP> = <STR><SOP><STR>#

_STCOMP:	.byte	$C4,$E3,$C2,_RTN

; <STR> = <SFUN> | <SVAR> | <SCON>#

_SSTR:		.byte	$C8,_OR,$CB,_OR,$01
		.word	_TSCON-1
		.byte	_RTN

; <SFUN> = SFNP<NFP>#

_SFUN:		.byte	$00
		.word	_SFNP-1
		.byte	$A5,_RTN

; <SVAR> = <TSVAR><SMAT>#

_SVAR:		.byte	$01
		.word	_TSVAR-1
		.byte	$C2,_RTN

; <SMAT> = (<EXP><SMAT2>) | &#

_SMAT:		.byte	$2B,CHNG($37),_VEXP,$C4,CRPRN,_OR,_RTN

; <SMAT2> = ,<EXP> | &#

_SMAT2:		.byte	CCOM,CHNG($3C),_VEXP,_OR,_RTN

; <SOP> = <><#

_SOP:		.byte	$1D,CHNG($2F),_OR	; <= CLE :CHNG CSLE :OR
		.byte	$1E,CHNG($30),_OR	; <> CNE :CHNG CSNE :OR
		.byte	$1F,CHNG($31),_OR	; >= CGE :CHNG CSGE :OR
		.byte	$20,CHNG($32),_OR	; <  CLT :CHNG CSLT :OR
		.byte	$21,CHNG($33),_OR	; >  CGT :CHNG CSGT :OR
		.byte	$22,CHNG($34),_RTN	; =  CEQ :CHNG CSEQ :OR

		; PUT statement

; <PUT> = <D1>,<EXP><EOS>#

_SPUT:		.byte	$1C,_VEXP,CCOM

		; COLOR, GOTO, GO TO, GOSUB, TRAP, GRAPHICS statement

_STRAP:
_SGOTO:
_SGOSUB:
_SGR:
_SCOLOR:
_XEOS:		.byte	_VEXP

		; BYE, CONT, CLR, DEG, END, NEW, RAD, RETURN,
		; STOP, POP, DOS, CSAVE, CLOAD statement

_SCSAVE:
_SCLOAD:
_SDOS:
_SCLR:
_SRET:
_SEND:
_SSTOP:
_SPOP:
_SNEW:
_SBYE:
_SCONT:
_SDEG:
_SRAD:		.byte	$FA,_RTN

		; LET, silent LET statement

_SLET:
_SILET:		.byte	$00
		.word	_NVAR-1
		.byte	$22,CHNG($2D),_VEXP,$F1,_OR,$86,$22,CHNG($2E),$00
		.word	_SSTR-1
		.byte	$E8,_RTN

		; FOR statement

_SFOR:		.byte	$01
		.word	_TNVAR-1
		.byte	$22,CHNG($2D),_VEXP,$19,_VEXP,$C3,$DC,_RTN
		.byte	$1A,_VEXP,_OR,_RTN

		; LOCATE statement

_SLOCATE:	.byte	_VEXP,CCOM
		.byte	_VEXP,CCOM
		.byte	$C4,_RTN

		; GET statement

_SGET:		.byte	$DD,CCOM

		; NEXT statement

_SNEXT:		.byte	$01
		.word	_TNVAR-1
		.byte	$CB,_RTN

		; RESTORE statement

_SREST:		.byte	_VEXP,$C8,_OR,$C6,_RTN

		; INPUT statement

_SINPUT:	.if	BASIC_REVISION = 1
		.byte	$F8
		.else
		.byte	$F7
		.endif

		; READ statement

_SREAD:		.byte	$DB,$C2,_RTN
		.byte	$14,_OR,$16
		.byte	_RTN

		; PRINT, ? statement

_SPRINT:	.byte	$C9,$BB,_OR

		.if	BASIC_REVISION = 1
		.byte	$ED
		.else
		.byte	$EC
		.endif

		; LPRINT statement

_SLPRINT:	.byte	$00
		.word	A79B-1

		.byte	$B5,_RTN,$1C,_VEXP,_RTN,$01
		.word	_TNVAR-1
		.byte	_OR,$01

		.word	_TSVAR-1

		.byte	_RTN,$B8
		.if	BASIC_REVISION = 1
		.byte	$C3,_OR,_RTN,CCOM,$BB,_OR
		.else
		.byte	$C2,_RTN,CCOM,$BC,_OR
		.endif
		.byte	_RTN

		; XIO statement

_SXIO:		.byte	_VEXP,CCOM

		; OPEN statement

_SOPEN:		.if	BASIC_REVISION = 1
		.byte	$AB
		.else
		.byte	$AC
		.endif
		.byte	CCOM,$F9,CCOM
		.if	BASIC_REVISION = 1
		.byte	$F3,$99
		.else
		.byte	$F3,$9A
		.endif
		.byte	_RTN

		; CLOSE statement

_SCLOSE:	.if	BASIC_REVISION = 1
		.byte	$A4,$96
		.else
		.byte	$A5,$97
		.endif
		.byte	_RTN

		; ENTER, LOAD, SAVE statement

_SENTER:
_SLOAD:
_SSAVE:		.if	BASIC_REVISION = 1
		.byte	$ED,$93,_RTN
		.else
		.byte	$ED,$94,_RTN
		.endif

		; RUN statement

_SRUN:		.byte	$EA
		.if	BASIC_REVISION = 1
		.byte	$90,_OR,$8E,_RTN,$99,CCOM,_OR,$96
		.else
		.byte	$91,_OR,$8F,_RTN,$9A,CCOM,_OR,$97
		.endif
		.byte	$15,_OR,_RTN

		; LIST statement

_SLIST:		.byte	$DE
		.if	BASIC_REVISION = 1
		.byte	$84
		.else
		.byte	$85
		.endif
		.byte	_OR,$DB,CCOM,$C4,_OR,$C2,_RTN
		.byte	$00
		.word	A7BB-1
		.byte	$F4,_RTN

		; STATUS statement

_SSTATUS:	.if	BASIC_REVISION = 1
		.byte	$C3,$F1,_RTN,$81,CCOM,$00
		.else
		.byte	$C3,$F1,_RTN,$82,CCOM,$00
		.endif
		.word	_NVAR-1
		.byte	_RTN

		; NOTE, POINT statement

_SNOTE:
_SPOINT:	.byte	$BA,CCOM,$00
		.word	_NVAR-1
		.byte	$E4,_RTN

		.byte	$00
		.word	_SSTR-1
		.byte	_RTN

		.byte	_VEXP,CCOM
		.byte	_VEXP,_RTN

		; SOUND statement

_SOUND:		.byte	_VEXP,CCOM

		; SETCOLOR statement

_SSETCOLOR:	.byte	_VEXP,CCOM

		; POKE, PLOT, POSITION, DRAWTO statement

_SPOKE:
_SPLOT:
_SPOS:
_SDRAWTO:	.byte	$B8,$D5,_RTN

		; COM, DIM statement

_SCOM:
_SDIM:		.if	BASIC_REVISION = 1
		.byte	$EC,$D2
		.else
		.byte	$ED,$D2
		.endif
		.byte	_RTN

		; ON statement

_SON:		.byte	_VEXP,$C4,$C7,$CD,_RTN,$17,_OR,$18
		.byte	_RTN,_VEXP,$C2,_RTN,CCOM,$BC,_OR,_RTN
		.byte	$14,_OR,$16,_RTN,$01
		.word	_TNVAR-1

		.if	BASIC_REVISION > 1
		.byte	$0D
		.endif

		.byte	$2B,CHNG($39),_VEXP,$00
		.word	_NMAT2-1
		.byte	CRPRN,_OR,$01

		.word	_TSVAR-1

		.byte	$2B,CHNG($3B),_VEXP,CRPRN,_RTN
		.if	BASIC_REVISION = 1
		.byte	$AB
		.else
		.byte	$AA
		.endif
		.byte	$C3,_OR,_RTN,CCOM,$BB,_OR,_RTN

		; IF statement

_SIF:		.byte	_VEXP,CTHEN,JS(_IFA)
		.if	BASIC_REVISION = 1
		.byte	$9C
		.else
		.byte	$9B
		.endif
		.byte	_RTN

_IFA:		.byte	ESRT(_TNCON)
		.byte	_OR,$01
		.word	_EIF-1

A79B:		.byte	$C9,_OR,$D4,$C3,_OR,_RTN,$C3,_OR,_RTN,$C3,$C8,_RTN,_VEXP,_OR,$00
		.word	_SSTR-1
		.byte	_RTN,$C4,$B3,_OR,_RTN,$C6,$C2,_RTN,$BD,_OR,_RTN,CCOM,_OR,$15,_RTN

A7BB:		.byte	_VEXP,$C3,_OR,_RTN,CCOM,_VEXP,_OR,_RTN

		; REM statement

_SREM:		.byte	$01
		.word	_EREM-1

		; DATA statement

_SDATA:		.byte	$01
		.word	_EDATA-1
A7CE:		.byte	$40,_OR,$41,_OR,$43,_OR,$42,_RTN
_SFNP:		.byte	$3D,_OR,$3E,_RTN
A7D5:		.byte	_VEXP,$C2,_RTN
		.byte	CCOM,CHNG(CACOM),$BA,_OR,_RTN

OPNTAB:		.byte	$02+$80
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

EXPLOW:		lda	#0
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
		cpx	#MEOLFLG
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

CONTLOW:	.if	BASIC_REVISION > 1
		tay
		.endif

CONTLOW1:	lda	#0
CONTRACT:	sty	$A4
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
		cpx	#MEOLFLG
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
EXECNL:		jsr	B819
A961:		jsr	TSTBRK
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

EXECNL:		jsr	B819
A961:		jsr	TSTBRK
		beq	A99C
		.endif
		ldy	STMLBD
		cpy	MAXCIX
		bcs	A989
		lda	(STMCUR),Y
		sta	STMLBD
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
		lda	MAXCIX
		jsr	GNXTL
		jsr	A9E1
		bpl	EXECNL
		jmp	B78C
A99C:		jmp	B792
A99F:		jmp	SNX3

GETSTMT:	lda	STMCUR
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

A9C7:		jsr	GETLL
		jsr	GNXTL
		jmp	A9B2

GNXTL:		clc			; STMCUR += accu
		adc	STMCUR
		sta	STMCUR

		.if	BASIC_REVISION = 1
		tay
		.endif

		lda	STMCUR+1
		adc	#0
		sta	STMCUR+1
		rts

GETLL:		ldy	#2
		lda	(STMCUR),Y
		rts

A9E1:		ldy	#1
		lda	(STMCUR),Y

		.if	BASIC_REVISION = 1
		rts
		.endif

A9E5:		rts			; execute REM, DATA

A9E6:		jsr	CLSALL		; execute BYE
		jmp	SELFSV

A9EC:		jsr	CLSALL		; execute DOS
		jmp	(DOSVEC)

TSTBRK:		.if	BASIC_REVISION = 1
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
		.dbyt	B206-1,BC22-1,B766-1,B28D-1,B206-1,B78C-1,XNEW-1,BBF2-1
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
		jmp	CVFPI
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
CVFPI:		jsr	FPI
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
		sta	STENUM
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
		lda	STENUM
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
		jsr	RSTSEOL
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
		jsr	CVFPI
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
		.if	BASIC_REVISION > 3
		jmp	AD26
		nop
		.else
		JSR	AD26		; ADD IT
		RTS
		.endif
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
		jsr	GETSTMT
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
		sta	DIRFLG
		inc	CIX

		.if	BASIC_REVISION = 1
		jmp	B35F
		.else
		bcs	B35F
		.endif

B32D:		inc	CIX
B32F:		ldy	CIX
		lda	(INBUFF),Y
		cmp	#CRPRN
		clc
		beq	B33A
		cmp	#CR
B33A:		rts
B33B:		jsr	B928

B33E:		lda	#$3F		; execute INPUT
		sta	$C2
		jsr	AB36
		dec	$A8
		bcc	B34E
		jsr	BD07
		sta	$B4
B34E:		jsr	INTLBF

		.if	BASIC_REVISION = 1
		jsr	BA89
		jsr	B378
		.else
		jsr	BDE4
		jsr	TSTBRK
		beq	B378
		.endif

		ldy	#0
		sty	DIRFLG
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
		jsr	TSTBRK
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
		bit	DIRFLG
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
B3AD:		bit	DIRFLG
		bvc	B3C0
		inc	DATAD
		jsr	B904
		bcs	B3C5
		jsr	B32F
		bcc	B3D5
		jmp	B2FB
B3C0:		jsr	B904
		bcc	B3CD
B3C5:		jsr	INTLBF
		lda	#0
		sta	$B4
		rts
B3CD:		jsr	B32F
		bcc	B3D5
		jmp	B34E
B3D5:		inc	CIX
		jmp	B35F

B3DA:		lda	PTABW		; execute PRINT, ?
		sta	STENUM
		lda	#0
		sta	COX
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
B449:		ldy	COX
		iny
		cpy	STENUM
		bcc	B459
		clc
		lda	PTABW
		adc	STENUM
		sta	STENUM
		bcc	B449
B459:		ldy	COX
		cpy	STENUM
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

B485:		lda	#CR
		jsr	B491
B48A:		lda	#0
		sta	$B5
		rts
B48F:		and	#$7F
B491:		inc	COX
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
		lda	#CR
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
@4:		jsr	GETSTMT
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
@6:		jsr	LLINE
		jsr	TSTBRK

		.if	BASIC_REVISION = 1
		bne	@7
		.else
		beq	@7
		.endif

		jsr	GETLL
		jsr	GNXTL
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
		dec	STENUM
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
		sty	STENUM
@1:		inc	STENUM
		ldy	STENUM
		lda	(POKADR),Y
		pha
		cmp	#CR
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

LLINE:		ldy	#0
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
LDLINE:		ldy	#2
		lda	(STMCUR),Y
		sta	MAXCIX
		iny
@loop_y:	lda	(STMCUR),Y
		sta	$A7
		iny
		sty	$A8
		jsr	B5C2
		ldy	$A7
		cpy	MAXCIX
		bcc	@loop_y
		rts

B5C2:		jsr	B663
		cmp	#$36
		beq	B5E0
		jsr	B66F
		jsr	B663
		cmp	#CERR
		beq	@1
		cmp	#2
		bcs	B5E0
@1:		jsr	B661
		jsr	BA99
		jmp	@1

B5E0:		jsr	B661
		bpl	@1
		and	#$7F
		sta	STENUM
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
		sta	STENUM
		lda	#$22
		jsr	BA99
		lda	STENUM
		beq	@5
@4:		jsr	B661
		jsr	BA99
		dec	STENUM
		bne	@4
@5:		lda	#$22
		jsr	BA99
		jmp	B5E0
@6:		sec
		sbc	#$10
		sta	STENUM
		ldx	#0
		lda	#>OPNTAB
		ldy	#<OPNTAB
		jsr	B53E
		jsr	B663
		cmp	#$3D
		bcs	@2
		ldy	#0
		lda	(POKADR),Y
		and	#$7F
		jsr	TSALPH
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

B66F:		sta	STENUM
		ldx	#2
		lda	#>SNTAB
		ldy	#<SNTAB
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
		ldx	SVONTX
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
B6E0:		jsr	GETSTMT
		bcs	B6EA
		pla
		pla
		jmp	EXECNL

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
		jsr	RUNINIT

B766:		jsr	B8B9		; execute CLR
		jsr	B8A8
		lda	#0
		sta	DATALN
		sta	DATALN+1
		sta	DATAD
		rts

B775:		jmp	SNX1

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

B787:		lda	MAXCIX
		sta	$A7
B78B:		rts

B78C:		jsr	B7A6		; execute END
		jmp	SNX1

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
B7B2:		jmp	SETDZ

		.if	BASIC_REVISION = 1
STOPPED_STR:	BasicString "STOPPED "
		.endif

B7B5:		jsr	A9E1		; execute CONT
		bpl	B7B2
		lda	STOPLN
		sta	$A0
		lda	STOPLN+1
		sta	$A1
		jsr	GETSTMT
		jsr	A9E1
		bmi	B775
		jsr	GETLL
		jsr	GNXTL
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
		sta	SVONTX
B7FF:		jsr	ABCD
		dec	SVONTX
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

B816:		jsr	GETSTMT
B819:		ldy	#2
		lda	(STMCUR),Y
		sta	MAXCIX
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
		jsr	CONTLOW
		.endif

		ldy	#3
		lda	(MEMTOP),Y
		sta	SVONTL
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
		jsr	CONTLOW
		.endif

		pla
B86F:		clc
		rts

B872:		.if	BASIC_REVISION = 1
		tay
		ldx	#$90
		jmp	CONTLOW
		.endif

B871:		jsr	B87A
		tay
		ldx	#$90
		jmp	EXPLOW
B87A:		ldx	MEMTOP
		stx	$C4
		ldx	MEMTOP+1
		stx	$C5
		rts
B883:		ldy	$A8
		sty	SVONTX
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

RUNINIT:	.if	BASIC_REVISION = 1

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

		jmp	CLSALL

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
ERLTL:		inc	$B9
B91A:		inc	$B9
B91C:		inc	$B9
B91E:		inc	$B9
B920:		inc	$B9
B922:		inc	$B9
B924:		inc	$B9
B926:		inc	$B9
B928:		inc	$B9
B92A:		inc	$B9
ERRVSF:		inc	$B9
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
		lda	#CERR
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
		jsr	SETDZ
		.endif
		jmp	SYNTAX

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
		bne	GLGO
		lda	#CR
		jsr	BA99
BA89:		ldx	$B4
		bne	GLGO
		lda	$C2
		jsr	BA99
GLGO:		ldx	$B4
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
		ldy	#MEOLFLG
		rts

BABE:		sta	$C0
BAC0:		stx	$C1
		jmp	BCAF

BAC5:		lda	#4		; execute ENTER
		jsr	BAD7
		sta	$B4
		jmp	SYNTAX

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
		sta	LOADFLG
		jsr	BCAF
		ldy	#14
		jsr	BD15
		jsr	BCBB
		lda	LBUFF
		ora	LBUFF+1
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
		cpx	#VNTP
		bcs	@3
		jsr	W9
		jsr	B766
		lda	#0
		sta	LOADFLG
		pla
		beq	@6
		rts
@6:		jmp	SNX1

@7:		lda	#0
		sta	LOADFLG
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
		jmp	INTLBF

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
		jsr	RSTSEOL
		jsr	INTLBF
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
		jsr	INTLBF
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
		lda	LOADFLG
		.if	BASIC_REVISION = 1
		.byte	$f0,$03
		.else
		beq	BD06
		.endif
		jmp	COLDSTART

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
BCE1:		jsr	SETDZ
		jmp	B934
BCE7:		cpy	#7
		bne	BCD8
		ldx	#$5D
		cpx	$C2
		bne	BCD8
		jsr	BCF7
		jmp	SNX2
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

CLSALL:		lda	#0
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
SETDZ:		lda	#0
		sta	$B4
		sta	$B5
		rts
		.endif

PREADY:		ldx	#6
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
SETDZ:		lda	#0
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
		lda	#CR
		sta	(INBUFF),Y
		sta	MEOLFLG
		rts
RSTSEOL:	ldy	$98
		lda	$97
		sta	(INBUFF),Y
		lda	#0
		sta	MEOLFLG
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
		ldy	SVONTL
		dey
		lda	(STMCUR),Y
		sta	$A7
		iny
		lda	(STMCUR),Y
BDDA:		rts

		ldx	$B4
		bne	GLGO2
		lda	#CR
		jsr	BA99
BDE4:		ldx	$B4
		bne	GLGO2
		lda	$C2
		jsr	BA99
GLGO2:		ldx	$B4
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
		BEQ	@SQROUT
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
@SQROUT:	rts

		.if	BASIC_REVISION = 1
FP_two:		.byte	$40,$02,$00,$00,$00,$00
		.byte	$30,$30,$20,$20,$60,$70,$70
		.byte	$00,$00,$00,$00,$30,$30,$3C,$7C,$7C,$7C,$64,$64,$67,$E7,$C0,$C0
		.byte	$80,$80,$E0,$E0,$00,$00,$00,$00,$30,$30,$38,$38,$38,$38,$FC,$EC
		.byte	$EC,$0C,$04,$06,$06,$02,$03,$03,$00,$00,$00,$00,$30,$30,$30,$B0
		.byte	$B0,$D8,$F8,$78,$38,$18,$18,$18,$10,$10,$1C,$1C,$00,$00,$00,$00
		.byte	$30,$30,$30,$30,$30,$30,$30,$30,$20,$E0,$E0,$80,$80,$00,$00,$00
		.endif

		.if	BASIC_REVISION = 1
		EntryPoint(BFF0)
		.byte	$00,$00,$00,$00,$CC,$B8,$A4,$90,$A0
ENTRY1:		rts
		.endif

		.if	BASIC_REVISION = 2
		.byte	$00,$00,$2C,$37,"inner"	; prehistoric debris!
		.endif

		.res	BFFA-*

		EntryPoint(BFFA)
		.word	COLDSTART
		EntryPoint(BFFC)
		.word	$0500
		EntryPoint(BFFE)
		.if	BASIC_REVISION = 1
		.word	ENTRY1
		.else
		.word	BFF0
		.endif

		.end
