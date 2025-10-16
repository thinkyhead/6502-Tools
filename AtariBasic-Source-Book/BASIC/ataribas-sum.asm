		; **********************************
		; **                              **
		; **  ATARI BASIC ROM: A000-BFFF  **
		; **                              **
		; **********************************

		.org $A000
		.segment "A000BFFF"

		.include "../include/atari.inc"
		.include "../include/pokey.inc"

		; Atari BASIC Source Book comments and equates
		; added 2025-10-10 by @thinkyhead
		.include "sourcebook.inc"

MAXEND		EQU	$C000-$10 ; So max size is $1FF0

		; BasicString str - Output len-1 regular bytes and last byte OR'ed with 0x80
		.macro	BasicString stringval
		.repeat	.strlen (stringval)-1,i
		.byte	.strat (stringval,i)
		.endrep
		.byte	.strat (stringval,.strlen(stringval)-1) | $80
		.endmacro

		; Token Counter
		.define	NEXTTOK NTOK .set NTOK+1
		.define	SetToken(name) name = NTOK
		.macro NextToken name
		SetToken name
		NEXTTOK
		.endmacro

		; WordBasicString adr,str - Output handler address and string
		.macro WordBasicString wordval,stringval,tokname
		.word wordval-1
		BasicString stringval
		.if .paramcount = 3
		NextToken tokname
		.endif
		.endmacro

		; CommandPrec prec[,name] - Emit Precedence, Define Command Name
		;			    for Command Precedence and Syntax Tables
		; NOTE: Command Precedence and Operator Names must align!
		.macro CommandPrec precval,tokname
		.if .paramcount = 2
		NextToken tokname
		.endif
		.byte precval
		.endmacro

		; Load X/Y with Address LO/HI
		.macro LDXY adr,usefn
		LDX	#<adr
		LDY	#>adr
		.if .paramcount = 2
		JSR	usefn
		.endif
		.endmacro

;;
;; Pseudo-Code Annotations
;;
;; A Python-like and C-like mixture of pseudo-code is used to describe each part of the program
;; at about the level of a C program, without getting too high level, so we first capture the program
;; structure. Annotations begin with ';;' so they can be easily extracted for processing by an LLM.
;;
;; Some of the idioms and standards and common practices and eventually I'll get the right word...
;;
;; 6502 vars used as addresses act like an array index, whether zero page or 16-bit Absolute, so here
;; Outer Square Brackets indicate the byte index into the whole address space from 0x0000 - 0xFFFF.
;; Unless labeled HI/LO all two byte values are working as a single unit.
;;
;; Examples:
;;  - [VARNAME] : Get or set the value at memory address VARNAME.
;;  - [VARNAME+X] : Get or set the value at memory address VARNAME+X.
;;  - [[VARNAME]+Y] : Take the address at VARNAME and add Y; get or set the value at the resulting address.
;;  - [[VNTD]] : Get or set the value pointed at by VNTD, VNDT+1.
;;
;; Work for the helpful code assistant:
;;  - Declare variables with appropriate types for our named variables.
;;  - There is no reason to maintain 6502 variables that overlap in C or Python. Overlap is not used.
;;

;---------------------------------------------------------------
; COLDSTART - Cartridge Entrypoint
;---------------------------------------------------------------

COLDSTART:

;; IF ![LOADFLG] AND [WARMST]: GOTO WARMSTART

		LDA	LOADFLG			; Y IN MIDDLE OF LOAD
		BNE	@COLD1			; DO COLDSTART
		LDA	WARMST			; IF WARM START
		BNE	WARMSTART		; THEN BRANCH

;; STACK_RESET()

@COLD1:		LDX	#$FF			; SET ENTRY STACK
		TXS				; TO TOS
		CLD				; CLEAR DECIMAL MODE

;---------------------------------------------------------------
; XNEW - Execute NEW
;---------------------------------------------------------------

;; [LOMEM] = [LMADR] (XY)
;; [MEOLFLG] = 0 (A)
;; [LOADFLG] = 0

XNEW:		LDX	LMADR			; LOAD LOW
		LDY	LMADR+1			; MEM VALUE
		STX	LOMEM			; SET LOMEM
		STY	LOMEM+1
		LDA	#0			; RESET MODIFIED
		STA	MEOLFLG			; EOL FLAG
		STA	LOADFLG			; RESET LOAD FLAG

;; [VNTP] = [VNTD] = [VVTP] = [STMTAB] = [STMCUR] =
;;        [STARP] = [RUNSTK] = [LMADR] = [MEMTOP] = (LMADR + 256) (AY)

		INY				; ALLOW 256 FOR OUTBUFF
		TXA				; VNTP

		LDX	#VNTP			; GET ZPG DISPC TO VNTP
@CS1:		STA	0,X			; SET TABLE ADR LOW
		INX
		STY	0,X			; SET TABLE ADR HIGH
		INX
		CPX	#MEMTOP+2		; AT LIMIT
		BCC	@CS1			; BR IF NOT

;; EXPAND_U8(VVTP, 1)

		LDX	#VVTP			; EXPAND VVT BY ONE
		LDY	#1			; FOR END OF VNT
		JSR	EXPLOW			; ZERO BYTE

;; EXPAND_U8(STARP, 3)

		LDX	#STARP			; EXPAND STMT TBL
		LDY	#3			; BY 3 BYTES
		JSR	EXPLOW			; GO DO IT

		LDA	#0			; SET 0

;; [[VNTD]] = 0
		TAY
		STA	(VNTD),Y		; INTO VNTD

;; [[STMCUR]] = 0

		STA	(STMCUR),Y		; INTO STMCUR+0

;; [[STMCUR]+1] = $80
		INY
		LDA	#$80			; $80 INTO
		STA	(STMCUR),Y		; STMCUR+1

;; [[STMCUR]+2] = $03
		INY
		LDA	#$03			; $03 INTO
		STA	(STMCUR),Y		; STMCUR+2

;; PTABW = 10
		LDA	#10			; SET PRINT TAB
		STA	PTABW			; WIDTH TO 10

;---------------------------------------------------------------
; Warm Start
;---------------------------------------------------------------

;---------------------------------------------------------------
;	WARMSTART - BASIC RESTART
;	            DOES NOT DESTROY CURRENT PGM
;---------------------------------------------------------------

;; RUNINIT() ; CLSALL() ; SETDZ()

WARMSTART:	JSR	RUNINIT			; INIT FOR RUN
SNX1:		JSR	CLSALL			; GO CLOSE DEVICE 1-7
SNX2:		JSR	SETDZ			; SET E/L DEVICE 0

;; IF [MEOLFLG]: RSTSEOL()

		LDA	MEOLFLG			; IF AN EOL INSERTED
		BEQ	SNX3
		JSR	RSTSEOL			; THEN UN-INSERT IT

;; PRINT_READY()

SNX3:		JSR	PREADY			; PRINT READY MESSAGE

;---------------------------------------------------------------
; Syntax
;---------------------------------------------------------------

;---------------------------------------------------------------
; Editor - Get Lines of Input
;---------------------------------------------------------------

;; IF [LOADFLG]: GOTO COLDSTART
;; or, SET_STATE(COLD) ; BREAK

SYNTAX:		LDA	LOADFLG			; IF LOAD IN PROGRESS
		BNE	COLDSTART		; GO DO COLDSTART

;; EMPTY_CPU_STACK()

		LDX	#$FF			; RESTORE STACK
		TXS

;; INIT_LINE_BUFFER()  # Initialize Line Buffer

		JSR	INTLBF			; GO INIT LBUFF

;; PROMPT = EPCHAR ; ']'

		LDA	#EPCHAR			; ]
		STA	PROMPT

;; GET_LINE_GLGO()
;; IF TEST_BREAK(): GOTO SYNTAX

		.if	BASIC_REVISION = 1
		JSR	GLGO
		JSR	TSTBRK			; TEST BREAK
		BNE	SYNTAX			; BR IF BREAK
		.else
		jsr	GLGO2
		jsr	TSTBRK
		beq	SYNTAX
		.endif

;; [CIX] = [MAXCIX] = [COX] = [DIRFLG] = [SVONTX] = [SVONTC] = [SVVVTE] = 0

		LDA	#0			; INIT CURRENT
		STA	CIX			; INPUT INDEX TO ZERO
		STA	MAXCIX
		STA	COX			; OUTPUT INDEX TO ZERO
		STA	DIRFLG			; SET DIRECT STMT
		STA	SVONTX			; SET SAVE ONT CIX
		STA	SVONTC
		STA	SVVVTE			; VALUE IN CASE

;; [SVVNTP] = [VNTD]

		LDA	VNTD			; OF SYNTAX ERROR
		STA	SVVNTP
		LDA	VNTD+1
		STA	SVVNTP+1

;; SKIP_BLANKS() ; GET_LINE_NUMBER() ; _SETCODE(0)

		JSR	SKBLANK			; SKIP BLANKS
		JSR	GETLNUM			; CONVERT AND PUT IN BUFFER
		JSR	_SETCODE		; SET DUMMY FOR LINE LENGTH

;; IF [BININT+1] & 0x80: DIRFLG = [BININT+1]

		LDA	BININT+1
		BPL	@SYN0
		STA	DIRFLG

;; SKIP_BLANKS()

@SYN0:		JSR	SKBLANKS		; SKIP BLANKS

;; [STMSTRT] = [CIX] (Y)

		LDY	CIX			; GET INDEX
		STY	STMSTRT			; SAVE INCASE OF SYNTAX ERROR

;; IF [[INBUFF]+Y] != CR: GOTO SYN1

		LDA	(INBUFF),Y		; GET NEXT CHAR
		CMP	#CR			; IS IT CR
		BNE	SYN1			; BR NOT CR

;; IF DIRECT_STMT_FLAG & 0x80 == 0: GOTO SDEL  # Go delete line
;; ELSE GOTO SYNTAX  # Go get new line

		BIT	DIRFLG			; IF NO LINE NUM
		BMI	SYNTAX			; THEN NO DELETE
		JMP	SDEL			; GO DELETE STMT

_XIF:
SYN1:

;; [STMLBD] = [COX]

		LDA	COX			; SAVE COX
		STA	STMLBD			; AS PM TO STMT LENGTH BYTE

;; _SETCODE([COX])

		JSR	_SETCODE		; DUMMY FOR STMT LENGTH

;; SKIP_BLANKS()

		JSR	SKBLANK			; GO SKIP BLANKS

;; [CIX] = SEARCH(SNTAB,2)

		LDA	#>SNTAB			; SET UP FOR STMT
		LDY	#<SNTAB			; NAME SEARCH
		LDX	#2
		JSR	SEARCH			; AND DO IT
		STX	CIX

;; _SETCODE([STENUM]) ; SKIP_BLANKS()

		LDA	STENUM			; GET STMT NUMBER
		JSR	_SETCODE		; GO SET CODE
		JSR	SKBLANK

;; IF SYNENT() == OK: GOTO SYNOK

		JSR	SYNENT			; AND GO SYNTAX HIM
		BCC	SYNOK			; BR IF OK SYNTAX
						; ELSE SYNTAX ERROR

;; A = [[INBUFF]+[MAXCIX]]
;; IF A == CR: { [[INBUFF]+[MAXCIX]+1] = CR ; A = ' ' }

		LDY	MAXCIX			; GET MAXCIX
		LDA	(INBUFF),Y		; LOAD MAXCIX CHAR
		CMP	#CR			; WAS IT CR
		BNE	@SYN3A			; BR IF NOT CR
		INY				; MOVE CR RIGHT ONE
		STA	(INBUFF),Y
		DEY				; THEN PUT A
		LDA	#' '			; BLANK IN ITS PLACE

;; [[INBUFF]+[MAXCIX]] = A | 0x80

@SYN3A:		ORA	#$80			; SET MAXCIX CHAR
		STA	(INBUFF),Y		; TO FLASH

;; DIRECT_STMT_FLAG |= 0x40

		LDA	#$40			; INDICATE SYNTAX ERROR
		ORA	DIRFLG
		STA	DIRFLG			; IN DIRFLG

;; [CIX] = [STMSTRT]

		LDY	STMSTRT			; RESTORE STMT START
		STY	CIX

;; [STMLBD] = 3

		LDX	#3			; SET FOR FIRST STMT
		STX	STMLBD

;; [COX] = 4

		INX				; INC TO CODE
		STX	COX			; AND SET COX

;; _SETCODE(kERROR)

		LDA	#kERROR			; GARBAGE CODE
SYN3:		JSR	_SETCODE		; GO SET CODE

;; IF [[INBUFF]+[CIX]++] != CR: GOTO SYN3

_XDATA:		LDY	CIX			; GET INDEX
		LDA	(INBUFF),Y		; GET INDEX CHAR
		INC	CIX			; INC TO NXT
		CMP	#CR			; IS IT CR
		BNE	SYN3			; BR IF NOT

;; _SETCODE(CR)

		JSR	_SETCODE

;; [[OUTBUFF]+[STMLBD]] = [COX]

SYNOK:		LDA	COX			; GET DISPL TO END OF STMT
		LDY	STMLBD
		STA	(OUTBUFF),Y		; SET LENGTH BYTE

;; IF [[INBUFF]+[CIX]-1] != CR: GOTO SYN1

		LDY	CIX			; GET INPUT DISPL
		DEY
		LDA	(INBUFF),Y		; GET LAST CHAR
		CMP	#CR			; IS IT CR
		BNE	SYN1			; BR IF NOT

;; [[OUTBUFF]+2] = [COX]

		LDY	#2			; SET LINE LENGTH
		LDA	COX			; INTO STMT
		STA	(OUTBUFF),Y

;; CF = GET_STATEMENT()

		JSR	GETSTMT			; GO GET STMT

;; A = 0 ; IF CF == 0: A = GET_LINE_LENGTH()

		LDA	#0
		BCS	@SYN6
		JSR	GETLL			; GO GET LINE LENGTH

;; A -= [COX]
;; IF A == 0: GOTO @SYNIN ELIF A < 0: GOTO @SYNCON

@SYN6:		SEC
		SBC	COX			; ACU=LENGTH[OLD-NEW]
		BEQ	@SYNIN			; BR NEW=OLD
		BCS	@SYNCON			; BR OLD>NEW
						; OLD<NEW

;; EXPAND_U8([STMCUR], ABS(A))

		EOR	#$FF			; COMPLEMENT RESULT
		TAY
		INY
		LDX	#STMCUR			; POINT TO STMT CURRENT
		JSR	EXPLOW			; GO EXPAND

;; [STMCUR] = [SVESA]
;; IF [STMCUR] != 0: GOTO @SYNIN

		LDA	SVESA			; RESET STMCUR
		STA	STMCUR
		LDA	SVESA+1
		STA	STMCUR+1
		BNE	@SYNIN
@SYNCON:

		.if	BASIC_REVISION = 1

;; OLDA = A ; GET_NEXT_LINE() ; LL = OLDA

		PHA				; CONTRACT LENGTH
		JSR	GNXTL
		PLA
		TAY
		.else
		tay
		jsr	GNXTL
		.endif

;; CONTRACT_U8(STMCUR, LL)

		LDX	#STMCUR			; POINT TO STMT CURRENT
		JSR	CONTLOW1		; GO CONTRACT

;; FOR (Y = [COX]-1 ; Y >= 0 ; --Y):
;;   [[STMCUR]+Y] = [[OUTBUFF]+Y]

@SYNIN:		LDY	COX			; STMT LENGTH
@SYN7:		DEY				; MINUS ONE
		LDA	(OUTBUFF),Y		; GET BUFF CHAR
		STA	(STMCUR),Y		; PUT INTO STMT TBL
		TYA				; TEST END
		BNE	@SYN7			; BR IF NOT

;; IF [DIRECT_STMT_FLAG] & 0x40 == 0: GOTO _SYN8  # at _SYN8 check 0x80

		BIT	DIRFLG			; TEST FOR SYNTAX ERROR
		BVC	_SYN8			; BR IF NOT

;; A = [SVVVTE] * 8

		LDA	SVVVTE			; CONTRACT VVT
		ASL
		ASL
		ASL

;; CONTRACT_U8(ENDVVT, [SVVVTE] * 8)

		.if	BASIC_REVISION = 1
		TAY
		.endif
		LDX	#ENDVVT
		JSR	CONTLOW

;; YA = [VNTD] - [SVVNTP]

		SEC
		LDA	VNTD			; CONTRACT VNT
		SBC	SVVNTP
		TAY
		LDA	VNTD+1
		SBC	SVVNTP+1

;; CONTRACT(VNTD, YA)

		LDX	#VNTD
		JSR	CONTRACT

;; IF [DIRECT_STMT_FLAG] & 0x80:
;;   LDLINE()
;;   GOTO SYNTAX

		BIT	DIRFLG			; IF STMT NOT DIRECT
		BPL	@SYN9A			; THEN BRANCH
		JSR	LDLINE			; ELSE LIST DIRECT LINE
		JMP	SYNTAX			; THEN BACK TO SYNTAX

;; LIST_LINE()

@SYN9A:		JSR	LLINE			; LIST ENTIRE LINE

;; GOTO SYNTAX

_SYN9:		JMP	SYNTAX

;; IF DIRECT_STMT_FLAG & 0x80: GOTO EXECNL

_SYN8:		BPL	_SYN9
		JMP	EXECNL			; GO TO PROGRAM EXECUTOR

;; IF GET_STATEMENT() == NOTFOUND: GOTO SYNTAX

SDEL:		JSR	GETSTMT			; GO GET LINE
		BCS	_SYN9			; BR NOT FOUND

;; A = GET_LINE_LENGTH()

		JSR	GETLL			; GO GET LINE LENGTH

;; OLDA = A ; GET_NEXT_LINE() ; LL = OLDA

		.if	BASIC_REVISION = 1
		PHA				; Y
		JSR	GNXTL
		PLA
		TAY
		.else
		tay
		jsr	GNXTL
		.endif

;; CONTRACT_U8(STMCUR, LL)

		LDX	#STMCUR			; GET STMCUR DISPL
		JSR	CONTLOW1		; GO DELETE
		JMP	SYNTAX			; GO FOR NEXT LINE

;---------------------------------------------------------------
; Get a Line Number
;---------------------------------------------------------------
;	GETLNUM - GET A LINE NUM FROM ASCLT IN INBUFF
;		  TO BINARY INTO OUTBUFF
;---------------------------------------------------------------

GETLNUM:

;; AY = ASC_TO_FP()
;; IF AY == NAN:

		JSR	CVAFP			; GO CONVERT LINE #
		BCC	@GLNUM			; BR IF GOOD LINE #

@GLN1:

;;   [CIX] = 0 ; AY = 0x8000

		LDA	#0			; SET LINE #
		STA	CIX
		LDY	#$80			; =$8000
		BMI	@SLNUM

;; ELSE:
;;   AY = FP_TO_INT()

@GLNUM:
		JSR	CVFPI			; CONVERT FP TO INT
		LDY	BININT+1		; LOAD RESULT

;;   IF AY == NAN:
;;     [CIX] = 0 ; AY = 0x8000

		BMI	@GLN1			; BR IF LNO>32767

;; AY = [BININT]

		LDA	BININT

;; [TSLNUM] = AY

@SLNUM:		STY	TSLNUM+1		; SET LINE # HIGH
		STA	TSLNUM			; AND LOW

;; _SETCODE(A)

		JSR	_SETCODE		; OUTPUT LOW

;; [BININT.HI] = [TSLNUM.HI] (A)

		LDA	TSLNUM+1		; OUTPUT HI
		STA	BININT+1

;; _SETCODE(A)
;; RETURN
		JMP	_SETCODE		; AND RETURN

;---------------------------------------------------------------
; SYNENT - PERFORM LINE PRE-COMPILE
;---------------------------------------------------------------

;; [CPC] = [SPC] = [[SRCADR]]

SYNENT:		LDY	#1			; GET PC HIGH
		LDA	(SRCADR),Y
		STA	CPC+1			; SET PGM COUNTERS
		STA	SPC+1
		DEY
		LDA	(SRCADR),Y
		STA	CPC
		STA	SPC

;; [STKLVL] = 0

		.if	BASIC_REVISION = 1
		LDA	#0			; SET STKLVL
		STA	STKLVL			; SET STKLVL
		.else
		sty	STKLVL
		.endif

;; [SOX] = [COX] ; [SIX] = [CIX]

		LDA	COX			; MOVE
		STA	SOX			; COX TO SOX
		LDA	CIX			; MOVE
		STA	SIX			; CIX TO SIX

;---------------------------------------------------------------
; NEXT		GET NEXT SYNTAX CODE
;		AS LONG AS NOT FAILING
;---------------------------------------------------------------

;; A = NEXT_CODE()

NEXT:		JSR	NXSC			; GET NEXT CODE

;; IF A & 0x80: GOTO @ERNTV

		BMI	@ERNTV			; BR IF REL-NON-TERMINAL

;; IF A == 0: GOTO @GETADR  # ANTV(ADR-1)

		CMP	#_ESRT			; TEST CODE=1
		BCC	@GETADR			; BR CODE=0 [ABS-NON-TERMINAL]

;; IF A == 1:
;;   IF GETADR() == OK: GOTO NEXT
;;   ELSE: GOTO FAIL

		BNE	@TSTSUC			; BR CODE >1
		JSR	@GETADR			; CODE=1 [EXTERNAL SUBROUTINE]
		.if	BASIC_REVISION = 1
		BCC	NEXT			; BR IF SUB REPORTS SUCCESS
		JMP	FAIL			; ELSE GO TO FAIL CODE
		.else
		jmp	FAIL2
		.endif

;; IF A < 5: GOTO POP

@TSTSUC:	CMP	#_TERM			; TEST CODE = 5
		BCC	POP			; CODE = [2,3,or 4] POP UP TO
						; NEXT SYNTAX CODE

;; IF TERMTST(A) == OK: GOTO NEXT
;; ELSE GOTO FAIL

		JSR	TERMTST			; CODE=5 GO TEST TERMINAL
		.if	BASIC_REVISION = 1
		BCC	NEXT			; BR IF SUCCESS
		JMP	FAIL			; ELSE GO TO FAIL CODE
		.else
		jmp	FAIL2
		.endif

;; DIFF = A - 0xC1
;; X = DIFF >= 0 ? 0x000 : -0x100
;; PUSH_WORD([CPC] + DIFF + X)
;; GOTO _PUSH

@ERNTV:		SEC				; RELATIVE NON TERMINAL
		.if	BASIC_REVISION = 1
		LDX	#0			; TOKEN MINUS
		.endif
		SBC	#$C1
		BCS	@ERN1			; BR IF RESULT PLUS
		LDX	#$FF			; ADD A MINUS
@ERN1:		CLC
		ADC	CPC			; RESULT PLUS CPC
		PHA				; IS NEW CPC-1
		TXA
		ADC	CPC+1
		PHA				; SAVE NEW PC HIGH
		JMP	_PUSH

@GETADR:

;; PUSH(NEXT_CODE()) ; A = NEXT_CODE() ; PUSH(A)
;; IF OK: GOTO _PUSH

		JSR	NXSC			; GET NEXT CODE
		PHA				; SAVE ON STACK
		JSR	NXSC			; GET NEXT CODE
		PHA				; SAVE ON STACK
		BCC	_PUSH			; BR IF CODE =0

;; SWAP_TOP_TWO_STACK_BYTES()  # Change endian for adr

		PLA				; EXCHANGE TOP
		TAY				; 2 ENTRIES ON
		PLA				; CPU STACK
		TAX
		TYA
		PHA
		TXA
		PHA

;; RETURN

NRTS1:		RTS				; ELSE GOTO EXTERNAL SRT VIA RTS

;---------------------------------------------------------------
; PUSH		PUSH TO NEXT STACK LEVEL
;---------------------------------------------------------------

;; def PUSH():
;;   IF ([STKLVL] += 4) >= 256: GOTO ERROR_LINE_TOO_LONG

_PUSH:		LDX	STKLVL			; GET STACK LEVEL
		INX				; PLUS 4
		INX
		INX
		INX
		BEQ	@SSTB			; BR STACK TOO  BIG
		STX	STKLVL			; SAVE NEW STACK LEVEL

;;   [[STKLVL].SIX] = [CIX]
;;   [[STKLVL].SOX] = [COX]
;;   [[STKLVL].SPC] = [CPC]

		LDA	CIX			; CIX TO
		STA	SIX,X			; STACK IX
		LDA	COX			; COX TO
		STA	SOX,X			; STACK OX
		LDA	CPC			; CPC TO
		STA	SPC,X			; STACK CPC
		LDA	CPC+1
		STA	SPC+1,X

;; [CPC] = PULL16()

		PLA				; MOVE STACKED
		STA	CPC+1			; PC TO CPC
		PLA
		STA	CPC

;; GOTO NEXT

		JMP	NEXT			; GO FOR NEXT

;; GOTO ERROR_LINE_TOO_LONG

@SSTB:		JMP	ERLTL

;---------------------------------------------------------------
; POP		LOAD CPC FROM STACK PC
;		AND DECREMENT TO PREVIOUS STACK LEVEL
;---------------------------------------------------------------

;; IF [STKLVL] == 0: RETURN

POP:		LDX	STKLVL			; GET STACK LEVEL
		.if	BASIC_REVISION = 1
		BNE	@POP1			; BR NOT TOP OF STACK
		RTS				; TO SYNTAX CALLER
		.else
		beq	NRTS1
		.endif

;; [CPC] = [STKLVL+SPC]

@POP1:		LDA	SPC,X			; MOVE STACK PC
		STA	CPC			; TO CURRENT PC
		LDA	SPC+1,X
		STA	CPC+1

;; [STKLVL] -= 4

		DEX				; X=X-4
		DEX
		DEX
		DEX
		STX	STKLVL

;; IF STATUS = ERR: GOTO FAIL

FAIL2:		BCS	FAIL			; BR IF CALLER FAILING

;; GOTO NEXT

		JMP	NEXT			; ELSE GO TO NEXT

;---------------------------------------------------------------
; FAIL
;		TERMINAL FAILED
;		LOOK FOR ALTERNATIVE [OR] OR
;		A RETURN INDICATOR
;---------------------------------------------------------------

;; DO { A = NEXT_CODE() } WHILE (A & 0x80)

FAIL:		JSR	NXSC			; GET NEXT CODE

		BMI	FAIL			; BR IF RNTV

;; Pro Tip:  BCS=BGE  BCC=BLT

;; IF A <= _OR:
;;   CPC++ ; CPC++
;;   GOTO FAIL

		CMP	#_OR			; TEST CODE =2
		BCS	@TSTOR			; BR IF POSSIBLE OR
		JSR	_INCCPC			; CODE = 0 OR 1
		JSR	_INCCPC			; INC PC BY TWO
		BNE	FAIL			; AND CONTINUE FAIL PROCESS

;; IF A == _RTN: GOTO POP

@TSTOR:		CMP	#_RTN			; TEST CODE=3
		BEQ	POP			; BR CODE =3 [RETURN]

;; IF A < _RTN: GOTO FAIL

		BCS	FAIL			; CODE>3 [RNTV] CONTINUE

;; IF [CIX] > [MAXCIX]: [MAXCIX] = [CIX]

		LDA	CIX			; IF THIS CIX
		CMP	MAXCIX			; IS A NEW MAX
		BCC	@SCIX
		STA	MAXCIX			; THEN SET NEW MAX

;; [CIX] = [SIX+STKLVL] ; [COX] = [SOX+STKLVL]

@SCIX:		LDX	STKLVL			; CODE=2 [OR]
		LDA	SIX,X			; MOVE STACK INDEXES
		STA	CIX			; TO CURRENT INDEXES
		LDA	SOX,X
		STA	COX

;; GOTO NEXT
		JMP	NEXT			; TRY FOR SUCCESS HERE

;---------------------------------------------------------------
; Increment CPC
;---------------------------------------------------------------

;---------------------------------------------------------------
; INCCPC - INC CPC BY ONE
;---------------------------------------------------------------

;; def _INCCPC(): CPC++

_INCCPC:	INC	CPC
		BNE	@ICPCR
		INC	CPC+1
@ICPCR:		RTS


;---------------------------------------------------------------
; NXSC - GET NEXT SYNTAX CODE
;---------------------------------------------------------------

;; def NEXT_CODE(): CPC++ ; RETURN [[CPC]] (A)

NXSC:		JSR	_INCCPC			; INC PC
		LDX	#0
		LDA	(CPC,X)			; GET NEXT CODE
		RTS				; RETURN

;---------------------------------------------------------------
; TERMTST - TEST A TERMINAL CODE
;---------------------------------------------------------------

;; def TERMTST(A):

TERMTST:

;;   IF A == _CHNG: GOTO ECHNG  # Change last character

		CMP	#_CHNG			; TEST CODE=F
		BEQ	ECHNG			; BR CODE = F

;;   IF A > _CHNG: GOTO SRCONT  # Search Op Name Table
		BCS	SRCONT			; BR CODE > F

		.if	BASIC_REVISION > 1
		cmp	#_UNKN			; TEST CODE=D
		bne	@TT1
		jsr	_INCCPC			; NEXT CPC
		jmp	_SRCNT1
		.endif

;; Other BNF Commands

;; Replace RTN ADR with _EXP, Call _PUSH to advance [STKLVL] by 4 bytes, set [CPC] = _EXP, and jump to NEXT

@TT1:		PLA				; POP RTN ADR
		PLA
		LDA	#<(_EXP-1)		; PUSH EXP ADR
		PHA				; FOR SPECIAL
		LDA	#>(_EXP-1)		; EXP ANTV CALL
		PHA
		JMP	_PUSH			; GO PUSH

;---------------------------------------------------------------
; ECHNG - EXTERNAL CODE TO CHANGE COX -1
;---------------------------------------------------------------

;; CPC++ ; [OUTBUFF][COX-1] = *CPC

ECHNG:		JSR	_INCCPC			; INC PC TO CODE
		LDY	#0
		LDA	(CPC),Y			; GET CODE
		LDY	COX			; GET COX
		DEY				; MINUS 1
		STA	(OUTBUFF),Y		; SET NEW CODE

;; RETURN OK
		CLC				; SET SUCCESS
_ECRTS:		RTS				; RETURN

;; def _SETCODE(INCHAR):
;;   [OUTBUFF][COX++] = INCHAR
;;   IF COX >= 256: GOTO ERROR_LINE_TOO_LONG
;;   RETURN

_SETCODE:	LDY	COX			; GET COX
		STA	(OUTBUFF),Y		; SET CHAR
		INC	COX			; INC COX

		.if	BASIC_REVISION = 1
		BEQ	@SCOVF			; BR IF NOT ZERO
		RTS				; DONE
		.else
		bne	_ECRTS
		.endif

@SCOVF:		JMP	ERLTL			; GO TO LINE TOO LONG ERR

;---------------------------------------------------------------
; Exits for IF and REM
;---------------------------------------------------------------

;; STACK_RESET() ; [OUTBUFF][STMLBD] = COX ; GOTO _XIF

_EIF:		LDX	#$FF			; RESET STACK
		TXS
		LDA	COX			; SET STMT LENGTH
		LDY	STMLBD
		STA	(OUTBUFF),Y
		JMP	_XIF			; GO CONTINUE IF

;; STACK_RESET() ; GOTO _XDATA

_EREM:
_EDATA:		LDX	#$FF			; RESET STACK
		TXS
		JMP	_XDATA			; GO CONTINUE DATA

;---------------------------------------------------------------
; SRCONT - SEARCH OP NAME TABLE AND TEST RESULT
;---------------------------------------------------------------

;; def SEARCH_ONT():

SRCONT:

;; SKIP_BLANKS()

		JSR	SKBLANK			; SKIP BLANKS

;; IF CIX == SVONTX: GOTO @SONT1

_SRCNT1:	LDA	CIX			; GET CURRENT INPUT INDEX
		CMP	SVONTX			; COMPARE WITH SAVED IX
		BEQ	@SONT1			; BR IF SAVED IX SAME

;; SVONTX = CIX

		STA	SVONTX			; SAVE NEW IX

;; X = SEARCH(OPNTAB, 0)  # from INBUFF[CIX]
;; IF CS == NOTFOUND: GOTO @SONF
;; SVONTL = X  # ==CIX+1

		LDA	#>OPNTAB		; SET UP FOR ONT
		LDY	#<OPNTAB		; SEARCH
		LDX	#0
		JSR	SEARCH			; GO SEARCH
		BCS	@SONF			; BR NOT FOUND
		STX	SVONTL			; SAVE NEW CIX

;; SVONTC = STENUM + $10

		.if	BASIC_REVISION = 1
		CLC
		.endif
		LDA	STENUM			; ADD $10 TO
		ADC	#$10			; ENTRY NUMBER TO
		STA	SVONTC			; GET OPERATOR CODE

;; IF *CPC == SVONTC: GOTO @SONT2  # Go Set Operator

@SONT1:		LDY	#0
		LDA	(CPC),Y			; GET SYNTAX REQ CODE
		CMP	SVONTC			; DOES IT MATCH THE FOUND
		BEQ	@SONT2			; BR IF MATCH

;; IF *CPC != CNFNP: GOTO @SONTF   # Not CNFNP ? Fail

		CMP	#CNFNP			; WAS REQ NFNP

		.if	BASIC_REVISION = 1
		BNE	@SONTF			; BR IF NOT

;; IF SVONTC > CNFNP: GOTO @SONTS  # Not <= CNFNP ? Fail

		LDA	SVONTC			; GET WHAT WE GOT
		CMP	#CNFNP			; IS IT NFNA
		BCS	@SONTS			; BR IF IT IS

;; RETURN FAIL

@SONTF:		SEC				; REPORT FAIL
		RTS

;; _SETCODE(SVONTC)

@SONTS:		LDA	SVONTC			; GET REAL CODE

		.else

		bne	@SONF1
		lda	SVONTC
		cmp	#CNFNP
		bcc	@SONF1

		.endif

@SONT2:		JSR	_SETCODE		; GO SET CODE

;; CIX = SVONTL

		LDX	SVONTL			; INC CIX BY
		STX	CIX

;; RETURN OK
		CLC				; REPORT SUCCESS
		RTS				; DONE

;; SVONTC = 0

@SONF:		LDA	#0			; SET ZERO AS
		STA	SVONTC			; SAVED CODE

;; RETURN FAIL

@SONF1:		SEC
		RTS				; DONE

;---------------------------------------------------------------
; _TNVAR - EXTERNAL SUBROUTINE FOR TNVAR & TSVAR
;
; ESRT to get valid Numeric ($00) or String ($80) variable name
; and to create storage for the variable if it has not been seen.
; Fail = Carry Set (Invalid Name, Wrong Var Type)
;---------------------------------------------------------------

;; def _TNVAR():

_TNVAR:		LDA	#$00			; SET NUMERIC TEST
		BEQ	_TVAR

_TSVAR:		LDA	#$80			; SET STR TEST

_TVAR:

;; TVTYPE = A  # $00 or $80

		STA	TVTYPE			; SAVE TEST TYPE

;; SKIP_BLANKS()

		JSR	SKPBLANK		; SKIP LEADING BLANKS

;; TVSCIX = CIX

		LDA	CIX			; GET INDEX
		STA	TVSCIX			; FOR SAVING

;; IF _TSTALPH() == FAIL: GOTO @TVFAIL

		JSR	_TSTALPH		; GO TEST FIRST CHAR
		BCS	@TVFAIL			; BR NOT ALPHA

;; SEARCH_ONT()
;; IF SVONTC == 0: GOTO @TV1

		JSR	SRCONT			; IF THIS IS A
		LDA	SVONTC			; RESVD NAME
		BEQ	@TV1			; BR NOT RSVDNAME

;; IF [INBUFF][SVONTL] < '0': GOTO @TVFAIL

		LDY	SVONTL			; IF NEXT CHAR AFTER
		LDA	(INBUFF),Y		; RESERVED NAME
		CMP	#'0'			; NOT ALARM NUMERIC
		BCC	@TVFAIL			; THEN ERROR

@TV1:

;; CIX++ ; if _TSTALPH() || TSTNUM(): GOTO @TV1

		INC	CIX			; INC TO NEXT CHAR
		JSR	_TSTALPH		; TEST ALPHA
		BCC	@TV1			; BR IF ALPHA
		JSR	TSTNUM			; TRY NUMBER
		BCC	@TV1			; BR IF NUMBER

;; IF [INBUFF][SVONTL] == '$': GOTO @TVSTR  # go check if string var ok

		LDA	(INBUFF),Y		; GET OFFENDING CHAR
		CMP	#'$'			; IS IT $
		BEQ	@TVSTR			; BR IF $ [STRING]

;; IF TVTYPE & 0x80 == 0x00: GOTO @TVOK  # fail if numeric var not ok

		BIT	TVTYPE			; THIS A NVAR SEARCH
		BPL	@TVOK			; BR IF NVAR

@TVFAIL:

;; RETURN FAIL
		SEC				; SET FAIL CODE
		RTS				; DONE

@TVSTR:

;; IF TVTYPE & 0x80 == 0x00: GOTO @TVFAIL

		BIT	TVTYPE			; TEST SVAR SEARCH
		BPL	@TVFAIL			; BR IF SVAR

;; Y = SVONTL + 1
;; IF Y < 256: GOTO @TVOK2  # "always" branch

		INY				; INC OVER $
		BNE	@TVOK2			; BR ALWAYS

@TVOK:

;; IF [[INBUFF]+Y] != '(': GOTO @TVOK2

		LDA	(INBUFF),Y		; GET NEXT CHAR
		CMP	#'('			; IS IT PAREN
		BNE	@TVOK2			; BR NOT PAREN

;; Y++

		INY				; INC OVER PAREN

;; TVTYPE |= VT_ARRAY

		LDA	#$40			; OR IN ARRAY
		ORA	TVTYPE			; CODE TO TVTYPE
		STA	TVTYPE

@TVOK2:

;; CIX = TVSCIX ; TVSCIX = Y

		LDA	TVSCIX			; GET SAVED CIX
		STA	CIX			; PUT BACK
		STY	TVSCIX			; SAVE NEW CIX

;; X = SEARCH(VNTP, 0)
		LDA	VNTP+1			; SEARCH VNT
		LDY	VNTP			; FOR THIS GUY
		LDX	#0
		JSR	SEARCH

@TVRS:

;; IF CS == NOT_FOUND: GOTO @TVS0

		BCS	@TVS0			; BR NOT FOUND

;; IF X == TVSCIX: GOTO @TVSUC

		CPX	TVSCIX			; FOUND RIGHT ONE
		BEQ	@TVSUC			; BR IF YES

;; SRCNXT() ; GOTO @TVRS

		JSR	SRCNXT			; GO SEARCH MORE
		JMP	@TVRS			; TEST THIS RESULT

@TVS0:

;; CIX = TVSCIX - CIX

		SEC				; SIGH:
		LDA	TVSCIX			; VAR LENGTH IS
		SBC	CIX			; NEW CIX-OLD CIX
		STA	CIX

;; EXPAND_U8(VNTD, CIX)

		TAY				; GO EXPAND VNT
		LDX	#VNTD			; BY VAR LENGTH
		JSR	EXPLOW

;; TVNUM = STENUM

		LDA	STENUM			; SET VARIABLE NUMBER
		STA	TVNUM

;; Y = CIX-1 ; X = TVSCIX-1

		LDY	CIX			; AND
		DEY
		LDX	TVSCIX			; GET DISPL TO EQU+1
		DEX
@TVS1:

;; [[SVESA]+Y] = [LBUFF+X]

		LDA	LBUFF,X			; MOVE VAR TO
		STA	(SVESA),Y

;; X-- ; Y-- ; IF Y >= 0: GOTO @TVS1

		DEX
		DEY
		BPL	@TVS1

;; [[SVESA]+CIX-1] |= 0x80

		LDY	CIX			; TURN ON MSB
		DEY				; OF LAST CHAR
		LDA	(SVESA),Y		; IN VTVT ENTRY
		ORA	#$80
		STA	(SVESA),Y

;; EXPAND_U8(STMTAB, 8)

		LDY	#8			; THEN EXPAND
		LDX	#STMTAB			; VVT BY 8
		JSR	EXPLOW

;; SVVVTE++

		INC	SVVVTE			; INC VVT EXP SIZE

;; FOR Y = 2 TO 8: [TVTYPE+Y] = 0

		LDY	#2			; CLEAR VALUE
		LDA	#0			; PART OF
@TVS1A:		STA	TVTYPE,Y		; ENTRY
		INY
		CPY	#8
		BCC	@TVS1A

;; FOR Y = 7 TO 0: [[SVESA]+Y] = [TVTYPE+Y]

		DEY				; AND THEN
@TVS2:		LDA	TVTYPE,Y		; PUT IN VAR TABLE
		STA	(SVESA),Y		; ENTRY
		DEY
		BPL	@TVS2

@TVSUC:

;; IF TVTYPE & 0x40 == 0: GOTO @TVNP

		BIT	TVTYPE			; WAS THERE A PAREN
		BVC	@TVNP			; BR IF NOT

;; TVSCIX--

		DEC	TVSCIX			; LET SYNTAX SEE PAREN

@TVNP:

;; CIX = TVSCIX

		LDA	TVSCIX			; GET NEW CIX
		STA	CIX			; TO CIX

;; A = STENUM ; IF A & 0x80: GOTO @TVFULL

		LDA	STENUM			; GET TABLE ENTRY NO
		BMI	@TVFULL			; BR IF > $7F

;; A |= 0x80

		ORA	#$80			; MAKE IT > $7F

;; _SETCODE(A)  # Variable ID Token
;; RETURN OK

		.if	BASIC_REVISION = 1
		JSR	_SETCODE		; SET CODE TO OUTPUT BUFFER
		CLC				; SET SUCCESS CODE
		RTS				; RETURN
		.else
		clc
		jmp	_SETCODE
		.endif

;; GOTO ERRVSF  # Variable Table Full

@TVFULL:	jmp	ERRVSF			; GO TO ERROR RTN

;---------------------------------------------------------------
; TSTALPH	TEST CIX FOR ALPHA
;---------------------------------------------------------------

;; def _TSTALPH(): RETURN ('A' <= INBUFF[CIX] <= 'Z') ? OK : FAIL

_TSTALPH:	LDY	CIX
		LDA	(INBUFF),Y
TSALPH:		CMP	#'A'
		BCC	_TAFAIL
		CMP	#'Z'+1
		RTS
_TAFAIL:	SEC
		RTS

;---------------------------------------------------------------
; _TNCON	EXTERNAL SUBROUTINE TO CHECK FOR NUMBER
;---------------------------------------------------------------

;; def _TNCON():

;; SKIP_BLANKS()

_TNCON:		JSR	SKBLANK

;; TVSCIX = CIX

		LDA	CIX
		STA	TVSCIX

;; AY = ASC_TO_FP()  # Advances CIX

		JSR	CVAFP			; GO TEST AND CONV

;; IF AY == NAN: { CIX = TVSCIX ; RETURN }

		BCC	@TNC1			; BR IF NUMBER
		LDA	TVSCIX
		STA	CIX
		RTS				; RETURN FAIL

@TNC1:

;; _SETCODE(CBCD)

		LDA	#CBCD			; SET NUMERIC CONST
		JSR	_SETCODE

;; Y = COX

		.if	BASIC_REVISION = 1
		LDY	COX
		.else
		iny
		.endif

;; # OUTBUFF.ADD(FR0)  would advance COX
;; FOR (X = 0; X < 6: ++X, ++Y): [[OUTBUFF]+Y] = [FR0+X]  # Copy FP6

		LDX	#0
@TNC2:		LDA	FR0,X			; MOVE CONST TO STMT
		STA	(OUTBUFF),Y
		INY
		INX
		CPX	#6
		BCC	@TNC2

;; COX = Y  # Past Inserted FP6

		STY	COX
		CLC

;; RETURN
		RTS

;---------------------------------------------------------------
; _TSCON	EXTERNAL SUBROUTINE TO CHECK FOR STRING CONSTANT
;---------------------------------------------------------------

_TSCON:

;; SKIP_BLANKS()

		JSR	SKBLANK

;; IF [INBUFF+CIX] != '"': RETURN FAIL

		LDY	CIX			; GET INDEX
		LDA	(INBUFF),Y		; GET CHAR
		CMP	#'"'			; IS IT DQUOTE

		.if	BASIC_REVISION = 1
		BEQ	@TSC1			; BR IF DQ
		SEC				; SET FAIL
		RTS				; RETURN
		.else
		bne	_TAFAIL
		.endif

@TSC1:

;; _SETCODE(CSTOK)  # String Literal Token

		LDA	#CSTOK			; SET SCON CODE
		JSR	_SETCODE

;; TSCOX = COX ; _SETCODE(A)  # Dummy Length

		LDA	COX			; SET COX
		STA	TSCOX			; SAVE FOR LENGTH
		JSR	_SETCODE		; SET DUMMY FOR NOW

@TSC2:

;; CIX++ ; IF [[INBUFF]+CIX] == CR: GOTO @TSC4

		INC	CIX			; NEXT INPUT CHAR
		LDY	CIX
		LDA	(INBUFF),Y
		CMP	#CR			; IS IT CR
		BEQ	@TSC4			; BR IF CR

;; IF A != '"': { _SETCODE(A) ; GOTO @TSC2 }

		CMP	#'"'			; IS IT DQ
		BEQ	@TSC3			; BR IF DQ
		JSR	_SETCODE		; OUTPUT IT
		JMP	@TSC2			; NEXT

;; CIX++

@TSC3:		INC	CIX			; INC CIX OVER DQ

;; [[OUTBUFF]+TSCOX] = COX-TSCOX-1

@TSC4:		CLC
		LDA	COX			; LENGTH IS COX MINUS
		SBC	TSCOX			; LENGTH BYTE COX
		LDY	TSCOX
		STA	(OUTBUFF),Y		; SET LENGTH

_TSSUC:

;; RETURN OK
		CLC				; SET SUCCESS
		RTS				; DONE

;---------------------------------------------------------------
; Search a Table
;---------------------------------------------------------------
;		TABLE FORMAT:
;		GARBAGE TO SKIP [N]
;		ASCII CHAR      [N]
;		  WITH LEAST SIGNIFICANT BYTE HAVING
;		  MOST SIGNIFICANT BIT ON
;		LAST TABLE ENTRY MUST HAVE FIRST ASCII
;		  CHAR = 0
;---------------------------------------------------------------
;		ENTRY PARMS:
;		  X = SKIP LENGTH
;		  A,Y = TABLE ADR [HIGH LOW]
;		  ARGUMENT = INBUFF + CIX
;		EXIT PARMS:
;		  CARRY = CLEAR IF FOUND
;		  X = FOUND ARGUMENT END CIX+1
;		  SRCADR = TABLE ENTRY ADR
;		  STENUM = TABLE ENTRY NUMBER
;---------------------------------------------------------------

SEARCH:

;; SRCSKP = X ; STENUM = -1

		STX	SRCSKP			; SAVE SKIP FACTOR
		LDX	#$FF			; SET ENTRY NUMBER
		STX	STENUM			; TO ZERO

_SRC1:

;; SRCADR = YA

		STA	SRCADR+1		; SET SEARCH ADR
		STY	SRCADR

;; STENUM++

		INC	STENUM			; INC ENTRY NUMBER

;; X = CIX

		LDX	CIX			; GET ARG DISPL

;; A = [[SRCADR]+SRCSKP] ; IF A == 0 GOTO _SRCNF

		LDY	SRCSKP			; GET SKIP LENGTH
		LDA	(SRCADR),Y		; GET FIRST CHAR
		BEQ	_SRCNF			; BR IF EOT

;; A = 0 ; PUSH(P)

		LDA	#0			; SET STATUS = EQ
		PHP				; AND PUSH IT

_SRC2:

;; A = [LBUFF+X] & 0x7F    # Ignore inverted. So inverted keywords are ok.
;; IF A == '.' GOTO _SRC5  # Command Keyword has dot shorthand

		LDA	LBUFF,X			; GET INPUT CHAR
		AND	#$7F			; TURN OFF MSB
		CMP	#'.'			; IF WILD CARD
		BEQ	_SRC5			; THEN BR
_SRC2A:

;; IF (A ^ [[SRCADR]+Y]) & 0x7F == 0: GOTO _SRC3  # ASL used to test low 7 bits (not for A*2)

		EOR	(SRCADR),Y		; EX-OR WITH TABLE CHAR
		ASL				; SHIFT MSB TO CARRY
		BEQ	_SRC3			; BR IF [ARG=TAB] CHAR

;; A = PULL() ; P = PULL()

		PLA				; POP STATUS
		PHP				; PUSH NEW STATUS

_SRC3:

;; Y++ ; X++ ; IF X & 0xFF: GOTO _SRC2

		INY				; INC TABLE INDEX
		INX				; INC ARG INDEX
		BCC	_SRC2			; IF TABLE MSB OFF, CONTINUE
						; ELSE END OF ENTRY

;; P = PULL() ; IF P.ZERO_FLAG: GOTO _SRCFND

		PLP				; GET STATUS
		.if	BASIC_REVISION = 1
		BEQ	_SRCFND			; BR IF NO MIS MATCH
		.else
		beq	_TSSUC
		.endif

SRCNXT:
;; YA = YA + SRCADR
		CLC
		TYA				; ACV=ENTRY LENGTH
		ADC	SRCADR			; PLUS START ADR [L]
		TAY				; TO Y
		LDA	SRCADR+1		; ETC
		ADC	#0

;; GOTO _SRC1

		BNE	_SRC1			; BR ALWAYS

_SRCFND:	.if	BASIC_REVISION = 1

;; RETURN OK
		CLC				; INDICATE FOUND
		RTS
		.endif

_SRCNF:

;; RETURN FAIL
		SEC				; INDICATE NOT FOUND
		RTS

_SRC5:

;; IF _SRCSKIP != 2: GOTO _SRC2A   # go to compare A & 0x7F...

		LDA	#2			; IF NOT
		CMP	SRCSKP			; STMT NAME TABLE
		BNE	_SRC2A			; THEN IGNORE

@SRC6:

;; IF [[SRCADR]+Y] & 0x80: GOTO @SRC7

		LDA	(SRCADR),Y		; TEST MSB OF TABLE
		BMI	@SRC7			; IF ON DONE
		INY				; ELSE
		BNE	@SRC6			; LOOK AT NEXT CHAR
@SRC7:

;; P.CS = 1 ; GOTO _SRC3  # CS = Fall thru to PLP...

		SEC				; INDICATE MSB ON
		BCS	_SRC3			; AND RE-ENTER CODE

;---------------------------------------------------------------
; Statement Name Table
;---------------------------------------------------------------
; SNTAB- STATEMENT NAME TABLE
;	EACH ENTRY HAS SYNTAX TBLE ADR PTR
;	FOLLOWED BY STMT NAME
;---------------------------------------------------------------

NTOK		.set	$00
SNTAB:		WordBasicString _SREM,		"REM",		kREM
		WordBasicString _SDATA,		"DATA",		kDATA
		WordBasicString _SINPUT,	"INPUT",	kINPUT
		WordBasicString _SCOLOR,	"COLOR",	kCOLOR
		WordBasicString _SLIST,		"LIST",		kLIST
		WordBasicString _SENTER,	"ENTER",	kENTER
		WordBasicString _SLET,		"LET",		kLET
		WordBasicString _SIF,		"IF",		kIF
		WordBasicString _SFOR,		"FOR",		kFOR
		WordBasicString _SNEXT,		"NEXT",		kNEXT
		WordBasicString _SGOTO,		"GOTO",		kGOTO
		WordBasicString _SGOTO,		"GO TO",	kGO_TO
		WordBasicString _SGOSUB,	"GOSUB",	kGOSUB
		WordBasicString _STRAP,		"TRAP",		kTRAP
		WordBasicString _SBYE,		"BYE",		kBYE
		WordBasicString _SCONT,		"CONT",		kCONT
		WordBasicString _SCOM,		"COM",		kCOM
		WordBasicString _SCLOSE,	"CLOSE",	kCLOSE
		WordBasicString _SCLR,		"CLR",		kCLR
		WordBasicString _SDEG,		"DEG",		kDEG
		WordBasicString _SDIM,		"DIM",		kDIM
		WordBasicString _SEND,		"END",		kEND
		WordBasicString _SNEW,		"NEW",		kNEW
		WordBasicString _SOPEN,		"OPEN",		kOPEN
		WordBasicString _SLOAD,		"LOAD",		kLOAD
		WordBasicString _SSAVE,		"SAVE",		kSAVE
		WordBasicString _SSTATUS,	"STATUS",	kSTATUS
		WordBasicString _SNOTE,		"NOTE",		kNOTE
		WordBasicString _SPOINT,	"POINT",	kPOINT
		WordBasicString _SXIO,		"XIO",		kXIO
		WordBasicString _SON,		"ON",		kON
		WordBasicString _SPOKE,		"POKE",		kPOKE
		WordBasicString _SPRINT,	"PRINT",	kPRINT
		WordBasicString _SRAD,		"RAD",		kRAD
		WordBasicString _SREAD,		"READ",		kREAD
		WordBasicString _SREST,		"RESTORE",	kRESTORE
		WordBasicString _SRET,		"RETURN",	kRETURN
		WordBasicString _SRUN,		"RUN",		kRUN
		WordBasicString _SSTOP,		"STOP",		kSTOP
		WordBasicString _SPOP,		"POP",		kPOP
		WordBasicString _SPRINT,	"?",		kQUESTION
		WordBasicString _SGET,		"GET",		kGET
		WordBasicString _SPUT,		"PUT",		kPUT
		WordBasicString _SGR,		"GRAPHICS",	kGRAPHIC
		WordBasicString _SPLOT,		"PLOT",		kPLOT
		WordBasicString _SPOS,		"POSITION",	kPOSITION
		WordBasicString _SDOS,		"DOS",		kDOS
		WordBasicString _SDRAWTO,	"DRAWTO",	kDRAWTO
		WordBasicString _SSETCOLOR,	"SETCOLOR",	kSETCOLOR
		WordBasicString _SLOCATE,	"LOCATE",	kLOCATE
		WordBasicString _SOUND,		"SOUND",	kSOUND
		WordBasicString _SLPRINT,	"LPRINT",	kLPRINT
		WordBasicString _SCSAVE,	"CSAVE",	kCSAVE
		WordBasicString _SCLOAD,	"CLOAD",	kCLOAD

		NextToken kILET

		.word	_SILET-1	; silent "LET"
		.byte	$00		; 00 = Last

		NextToken kERROR

		.byte	$80,$00
		BasicString "*ERROR-  "

		.if	BASIC_REVISION > 1
_MSTOP:		BasicString "STOPPED "
		.endif

;---------------------------------------------------------------
; SYNTAX TABLES
;---------------------------------------------------------------

;---------------------------------------------------------------
; Syntax Table OP Codes
;---------------------------------------------------------------
_ANTV	= $00	; Absolute Non-Terminal Vector (ANTV) to sub-call another rule
_ESRT	= $01	; External Subroutine Call (ESRT) to call a handler for more complex rules
_OR	= $02	; ABML or
_RTN	= $03	; (aka <END>) Return, marks the end of an ABML rule. Return pass or fail.
_TERM	= $05	; Above this point are Terminal Codes
_UNKN	= $0D	; (aka <UNKN>) Not sure what this does in BASIC Rev. B.
_VEXP	= $0E	; (aka <EXP>) Expression Non-Terminal Vector. Shorthand for ANTV AD(EXP)
_CHNG	= $0F	; Change Last Token to X. e.g., to rectify '=' as assign or compare.

;---------------------------------------------------------------
; Syntax Table Helper macros expand for use with .byte
;---------------------------------------------------------------

.define	JS(aa) $80 + (((aa - *) & $7F) ^ $40)	; Jump to Sub-rule address label
.define	AD(aa) (aa - 1)				; Adr-1 for use with PHA+PHA+RTS (e.g., @STGO, _EXOP)
.define	CHNG(vv) _CHNG, vv			; Change found token to target token
.define	ANTV(aa) _ANTV, <AD(aa), >AD(aa)	; Process a distant Sub-rule
.define	ESRT(aa) _ESRT, <AD(aa), >AD(aa)	; JSR to 6502 subroutine code
.define	UNJS(vv) (* + (vv ^ $40) - $80)		; Utility function reverse JS back to offset

;---------------------------------------------------------------
; <EXP> = (<EXP>)<NOP> | <UNARY><EXP> | <NV><NOP>#
;---------------------------------------------------------------

_EXP:		.if	BASIC_REVISION = 1
		.byte	CLPRN,JS(_EXP),CRPRN,JS(_NOP),_OR,JS(_UNARY),JS(_EXP),_OR
		.else
		.byte	JS(_UNARY),JS(@EXP2),_OR,JS(@EXP2),_RTN
@EXP2:		.byte	CLPRN,JS(_SFUN),CRPRN,JS(_NOP),_OR
		.endif
		.byte	JS(_NV),JS(_NOP),_RTN

;---------------------------------------------------------------
; <UNARY> = + | - | NOT#
;---------------------------------------------------------------

_UNARY:		.byte	CPLUS,CHNG(CUPLUS),_OR,CMINUS,CHNG(CUMINUS),_OR,CNOT,_RTN

;---------------------------------------------------------------
; <NV> = <NFUN> | <NVAR> | <NCON> | <STCOMP>#
;---------------------------------------------------------------

_NV:		.byte	JS(_NFUN),_OR,JS(_NVAR),_OR,ESRT(_TNCON),_OR,ANTV(_STCOMP),_RTN

;---------------------------------------------------------------
; <NOP> = <OP><EXP> | &#
;---------------------------------------------------------------

_NOP:		.byte	JS(_OP),JS(_EXP),_OR,_RTN

;---------------------------------------------------------------
; <OP> = ** | + | - | * | / | <= | >= | <> | > | < | = | AND | OR#
;---------------------------------------------------------------

_OP:		.byte	CEXP,_OR,CPLUS,_OR,CMINUS,_OR,CMUL,_OR,CDIV,_OR,CLE,_OR,CGE,_OR
		.byte	CNE,_OR,CGT,_OR,CLT,_OR,CEQ,_OR,CAND,_OR,COR,_RTN

;---------------------------------------------------------------
; <NVAR> = <TNVAR><NMAT>#
;---------------------------------------------------------------

_NVAR:		.byte	ESRT(_TNVAR),JS(_NMAT),_RTN

;---------------------------------------------------------------
; <NMAT> = (<EXP><NMAT2>) | &#
;---------------------------------------------------------------

_NMAT:		.if	BASIC_REVISION > 1
		.byte	_UNKN
		.endif
		.byte	CLPRN,CHNG(CALPRN),_VEXP,JS(_NMAT2),CRPRN,_OR,_RTN

;---------------------------------------------------------------
; <NMAT2> = ,<EXP> | &#
;---------------------------------------------------------------

_NMAT2:		.byte	CCOM,CHNG(CACOM),_VEXP,_OR,_RTN

;---------------------------------------------------------------
; <NFUN> = <NFNP><NFP> | <NFSP><SFP> | <NFUSR>#
;---------------------------------------------------------------

_NFUN:		.byte	CNFNP,JS(_NFP),_OR,ANTV(_NFSP),JS(_SFP),_OR,JS(_NFUSR),_RTN

;---------------------------------------------------------------
; <NFUSR> = USR(<PUSR>)#
;---------------------------------------------------------------

_NFUSR:		.byte	CUSR,CLPRN,CHNG(CFLPRN),ANTV(_PUSR),CRPRN,_RTN

;---------------------------------------------------------------
; <NFP> = (<EXP>)#
;---------------------------------------------------------------

_NFP:		.byte	CLPRN,CHNG(CFLPRN),_VEXP,CRPRN,_RTN

;---------------------------------------------------------------
; <SFP> = <STR>)#
;---------------------------------------------------------------

_SFP:		.byte	CLPRN,CHNG(CFLPRN),JS(_STR),CRPRN,_RTN

;---------------------------------------------------------------
; <STCOMP> = <STR><SOP><STR>#
;---------------------------------------------------------------

_STCOMP:	.byte	JS(_STR),JS(_SOP),JS(_STR),_RTN

;---------------------------------------------------------------
; <STR> = <SFUN> | <SVAR> | <SCON>#
;---------------------------------------------------------------

_STR:		.byte	JS(_SFUN),_OR,JS(_SVAR),_OR,ESRT(_TSCON),_RTN

;---------------------------------------------------------------
; <SFUN> = SFNP<NFP>#
;---------------------------------------------------------------

_SFUN:		.byte	ANTV(_SFNP),JS(_NFP),_RTN

;---------------------------------------------------------------
; <SVAR> = <TSVAR><SMAT>#
;---------------------------------------------------------------

_SVAR:		.byte	ESRT(_TSVAR),JS(_SMAT),_RTN

;---------------------------------------------------------------
; <SMAT> = (<EXP><SMAT2>) | &#
;---------------------------------------------------------------

_SMAT:		.byte	CLPRN,CHNG(CSLPRN),_VEXP,JS(_SMAT2),CRPRN,_OR,_RTN

;---------------------------------------------------------------
; <SMAT2> = ,<EXP> | &#
;---------------------------------------------------------------

_SMAT2:		.byte	CCOM,CHNG(CACOM),_VEXP,_OR,_RTN

;---------------------------------------------------------------
; <SOP> = <><#
;---------------------------------------------------------------

_SOP:		.byte	CLE,CHNG(CSLE),_OR
		.byte	CNE,CHNG(CSNE),_OR
		.byte	CGE,CHNG(CSGE),_OR
		.byte	CGT,CHNG(CSGT),_OR
		.byte	CLT,CHNG(CSLT),_OR
		.byte	CEQ,CHNG(CSEQ),_RTN

;---------------------------------------------------------------
;	PUT statement
;---------------------------------------------------------------
; <PUT> = <D1>,<EXP><EOS>#
;---------------------------------------------------------------

_SPUT:		.byte	CPND,_VEXP,CCOM

;---------------------------------------------------------------
;	COLOR, GOTO, GO TO, GOSUB, TRAP, GRAPHICS statement
;---------------------------------------------------------------

_STRAP:
_SGOTO:
_SGOSUB:
_SGR:
_SCOLOR:
_XEOS:		.byte	_VEXP

;---------------------------------------------------------------
;	BYE, CONT, CLR, DEG, END, NEW, RAD, RETURN,
;	STOP, POP, DOS, CSAVE, CLOAD statement
;---------------------------------------------------------------

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
_SRAD:		.byte	JS(_EOS),_RTN

;---------------------------------------------------------------
;	LET, silent LET statement
;---------------------------------------------------------------
; <LET> = <NVAR> = <EXP><EOS> | <SVAR>=<STR><EOS>#
;---------------------------------------------------------------

_SLET:
_SILET:		.byte	ANTV(_NVAR),CEQ,CHNG(CAASN),_VEXP,JS(_EOS),_OR
		.byte	JS(_SVAR),CEQ,CHNG(CSASN),ANTV(_STR),JS(_EOS),_RTN

;---------------------------------------------------------------
;	FOR statement
;---------------------------------------------------------------
; <FOR> = <TNVAR> = <EXP> TO <EXP><FSTEP><EOS>#
;---------------------------------------------------------------

_SFOR:		.byte	ESRT(_TNVAR),CEQ,CHNG(CAASN),_VEXP,CTO,_VEXP,JS(_FSTEP),JS(_EOS),_RTN

;---------------------------------------------------------------
; <FSTEP> = STEP<EXP> | &
;---------------------------------------------------------------

_FSTEP:		.byte	CSTEP,_VEXP,_OR,_RTN

;---------------------------------------------------------------
;	LOCATE statement
;---------------------------------------------------------------
; <LOCATE> = <EXP>,<EXP>,<TNVAR><EOL>#
;---------------------------------------------------------------

_SLOCATE:	.byte	_VEXP,CCOM,_VEXP,CCOM,JS(_SNEXT),_RTN

;---------------------------------------------------------------
;	GET statement
;---------------------------------------------------------------
; <GET> = <D1>,<TNVAR>#
;---------------------------------------------------------------

_SGET:		.byte	JS(_D1),CCOM

;---------------------------------------------------------------
;	NEXT statement
;---------------------------------------------------------------
; <NEXT> = <TNVAR><EOS>#
;---------------------------------------------------------------

_SNEXT:		.byte	ESRT(_TNVAR),JS(_EOS),_RTN

;---------------------------------------------------------------
;	RESTORE statement
;---------------------------------------------------------------
; <RESTORE> = <EXP><EOS> | <EOS>#
;---------------------------------------------------------------

_SREST:		.byte	_VEXP,JS(_EOS),_OR,JS(_EOS),_RTN

;---------------------------------------------------------------
;	INPUT statement
;---------------------------------------------------------------
; <INPUT> = <OPD><READ>#
;---------------------------------------------------------------

_SINPUT:	.byte	JS(_OPD)

;---------------------------------------------------------------
;	READ statement
;---------------------------------------------------------------
; <READ> = <NSVARL><EOS>#
;---------------------------------------------------------------

_SREAD:		.byte	JS(_NSVRL),JS(_EOS),_RTN

;---------------------------------------------------------------
; <EOS> = : | CR #
;---------------------------------------------------------------

_EOS:		.byte	CEOS,_OR,CCR,_RTN

;---------------------------------------------------------------
;	PRINT, ? statement
;---------------------------------------------------------------
; <PRINT> = <D1><EOS> | <D1><PR1><EOS>
;---------------------------------------------------------------

_SPRINT:	.byte	JS(_D1),JS(_EOS),_OR,JS(_OPD)

;---------------------------------------------------------------
;	LPRINT statement
;---------------------------------------------------------------
; <LPRINT> = <PR1> <EOS>
;---------------------------------------------------------------

_SLPRINT:	.byte	ANTV(_PR1),JS(_EOS),_RTN

;---------------------------------------------------------------
; <D1> = <CPND> <EXP> #
;---------------------------------------------------------------

_D1:		.byte	CPND,_VEXP,_RTN

;---------------------------------------------------------------
; <NSVAR> = <NVAR> | <SVAR> #
;---------------------------------------------------------------

_NSVAR:		.byte	ESRT(_TNVAR),_OR,ESRT(_TSVAR),_RTN

;---------------------------------------------------------------
; <NSVRL> = <NSVAR> | <NSV2> | <NADA> #   (Rev.1)
; <NSVRL> = <NSVAR> | <NSV2> #            (Rev.2+)
;---------------------------------------------------------------

_NSVRL:		.byte	JS(_NSVAR),JS(_NSV2)
		.if	BASIC_REVISION = 1
		.byte	_OR
		.endif
		.byte	_RTN

;---------------------------------------------------------------
; <NSV2> = ,<NSVRL> | &#
;---------------------------------------------------------------

_NSV2:		.byte	CCOM,JS(_NSVRL),_OR,_RTN

;---------------------------------------------------------------
;	XIO statement
;---------------------------------------------------------------
; <XIO> = <EXP>,<D2S>,<FS>,<EXP><EOS>#
;---------------------------------------------------------------

_SXIO:		.byte	_VEXP,CCOM

;---------------------------------------------------------------
;	OPEN statement
;---------------------------------------------------------------
; <OPEN> = <D1>,<EXP>,<EXP>,<FS><EOS>#
;---------------------------------------------------------------

_SOPEN:		.byte	JS(_D1),CCOM,JS(_TEXP),CCOM,JS(_FS),JS(_EOS),_RTN

;---------------------------------------------------------------
;	CLOSE statement
;---------------------------------------------------------------
; <CLOSE> = <D1><EOS>#
;---------------------------------------------------------------

_SCLOSE:	.byte	JS(_D1),JS(_EOS),_RTN

;---------------------------------------------------------------
;	ENTER, LOAD, SAVE statement
;---------------------------------------------------------------
; <ENTER> = <FS><EOS>#
;---------------------------------------------------------------

_SENTER:
_SLOAD:
_SSAVE:		.byte	JS(_FS),JS(_EOS),_RTN

;---------------------------------------------------------------
;	RUN statement
;---------------------------------------------------------------
; <RUN> = <FS><EOS2> | <EOS2>#
;---------------------------------------------------------------

_SRUN:		.byte	JS(_FS),JS(_EOS),_OR,JS(_EOS),_RTN

;---------------------------------------------------------------
; <OPD> = <D1>, | #
;---------------------------------------------------------------

_OPD:		.byte	JS(_D1),CCOM,_OR,JS(_D1),CSC,_OR,_RTN

;---------------------------------------------------------------
;	LIST statement
;---------------------------------------------------------------
; <LIST> = <FS>;<L2> | <L2>#
;---------------------------------------------------------------

_SLIST:		.byte	JS(_FS),JS(_EOS),_OR,JS(_FS),CCOM,JS(_LIS),_OR,JS(_LIS),_RTN

;---------------------------------------------------------------
; <LIS> = <L1><EOS2>#
;---------------------------------------------------------------

_LIS:		.byte	ANTV(_L1),JS(_EOS2),_RTN

;---------------------------------------------------------------
;	STATUS statement
;---------------------------------------------------------------
; <STATUS> = <STAT><EOS2>#
;---------------------------------------------------------------

_SSTATUS:	.byte	JS(_STAT),JS(_EOS2),_RTN

;---------------------------------------------------------------
; <STAT> = <D1>,<NVAR>#
;---------------------------------------------------------------

_STAT:		.byte	JS(_D1),CCOM,ANTV(_NVAR),_RTN

;---------------------------------------------------------------
;	NOTE, POINT statement
;---------------------------------------------------------------
; <SNOTE> = <STAT>,<NVAR><EOS2>#
;---------------------------------------------------------------

_SNOTE:
_SPOINT:	.byte	JS(_STAT),CCOM,ANTV(_NVAR),JS(_EOS2),_RTN

;---------------------------------------------------------------
; <FS> = <STR>
;---------------------------------------------------------------

_FS:		.byte	ANTV(_STR),_RTN

;---------------------------------------------------------------
; <TEXP> = <EXP>,<EXP>#
;---------------------------------------------------------------

_TEXP:		.byte	_VEXP,CCOM,_VEXP,_RTN

;---------------------------------------------------------------
;	SOUND statement
;---------------------------------------------------------------
; <SOUND> = <EXP>,<EXP>,<EXP>,<EXP><EOS>#
;---------------------------------------------------------------

_SOUND:		.byte	_VEXP,CCOM

;---------------------------------------------------------------
;	SETCOLOR statement
;---------------------------------------------------------------
; <SETCOLOR> = <EXP>,<EXP>,<EXP><EOS>#
;---------------------------------------------------------------

_SSETCOLOR:	.byte	_VEXP,CCOM

;---------------------------------------------------------------
;	POKE, PLOT, POSITION, DRAWTO statement
;---------------------------------------------------------------
; <...> = <EXP>,<EXP><EOS>#
;---------------------------------------------------------------

_SPOKE:
_SPLOT:
_SPOS:
_SDRAWTO:	.byte	JS(_TEXP),JS(_EOS2),_RTN

;---------------------------------------------------------------
;	COM, DIM statement
;---------------------------------------------------------------
; <DIM> = <NSML><EOS>#
;---------------------------------------------------------------

_SCOM:
_SDIM:		.byte	JS(_NSML),JS(_EOS2),_RTN

;---------------------------------------------------------------
;	ON statement
;---------------------------------------------------------------
; <ON> = <EXP><ON1><EXPL><EOS>#
;---------------------------------------------------------------
_SON:		.byte	_VEXP,JS(_ON1),JS(_EXPL),JS(_EOS2),_RTN
;---------------------------------------------------------------
; <ON1> = GOTO | GOSUB#
;---------------------------------------------------------------
_ON1:		.byte	CGTO,_OR,CGS,_RTN
;---------------------------------------------------------------
; <EXPL> = <EXP><EXPL1>#
;---------------------------------------------------------------
_EXPL:		.byte	_VEXP,JS(_EXPL1),_RTN
;---------------------------------------------------------------
; <EXPL1> = ,<EXPL> | &#
;---------------------------------------------------------------
_EXPL1:		.byte	CCOM,JS(_EXPL),_OR,_RTN
;---------------------------------------------------------------
; <EOS2> = CEOS | CCR#
;---------------------------------------------------------------
_EOS2:		.byte	CEOS,_OR,CCR,_RTN

;---------------------------------------------------------------
; <NSMAT> = <TNVAR>(<EXP><NMAT2>)
;---------------------------------------------------------------
_NSMAT:		.byte	ESRT(_TNVAR)
		.if	BASIC_REVISION > 1
		.byte	_UNKN
		.endif
		.byte	CLPRN,CHNG(CDLPRN),_VEXP,ANTV(_NMAT2),CRPRN,_OR
		.byte	ESRT(_TSVAR),CLPRN,CHNG(CDSLPR),_VEXP,CRPRN,_RTN

;---------------------------------------------------------------
; <NSML> = <NSMAT><NSML2> | &#
;---------------------------------------------------------------

_NSML:		.byte	JS(_NSMAT),JS(_NSML2),_OR,_RTN

;---------------------------------------------------------------
; <NSML2> = ,<NSML> | &#
;---------------------------------------------------------------

_NSML2:		.byte	CCOM,JS(_NSML),_OR,_RTN

;---------------------------------------------------------------
;	IF statement
;---------------------------------------------------------------
; <IF> = <EXP> THEN <IFA><EOS>#
;---------------------------------------------------------------

_SIF:		.byte	_VEXP,CTHEN,JS(_IFA),JS(_EOS2),_RTN
; <IFA> = <TNCON> | <EIF>
_IFA:		.byte	ESRT(_TNCON),_OR,ESRT(_EIF)

; <PR1> = <PEL> | <PSL><PR2> | &#
_PR1:		.byte	JS(_PEL),_OR,JS(_PSL),JS(_PR2),_OR,_RTN
; <PR2> = <PEL> | &#
_PR2:		.byte	JS(_PEL),_OR,_RTN
; <PEL> = <PES><PELA>#
_PEL:		.byte	JS(_PES),JS(_PELA),_RTN
; <PES> = <EXP> | <STR>
_PES:		.byte	_VEXP,_OR,ANTV(_STR),_RTN
; <PELA> = <PSL><PEL> | &#
_PELA:		.byte	JS(_PSL),JS(_PR2),_OR,_RTN
; <PSL> = <PS><PSLA>#
_PSL:		.byte	JS(_PS),JS(_PSLA),_RTN
; <PSLA> = <PSL> | &#
_PSLA:		.byte	JS(_PSL),_OR,_RTN
; <PS> = , | ,#
_PS:		.byte	CCOM,_OR,CSC,_RTN
; <L1> = <EXP><L2> | &#
_L1:		.byte	_VEXP,JS(_L2),_OR,_RTN
; <L2> = ,<EXP> | &#
_L2:		.byte	CCOM,_VEXP,_OR,_RTN

;---------------------------------------------------------------
;	REM statement
;---------------------------------------------------------------
; <REM> = <EREM>
;---------------------------------------------------------------

_SREM:		.byte	ESRT(_EREM)

;---------------------------------------------------------------
;	DATA statement
;---------------------------------------------------------------
; <DATA> = <EDATA>
;---------------------------------------------------------------

_SDATA:		.byte	ESRT(_EDATA)

;---------------------------------------------------------------
; <NFSP> = ASC | VAL | ADR | LEN#
;---------------------------------------------------------------

_NFSP:		.byte	CASC,_OR,CVAL,_OR,CADR,_OR,CLEN,_RTN

;---------------------------------------------------------------
; <SFNP> = STR | CHR#
;---------------------------------------------------------------

_SFNP:		.byte	CSTR,_OR,CCHR,_RTN

;---------------------------------------------------------------
; <PUSR> = <EXP><PUSR1>#
;---------------------------------------------------------------

_PUSR:		.byte	_VEXP,JS(_PUSR1),_RTN
; <PUSR1> = ,<PUSR> | &#
_PUSR1:		.byte	CCOM,CHNG(CACOM),JS(_PUSR),_OR,_RTN

;---------------------------------------------------------------
; OPNTAB - Operator Name Table
;---------------------------------------------------------------

OPNTAB:		.byte	$02+$80			; 0x10 DOUBLE QUOTE (unused?)
		.byte	$00+$80			; 0x11 DUMMY FOR SOE

		BasicString ","
		BasicString "$"
		BasicString ":"
		BasicString ";"
		.byte	CR			; CARRIAGE RETURN
		BasicString "GOTO"
		BasicString "GOSUB"
		BasicString "TO"
		BasicString "STEP"
		BasicString "THEN"
		BasicString "#"
						; START OF REAL OPS
		BasicString "<="		; XPLE		CLE
		BasicString "<>"		; XPNE		CNE
		BasicString ">="		; XPGE		CGE
		BasicString "<"			; XPLT		CGT
		BasicString ">"			; XPGT		CLT
		BasicString "="			; XPEQ		CEQ
		BasicString "^"			; XPPOWER	CEXP - UP ARROW FOR EXP
		BasicString "*"			; XPMUL		CMUL
		BasicString "+"			; XPPLUS	CPLUS
		BasicString "-"			; XPMINUS	CMINUS
		BasicString "/"			; XPDIV		CDIV

		BasicString "NOT"		; XPNOT		CNOT
		BasicString "OR"		; XPOR		COR
		BasicString "AND"		; XPAND		CAND
		BasicString "("			; XPLPRN	CLPRN
		BasicString ")"			; XPRPRN	CRPRN
		BasicString "="			; XPAASN	CAASN
		BasicString "="			; XSAASN	CSASN
		BasicString "<="		; XPSLE		CSLE
		BasicString "<>"		; XPSNE		CSNE
		BasicString ">="		; XPSGE		CSGE
		BasicString "<"			; XPSLT		CSLT
		BasicString ">"			; XPSGT		CSGT
		BasicString "="			; XPEQ		CSEQ
		BasicString "+"			; XPUPLUS	CUPLUS - UNARY PLUS
		BasicString "-"			; XPUMINUS	CUMINUS - UNARY MINUS

		BasicString "("			; XPSLPRN	CSLPRN - STRING LEFT PAREN
		.byte	$00+$80			; XPALPRN	CALPRN - ARRAY LEFT PAREN - DOES NOT PRINT
		.byte	$00+$80			; XPDLPRN	CDLPRN - DIM LEFT PAREN - DOES NOT PRINT
		BasicString "("			; XPFLPRN	CFLPRN - FUNCTION LEFT PAREN
		BasicString "("			; XDPSLP	CDSLPR - STRING LEFT PAREN
		BasicString ","			; XPACOM	CACOM - ARRAY COMMA

						; CNFNP
		BasicString "STR$"		; XPSTR		CSTR
		BasicString "CHR$"		; XPCHR		CCHR
		BasicString "USR"		; XPUSR		CUSR
		BasicString "ASC"		; XPASC		CASC
		BasicString "VAL"		; XPVAL		CVAL
		BasicString "LEN"		; XPLEN		CLEN
		BasicString "ADR"		; XPADR		CADR
		BasicString "ATN"		; XPATN		CATN
		BasicString "COS"		; XPCOS		CCOS
		BasicString "PEEK"		; XPPEEK	CPEEK
		BasicString "SIN"		; XPSIN		CSIN
		BasicString "RND"		; XPRND		CRND
		BasicString "FRE"		; XPFRE		CFRE
		BasicString "EXP"		; XPEXP		CEXP
		BasicString "LOG"		; XPLOG		CLOG
		BasicString "CLOG"		; XPL10		CCLOG
		BasicString "SQR"		; XPSQR		CSQR
		BasicString "SGN"		; XPSGN		CSGN
		BasicString "ABS"		; XPABS		CABS
		BasicString "INT"		; XPINT		CINT
		BasicString "PADDLE"		; XPPDL		CPADDLE
		BasicString "STICK"		; XPSTICK	CSTICK
		BasicString "PTRIG"		; XPPTRIG	CPTRIG
		BasicString "STRIG"		; XPSTRIG	CSTRIG

		.byte	$00			; 00 - End of Operator Name Table

		; END OF OPNTAB & FNTAB

;---------------------------------------------------------------
; Memory Manager
;---------------------------------------------------------------

;---------------------------------------------------------------
; MEMORY MANAGEMENT CONSISTS OF EXPANDING AND
; CONTRACTING TO INFORMATION AREA POINTED TO
; 8Y THE ZERO PAGE POINTER TABLES. ROUTINES
; MODIFY THE ADDRESS IN THE TABLES AND
; MOVE DATA AS REQUIRED. THE TWO FUNDAMENTAL
; ROUTINES ARE 'EXPAND' AND 'CONTRACT'
;---------------------------------------------------------------
; EXPAND
;	X = ZERO PAGE ADDRESS OF TABLE AT WHICH
;	EXPANSION IS TO START
;	Y EXPANSION SIZE IN BYTES [LOW]
;	A EXPANSION SIZE IN BYTES [HIGH]
;---------------------------------------------------------------
; EXPLOW - FOR EXPANSION < 256 BYTES
;		SETS A = 0
;---------------------------------------------------------------

;;
;; Pseudo-code in this section is less critical because Expand / Contract is now trivial.
;; Expand: Open up space, move upper contents up, reverse order.
;; Contract Close up space, move upper contents down, forward order.
;;

;; def EXPAND_U8(Y=SIZE):

EXPLOW:

		LDA	#0

;; def EXPAND(YA=SIZE):

EXPAND:		STY	ECSIZE			; SAVE EXPAND SIZE
		STA	ECSIZE+1

		.if	BASIC_REVISION > 1
		TYA
		.endif

;; YA = MEMTOP + ECSIZE

		SEC
		.if	BASIC_REVISION = 1
		LDA	MEMTOP			; TEST MEMORY TO BE FULL
		ADC	ECSIZE
		TAY				; MEMTOP+ECSIZE+1
		LDA	MEMTOP+1
		.else
		adc	MEMTOP
		tay
		lda	MEMTOP+1
		.endif
		ADC	ECSIZE+1		; MUST BE LE

;; IF A >= HIMEM.HI THEN:

		CMP	HIMEM+1
		BCC	@EXP2			; HIMEM

;;   IF A > HIMEM.HI: GOTO MEMFULL

		BNE	@EXP1

;;   IF Y > HIMEM: GOTO MEMFULL

		CPY	HIMEM
		BCC	@EXP2
		BEQ	@EXP2
@EXP1:		JMP	MEMFULL

;; ENDIF

@EXP2:

;; MVLNG = MEMTOP - [X]  # X is ZP Address of Some Pointer

		SEC				; FORM MOVE LENGTH [MVLNG]
		LDA	MEMTOP			; MOVE FROM ADR [MVFA]
		SBC	0,X			; MVLNG = MEMTOP-EXPAND ADR
		STA	MVLNG
		LDA	MEMTOP+1		; MVFA[L] EXP ADR [L]
		SBC	1,X
		STA	MVLNG+1			; MVFA[H] EXP ADR[H] + MVLNG[H]

;; MVFA.HI = MVLNG.HI + [X].HI
;; MVFA.LO = [X].LO

		CLC				; DURING MOVE MVLNG[L]
		ADC	1,X			; WILL BE ADDED SUCH
		STA	MVFA+1			; THAT MVFA = MEMTOP
		LDA	0,X			; SAVE PREMOVE EXPAND AT VALUE
		STA	MVFA			; SET MVFA LOW

;; SVESA = MVFA.LO

		STA	SVESA			; FORM MOVE TO ADR [MVTA]

;; MVTA = SVESA + ECSIZE

		ADC	ECSIZE			; MVTA[L] = EXP ADR[L] + ECSIZE[L]
		STA	MVTA			; MVTA[H) = [CARRY + EXP AD-[H]

;; SVESA.HI = [X].HI

		LDA	1,X			; +ECSIZE[H]] + MVLNG[H]
		STA	SVESA+1

;; MVTA.HI = MVLNG.HI + ECSIZE.HI + SVESA.HI

		ADC	ECSIZE+1		; DURING MOVE MVLNG[L]
		ADC	MVLNG+1			; WILL BE ADDED SUCH THAT
		STA	MVTA+1			; MVTA = MEMTOP + ECSIZE

@EXP3:

;; # Expand all that follow in: VNTD, VVTP, STMTAB, STMCUR, STARP, ENDSTAR, TOPRSTK
;; [X] += ECSIZE ; X += 2 ; IF X < MEMTOP+2: GOTO @EXP3

		LDA	0,X			; ADD ECSIZE TO
		ADC	ECSIZE			; ALL TABLE ENTRIES
		STA	0,X			; FROM EXPAND AT ADR
		LDA	1,X			; TO HIMEM
		ADC	ECSIZE+1
		STA	1,X
		INX
		INX
		CPX	#MEMTOP+2
		BCC	@EXP3

;; APMH = MEMTOP

		STA	APMH+1			; SET NEW APL
		LDA	MEMTOP			; HI MEM TO
		STA	APMH			; MEMTOP

;; YX = MVLNG + 0x100
;; GOTO (MVLNG.LO ? @EXP6 : @EXP7)

		LDX	MVLNG+1			; X = MVLNG[H]
		INX				; PLUS ONE
		LDY	MVLNG			; T = MVLNG[H]
		BNE	@EXP6			; TEST ZERO LENGTH

		.if	BASIC_REVISION = 1
		BEQ	@EXP7			; BR IF LOW = 0
		.elseif	BASIC_REVISION = 2
		dex
		bne	@EXP6
		rts
		.else
		nop
		beq	@EXP7			; BR IF LOW = 0
		nop
		.endif

;; Y = MVLNG - (MVLNG ? 1 : 0)

;; DO:  # (WHILE X > 0)

@EXP4:

;;   Y-- ; MVFA -= 0x100 ; MVTA -= 0x100

		DEY				; DEC MVLNG[L]
		DEC	MVFA+1			; DEC MVFA[H]
		DEC	MVTA+1			; DEC MVTA[H]

;;   DO:  # (WHILE Y > 0)
;;     [[MVTA]+Y] = [[MVFA]+Y]
;;     Y--
;;   :WHILE Y > 0
;;   [[MVTA]] = [[MVFA]]

@EXP5:		LDA	(MVFA),Y		; MVFA BYTE
		STA	(MVTA),Y		; TO MVTA
@EXP6:		DEY				; DEC COUNT LOW
		BNE	@EXP5			; BR IF NOT ZERO

		LDA	(MVFA),Y		; MOVE THE ZERO BYTE
		STA	(MVTA),Y

@EXP7:		DEX				; IF MVLNG[H] IS NOT
		BNE	@EXP4			; ZERO THEN MOVE 256 MORE

;;   X--
;; :WHILE X > 0
						; ELSE
		RTS				; DONE

;---------------------------------------------------------------
; CONTRACT
;	X ZERO PAGE ADR OF TABLE AT WHICH
;		CONTRACTION WILL START
;	Y CONTRACT SIZE IN BYTES [LOW]
;	A CONTRACT SIZE IN BYTES [HI]
; CONTLOW
;	TAKES A for Y in Rev.2
; CONTLOW1
;	SETS A = 0
;---------------------------------------------------------------

CONTLOW:
		.if	BASIC_REVISION > 1
;; def CONTRACT_U8(A):
		tay
		.endif

;; def CONTRACT_U8(Y):
CONTLOW1:	LDA	#0

;; def CONTRACT_U16(YA):
CONTRACT:

;; ECSIZE = YA

		STY	ECSIZE			; SAVE CONTRACT SIZE
		STA	ECSIZE+1

;; MVLNG = [X] - MEMTOP

		SEC				; FORM MOVE LENGTH [LOW]
		LDA	MEMTOP
		SBC	0,X			; MVLNG[L] = $100-
		EOR	#$FF			; [MEMTOP[L]] - CON AT VALUE [L]
		TAY				; THIS MAKES START Y AT
		INY				; MOVE HAVE A 2'S COMPLEMENT
		STY	MVLNG			; REMAINDER IN IT
		LDA	MEMTOP+1		; FORM MOVE LENGTH[HIGH]
		SBC	1,X
		STA	MVLNG+1

;; MVFA = [X] - MVLNG.LO

		LDA	0,X			; FORM MOVE FROM ADR [MVFA]
		SBC	MVLNG			; MVFA = CON AT VALUE
		STA	MVFA			; MINUS MVLNG[L]
		LDA	1,X			; DURING MOVE MVLNG[L]
		SBC	#0			; WILL BE ADDED BACK INTO
		STA	MVFA+1			; MVFA IN [IND],Y INST

;; MVTA = X

		STX	MVTA			; TEMP SAVE OF CON AT DISPL

;; # Contract all that follow in: VNTD, VVTP, STMTAB, STMCUR, STARP, ENDSTAR, TOPRSTK
;; [X] -= ECSIZE ; X += 2 ; IF X < MEMTOP+2: GOTO @CONT1

@CONT1:		SEC				; SUBTRACT ECSIZE FROM
		LDA	0,X			; ALL TABLE ENTRY FROM
		SBC	ECSIZE			; CON AT ADR TO HIMEM
		STA	0,X
		LDA	1,X
		SBC	ECSIZE+1
		STA	1,X
		INX
		INX
		CPX	#MEMTOP+2
		BCC	@CONT1

;; APMH = MEMTOP

		STA	APMH+1			; SET NEW APL
		LDA	MEMTOP			; HI MEM TO
		STA	APMH			; MEMTOP

;; X = MVTA
		LDX	MVTA

;; MVTA -= MVLNG.LO  # Carry set after BCC above

		LDA	0,X			; FORM MOVE TO ADDR [MVTA]
		SBC	MVLNG			; MVTA = NEW CON AT VALUE
		STA	MVTA			; MINUS MVLNG [L]
		LDA	1,X			; DURING MOVE MVLNG[L]
		SBC	#0			; WILL BE ADDED BACK INTO
		STA	MVTA+1			; MVTA IN [INO],Y INST

FMOVER:

;; YX = MVLNG + 0x100

		LDX	MVLNG+1			; GET MOVE LENGTH HIGH
		INX				; INC SO MOVE CAN BNE
		LDY	MVLNG			; GET MOVE LENGTH LOW

;; IF MVLNG.LO != 0: GOTO @CONT2

		BNE	@CONT2			; IF NOT ZERO GO

		.if	BASIC_REVISION = 1
;; IF MVLNG.LO == 0: GOTO @CONT4
		BEQ	@CONT4			; BR IF LOW = 0
		.else
;; IF MVLNG.HI != 0: GOTO @CONT2
		dex
		bne	@CONT2
		rts
		.endif

;; DO:  # (WHILE --X > 0)

@CONT3:

;;   MVFA += 0x100 ; MVTA += 0x100

		INC	MVFA+1			; INC MVFA[H]
		INC	MVTA+1			; INC MVTA[H]

;;   DO: [[MVTA]+Y] = [[MVFA]+Y] ; Y++ ; :WHILE Y <= 255

@CONT2:		LDA	(MVFA),Y		; GET MOVE FROM BYTE
		STA	(MVTA),Y		; GET MOVE TO BYTE
		INY				; INCREMENT COUNT LOW
		BNE	@CONT2			; BR IF NOT ZERO

@CONT4:		DEX				; DECREMENT COUNT HIGH
		BNE	@CONT3			; BR IF NOT ZERO

;; WHILE --X > 0

;; RETURN
		RTS				; ELSE DONE

;---------------------------------------------------------------
; Execute Control
;---------------------------------------------------------------

;---------------------------------------------------------------
; EXECNL - Execute Next Line
;---------------------------------------------------------------

EXECNL:

;; SETLN1()
		JSR	SETLN1			; SET UP LIN & NXT STMT

;---------------------------------------------------------------
; EXECNS - Execute Next Statement
;---------------------------------------------------------------

EXECNS:

;; IF TEST_BREAK(): GOTO XSTOP

		JSR	TSTBRK			; TEST BREAK
		.if	BASIC_REVISION = 1
		BNE	@EXBRK			; BR IF BREAK
		.else
		beq	@EXBRK			; BR IF BREAK
		.endif

;; IF NXTSTD >= MAXCIX: GOTO @EXEOL

		LDY	NXTSTD			; GET PTR TO NEXT STMT L
		CPY	MAXCIX			; AT END OF LINE
		BCS	@EXEOL			; BR IF EOL

;; NXTSTD = [[STMCUR]+Y]

		LDA	(STMCUR),Y		; GET NEW STMT LENGTH
		STA	NXTSTD			; SAVE AS FUTURE STMT LENGTH

;; A = Y ; Y++

		TYA				; Y=DISPL TO THIS STMT LENGTH
		INY				; PLUS 1 IS DISPL TO CODE

;; A = [[STMCUR]+Y]

		LDA	(STMCUR),Y		; GET CODE

;; Y++ ; STINDEX = Y

		INY				; INC TO STMT MEAT
		STY	STINDEX			; SET WORK INDEX

;; @STGO()

		JSR	@STGO			; GO EXECUTE

		.if	BASIC_REVISION > 1
		nop
		.endif

;; GOTO EXECNS

		JMP	EXECNS			; THEN DO NEXT STMT

;; def @STGO(A=TOKEN):

@STGO:

;; X = A * 2 ; GOTO ([STETAB[X]]+1)  # Run the Statement Token Handler

		ASL				; TOKEN*2
		TAX
		LDA	STETAB,X		; GET ADR AND
		PHA				; PUSH TO STACK
		LDA	STETAB+1,X		; AND GO TO
		PHA				; VIA
		RTS				; RTS

@EXEOL:

;; IF [[STMCUR]+1] & 0x80: GOTO SNX3

		LDY	#1
		LDA	(STMCUR),Y
		BMI	@EXFD			; BR IF DIR

;; GET_NEXT_LINE(MAXCIX)

		LDA	MAXCIX			; GET LINE LENGTH
		JSR	GNXTL			; INC STMCUR

;; IF TEST_END_STATEMENT_TABLE(): GOTO EXECNL  # Byte at [[STMCUR]+1] has bit 0x80 ?

		JSR	TENDST			; TEST END STMT TABLE
		BPL	EXECNL			; BR NOT END

@EXDONE:

;; GOTO XEND
		JMP	XEND			; GO BACK TO SYNTAX
@EXBRK:

;; GOTO XSTOP
		JMP	XSTOP			; BREAK, DO STOP
@EXFD:

;; GOTO SNX3
		JMP	SNX3			; GO TO SYNTAX VIA READY MSG

;---------------------------------------------------------------
; GETSTMT - Get Statement in Statement Table
;	SEARCH FOR STMT THAT HAS TSLNUM
;	SET STMCUR TO POINT TO IT IF FOUND
;	OR TO WHERE IT WOULD GO IF NOT FOUND
;	CARRY SET IF NOT FOUND
;---------------------------------------------------------------

;; def GET_STATEMENT():

GETSTMT:

;---------------------------------------------------------------
;	SAVE CURRENT LINE ADDR
;---------------------------------------------------------------

;; SAVCUR = STMCUR

		LDA	STMCUR
		STA	SAVCUR
		LDA	STMCUR+1
		STA	SAVCUR+1

;; YA = STMTAB

		LDA	STMTAB+1		; START AT TOP OF TABLE
		LDY	STMTAB

;; STMCUR = YA

		STA	STMCUR+1		; SET STMCUR
		STY	STMCUR

@GS2:

;; A = [[STMCUR]+1]

		LDY	#1
		LDA	(STMCUR),Y		; GET STMT LNO [HI]

;; IF A < TSLNUM.HI: GOTO @GS3

		CMP	TSLNUM+1		; TEST WITH TSLNUM
		BCC	@GS3			; BR IF S<TS

;; IF A > TSLNUM.HI: GOTO @GSRT1

		BNE	@GSRT1			; BR IF S>TS
		DEY				; S=TS, TEST LOW BYTE

;; A = [[STMCUR]]

		LDA	(STMCUR),Y

;; IF A < TSLNUM: GOTO @GS3

		CMP	TSLNUM
		BCC	@GS3			; BR S<TS

;; IF A > TSLNUM: RETURN FAIL

		BNE	@GSRT1			; BR S>TS

;; RETURN OK

		CLC				; S=TS, CLEAR CARRY
@GSRT1:		RTS				; AND RETURN [FOUND]

@GS3:		JSR	GETLL			; GO GET THIS GUYS LENGTH
		JSR	GNXTL
		JMP	@GS2

;; def GET_NEXT_LINE(LL):
;;   STMCUR += LL
;;   RETURN A=[STMCUR+1] # (Rev.1 YA=STMCUR)

GNXTL:		CLC
		ADC	STMCUR			; ADD LENGTH TO STMCUR
		STA	STMCUR
		.if	BASIC_REVISION = 1
		TAY
		.endif
		LDA	STMCUR+1
		ADC	#0
		STA	STMCUR+1
		RTS

;; def GETLL(): RETURN [[STMCUR]+2]

GETLL:		LDY	#2
		LDA	(STMCUR),Y
		RTS

;---------------------------------------------------------------
; TENDST - Test End of Statement Table
;---------------------------------------------------------------

;; def TEST_END_STATEMENT_TABLE():

TENDST:

;;   RETURN [[STMCUR]+Y]

		LDY	#1
		LDA	(STMCUR),Y

		.if	BASIC_REVISION = 1
		RTS
		.endif

;---------------------------------------------------------------
; XREM -- Execute REM, DATA
;---------------------------------------------------------------

XREM:
XDATA:

;; RETURN
		RTS

;---------------------------------------------------------------
; XBYE -- Execute BYE
;---------------------------------------------------------------

XBYE:

;; CLOSE_ALL() ; GOTO SELFSV  # Run Blackboard

		JSR	CLSALL
		JMP	SELFSV

;---------------------------------------------------------------
; XDOS -- Execute DOS
;---------------------------------------------------------------

XDOS:

;; CLOSE_ALL() ; GOTO [DOSVEC]  # Run DOS (or Blackboard)

		JSR	CLSALL
		JMP	(DOSVEC)

TSTBRK:

		.if	BASIC_REVISION = 1

;; IF BRKBYT == 0:
;;   BRKBYT = 0xFF
;;   RETURN ZERO_CLR
;; ELSE:
;;   RETURN ZERO_SET

		LDY	#0
		LDA	BRKBYT		; LOAD BREAK BYTE
		BNE	@TB2		; NOT ZERO, RETURN Z = TRUE
		LDY	#$FF		; IS ZERO, RETURN Z = FALSE
		STY	BRKBYT
		.else

;; # Reverse state for Rev.2+
;; IF BRKBYT == 0:
;;   BRKBYT = 0xFF
;;   RETURN ZERO_SET
;; ELSE:
;;   RETURN ZERO_CLR

		ldy	BRKBYT		; LOAD BREAK BYTE
		bne	@TBX		; NOT ZERO, RETURN Z = FALSE
		dec	BRKBYT
		.endif

@TB2:		TYA			; SET COND DONE
@TBX:		RTS			; DONE

;---------------------------------------------------------------
; Statement Execution Table
;---------------------------------------------------------------

;---------------------------------------------------------------
;	STETAB	- STATEMENT EXECUTION TABLE
;		- CONTAINS STMT EXECUTION ADR
;		- MUST BE IN SAME ORDER AS SNTAB
;---------------------------------------------------------------

STETAB:		.dbyt	XREM-1,XDATA-1,XINPUT-1,XCOLOR-1,XLIST-1,XENTER-1,XLET-1,XIF-1
		.dbyt	XFOR-1,XNEXT-1,XGOTO-1,XGOTO-1,XGOSUB-1,XTRAP-1,XBYE-1,XCONT-1
		.dbyt	XCOM-1,XCLOSE-1,XCLR-1,XDEG-1,XDIM-1,XEND-1,XNEW-1,XOPEN-1
		.dbyt	XLOAD-1,XSAVE-1,XSTATUS-1,XNOTE-1,XPOINT-1,XXIO-1,XON-1,XPOKE-1
		.dbyt	XPRINT-1,XRAD-1,XREAD-1,XREST-1,XRTN-1,XRUN-1,XSTOP-1,XPOP-1
		.dbyt	XPRINT-1,XGET-1,XPUT-1,XGR-1,XPLOT-1,XPOS-1,XDOS-1,XDRAWTO-1
		.dbyt	XSETCOLOR-1,XLOCATE-1,XSOUND-1,XLPRINT-1,XCSAVE-1,XCLOAD-1,XLET-1,XERR-1

;---------------------------------------------------------------
; Operator Execution Table
;---------------------------------------------------------------

;---------------------------------------------------------------
;	OPETAB	- OPERATOR EXECUTION TABLE
;		- CONTAINS OPERATOR EXECUTION ADR
;		- MUST BE IN SAME ORDER AS OPNTAB
;---------------------------------------------------------------

OPETAB:		.dbyt	XPLE-1,XPNE-1,XPGE-1,XPLT-1,XPGT-1,XPEQ-1,XPPOWER-1,XPMUL-1
		.dbyt	XPPLUS-1,XPMINUS-1,XPDIV-1,XPNOT-1,XPOR-1,XPAND-1,XPLPRN-1,XPRPRN-1
		.dbyt	XPAASN-1,XSAASN-1,XPSLE-1,XPSNE-1,XPSGE-1,XPSLT-1,XPSGT-1,XPEQ-1
		.dbyt	XPUPLUS-1,XPUMINUS-1,XPSLPRN-1,XPALPRN-1,XPDLPRN-1,XPFLPRN-1,XPDSLP-1,XPACOM-1
		.dbyt	XPSTR-1,XPCHR-1,XPUSR-1,XPASC-1,XPVAL-1,XPLEN-1,XPADR-1,XPATN-1
		.dbyt	XPCOS-1,XPPEEK-1,XPSIN-1,XPRND-1,XPFRE-1,XPEXP-1,XPLOG-1,XPL10-1
		.dbyt	XPSQR-1,XPSGN-1,XPABS-1,XPINT-1,XPPDL-1,XPSTICK-1,XPPTRIG-1,XPSTRIG-1

;---------------------------------------------------------------
; Execute Expression
;---------------------------------------------------------------

;---------------------------------------------------------------
; XLET -- Execute LET
;---------------------------------------------------------------

XLET:
EXEXPR:

;; EXPRESSION_INIT()

		JSR	EXPINT			; GO INIT

_EXNXT:

;; GET_TOKEN()

		JSR	GETTOKEN		; GO GET TOKEN
		BCS	@EXOT			; BR IF OPERATOR

		JSR	ARGPUSH			; PUSH ARG

		.if	BASIC_REVISION = 1
		JMP	_EXNXT			; GO FOR NEXT TOKEN
		.else
		bmi	_EXNXT
		.endif

@EXOT:		STA	EXSVOP			; SAVE OPERATOR
		TAX
		LDA	OPRTAB-16,X		; GET OP PREC
		LSR
		LSR
		LSR
		LSR
		STA	EXSVPR			; SAVE GOES ON PREC

_EXPTST:	LDY	OPSTKX			; GET OP STACK INDEX
		LDA	(ARGSTK),Y		; GET TOP OP
		TAX
		LDA	OPRTAB-16,X		; GET TOP OP PREC
		AND	#$0F
		CMP	EXSVPR			; [TOP OP]: [NEW OP]
		BCC	_EOPUSH			; IF T<N, PUSH NEW
						;	ELSE POP
		TAX				; IF POP SOE
		BEQ	_EXEND			; THEN DONE

EXOPOP:		LDA	(ARGSTK),Y		; RE-GET TOS OP
		INC	OPSTKX			; INC OP STACK INDEX
		JSR	_EXOP			; GET EXECUTE OP
		JMP	_EXPTST			; GO TEST OP WITH NEW TOS

_EOPUSH:	LDA	EXSVOP			; GET OP TO PUSH
		DEY				; DEC TO NEXT ENTRY
		STA	(ARGSTK),Y		; SET OP IN STACK
		STY	OPSTKX			; SAVE NEW OP STACK INDEX
		JMP	_EXNXT			; GO GET NEXT TOKEN

		.if	BASIC_REVISION = 1
_EXEND:
XPLPRN:		RTS				; DONE EXECUTE EXPR
		.endif

_EXOP:		SEC				; SUBTRACT FOR REL 0
		SBC	#CSROP			; VALUE OF FIRST REAL OP
		ASL				; VALUE * 2
		TAX
		LDA	OPETAB,X		; PUT OP EXECUTION
		PHA				; ROUTINE ON STACK
		LDA	OPETAB+1,X		; AND GOTO
		PHA				; VIA
		RTS				; RTS

;---------------------------------------------------------------
; Initialize Expression Parameters
;---------------------------------------------------------------

EXPINT:		LDY	#$FF
		LDA	#CSOE			; OPERATOR
		STA	(ARGSTK),Y		; STACK
		STY	OPSTKX
		INY				; AND INITIALIZE
		STY	COMCNT
		STY	ARSTKX			; ARG STACK
		STY	ADFLAG			; ASSIGN FLAG

		.if	BASIC_REVISION > 1
_EXEND:
XPLPRN:
; Unary plus
XPUPLUS:
		.endif

		RTS

;---------------------------------------------------------------
; GETTOKEN - Get Next Token and Classify
;---------------------------------------------------------------
; ON EXIT	A - The token
;		STINDEX - Advance for 1 token plus its data
;		VTYPE - For a Variable token, its type
;---------------------------------------------------------------

;; def GET_TOKEN():

GETTOKEN:

;; Y = STINDEX++ ; A = [[STMCUR]+Y] ; IF A & 0x80: GOTO GETVAR

		LDY	STINDEX			; GET STMT INDEX
		INC	STINDEX			; INC TO NEXT
		LDA	(STMCUR),Y		; GET TOKEN
		BMI	GETVAR			; BR IF VAR

;; IF A > 0x0F: RETURN
;; IF A == 0x0F: GOTO _EGSC

		CMP	#$0F			; TOKEN: $0F
		BCC	@EGNC			; BR IF $0E, NUMERIC CONST
		BEQ	_EGSC			; BR IF $0F, STR CONST
		RTS				; RTN IF OPERATOR

@EGNC:

;; def NCTOFR0():

NCTOFR0:

;; FOR N = 0 TO 5 : [FR0+N] = [[STMCUR]+Y+1+N]

		LDX	#0
@EGT1:		INY				; INC LINE INDEX
		LDA	(STMCUR),Y		; GET VALUE FROM STMT TBL
		STA	FR0,X			; AND PUT INTO FR0
		INX
		CPX	#6
		BCC	@EGT1

;; Y++ ; X = A = 0 ; GOTO _EGST

		INY				; INY Y BEYOND CONST
		LDA	#EVSCALER		; ACU=SCALER
		TAX				; X = VAR NUM 0
		BEQ	_EGST			; GO SET REM

_EGSC:

;; Y++
		INY				; INC Y TO LENGTH BYTE

;; A = [[STMCUR]+Y]

		LDA	(STMCUR),Y		; GET LENGTH
		LDX	#STMCUR			; POINT TO STMCUR

;; def RISC():

RISC:

;; [VTYPE.LEN].LO = [VTYPE.DIM].LO = A

		STA	VTYPE+EVSLEN		; SET AS LENGTH
		STA	VTYPE+EVSDIM		; AND DIM

;; Y++
		INY

;; [VTYPE.ADR].LO = [X].LO + Y

		TYA				; ACU=DISPL TO STR
		CLC
		ADC	$0,X			; DISPL PLUS ADR
		STA	VTYPE+EVSADR		; IS STR ADR

;; [VTYPE.LEN].LO = [VTYPE.DIM].LO = 0

		LDA	#0			; SET = 0
		STA	VTYPE+EVSLEN+1		; LENGTH HIGH
		STA	VTYPE+EVSDIM+1		; DIM HIGH

;; [VTYPE.ADR].HI = [X].HI

		ADC	$1,X			; FINISH ADR
		STA	VTYPE+EVSADR+1

;; Y += [VTYPE.LEN]

		TYA				; ACU=DISPL TO STR
		ADC	VTYPE+EVSLEN		; PLUS STR LENGTH
		TAY				; IS NEW INDEX

;; VNUM = 0 ; VTYPE = 0x80 + 0x02 + 0x01  # Dimmed String

		LDX	#0			; VAR NUM = 0
		LDA	#EVSTR+EVSDTA+EVDIM	; TYPE = STR

;; VTYPE = A ; VNUM = X ; STINDEX = Y

_EGST:		STA	VTYPE			; SET TYPE
		STX	VNUM			; SET NUM
		STY	STINDEX			; SET NEW INDEX

;; RETURN OK

		CLC				; INDICATE VALUE
		RTS				; RETURN

GETVAR:

;; WVVTPT = GVVTADR(A)

		JSR	GVVTADR			; GET VVT ADR

;; FOR Y = 0 TO 7: [VTYPE+Y] = [[WVVTPT]+Y]

@EGT2:		LDA	(WVVTPT),Y		; MOVE VVT ENTRY
		STA	VTYPE,Y			; TO FR0
		INY
		CPY	#8
		BCC	@EGT2


;; RETURN OK
		CLC				; INDICATE VALUE
		RTS				; RETURN

;---------------------------------------------------------------
; GSTRAD -- Get String [ABS] Address
;---------------------------------------------------------------
; ON EXIT	Y - STRING ADR LOW
;		A - STRING ADR HIGH
;		VTYPE.ADR - STRING ADR
;---------------------------------------------------------------

AAPSTR:

;; [VTYPE] = ARGPOP()

		JSR	ARGPOP

GSTRAD:

;; IF [VTYPE]: RETURN A=EVSDTA

		LDA	#EVSDTA			; LOAD TRANSFORMED BIT
		BIT	VTYPE			; TEST STRING ADR TRANSFORM
		BNE	@GSARTS			; BR IF ALREADY TRANSFORMED

;; [VTYPE] |= EVSDTA

		ORA	VTYPE			; TURN ON TRANS BIT
		STA	VTYPE			; AND SET

;; IF [VTYPE] & EVDIM == 0: GOTO @GSEND

		ROR				; SHIFT DIM BIT TO CARRY
		BCC	@GSEND

;; [VTYPE.ADR] += [STARP] ; YA = [VTYPE.ADR]

		CLC
		LDA	VTYPE+EVSADR		; STRING ADR = STRING DISPL + STARP
		ADC	STARP
		STA	VTYPE+EVSADR
		TAY
		LDA	VTYPE+EVSADR+1
		ADC	STARP+1
		STA	VTYPE+EVSADR+1

@GSARTS:

;; RETURN YA
		RTS

@GSEND:

;; ERRDIM()
		JSR	ERRDIM

;; # Push the arg in [VTYPE] to onto the [[ARGOPS]] Operator Stack
;; def ARGPUSH():

ARGPUSH:

;; A = ++ARSLVL * 8

		INC	ARSLVL			; INC ARG STK LEVEL
		LDA	ARSLVL			; GET ARG STACK LEVEL
		ASL				; ARG STACK LEVEL * 8
		ASL
		ASL

;; IF A >= [OPSTKX]: GOTO _APERR

		CMP	OPSTKX			; TEST EXCEED MAX
		BCS	_APERR			; BR IF GT MAX

;; Y = A - 1

		TAY				; Y = NEXT ENTRY ADR
		DEY				; MINUS ONE

;; FOR (X = 7 ; X >= 0 ; Y--, X--): [[ARGOPS]+Y] = [VTYPE+X]

		LDX	#7			; X = 7 FOR 8
@APH1:		LDA	VTYPE,X			; MOVE VTYPE,VNUM,FR0
		STA	(ARGOPS),Y		; TO ARG ENTRY
		DEY				; BACKWARDS
		DEX
		BPL	@APH1
_APRTS:

;; RETURN
		RTS				; DONE

_APERR:

;; ERRAOS() ; RETURN  # ARG STACK OVERFLOW
		JMP	ERRAOS			; STACK OVERFLOW

;---------------------------------------------------------------
; GETPINT -- Get Positive Integer from Expression
;---------------------------------------------------------------

;; def GET_POS_INT():

GETPINT:

;; IF GETINT().IS_NEGATIVE(): GOTO ERRLN

		JSR	GETINT			; GO GET INT
		LDA	FR0+1			; GET HIGH BYTE

		.if	BASIC_REVISION = 1
		BMI	@GPIERR			; BR > 32767
		RTS				; DONE
@GPIERR:	JMP	ERRLN
		.else
		bpl	_APRTS			; BR < 32768 DONE
		jmp	ERRLN			; JMP > 32767
		.endif

;---------------------------------------------------------------
; GETINT -- Get Integer from Expression
;---------------------------------------------------------------

;; def GETINT():

GETINT:

;; XLET()
		JSR	XLET			; EVAL EXPR

;; def POPFPI():

POPFPI:

;; [VTYPE] = ARGPOP() ; [FR0] = FP_TO_INT() ; RETURN

		JSR	ARGPOP			; POP VALUE TO FR0
		JMP	CVFPI			; GO CONVERT FR0 TO INT & RETURN

;---------------------------------------------------------------
; GET1INT - Get One-Byte Integer from Expression
;---------------------------------------------------------------

;; def GET1INT():

GET1INT:

;; IF GETPINT().HI == 0: RETURN

		JSR	GETPINT			; GET INT <32768
		BNE	@ERV1			; IF NOT 1 BYTE, THEN ERROR
		RTS

;; ERVAL()  # VALUE ERROR

@ERV1:		JSR	ERVAL			; FALL THRU TO ARGPOP

;---------------------------------------------------------------
; ARGPOP -- Pop Argument Stack Entry to FR0 or FR1
;           Each argument entry is 8 bytes long
;---------------------------------------------------------------

;; def ARGPOP():

ARGPOP:

;; Y = (ARSLVL--) * 8 - 1

		LDA	ARSLVL			; GET ARG STACK LEVEL
		DEC	ARSLVL			; DEC ARG STACK LEVEL
		ASL				; ARG STACK LEVEL * 8
		ASL
		ASL
		TAY				; Y = START OF NEXT ENTRY
		DEY				; MINUS ONE

;; FOR (X = 7 ; X >= 0 ; Y--, X--): [VTYPE+X] = [[ARGOPS]+Y]

		LDX	#7			; X = 7 FOR 8
@APOP0:		LDA	(ARGOPS),Y		; MOVE ARG ENTRY
		STA	VTYPE,X			; TO VTYPE,VNUM,FR0
		DEY				; BACKWARDS
		DEX
		BPL	@APOP0

;; RETURN
		RTS				; DONE

;---------------------------------------------------------------
; ARGP2 -- Pop TOS to FR1, TOS-1 to FR0
;---------------------------------------------------------------

;; def ARGP2():

ARGP2:

;; FR1 = ARGPOP() ; FR0 = ARGPOP() ; RETURN

		JSR	ARGPOP			; POP TOS TO FR0
		JSR	MV0TO1			; MOVE FR0 TO FR1
		JMP	ARGPOP			; POP TOS TO FR0 AND RETURN

;---------------------------------------------------------------
; POP1 -- Get a Value in FR0
;---------------------------------------------------------------
;	- EVALUATE EXPRESSION IN STMT LINE &
;	  POP IT INTO FR0
;---------------------------------------------------------------

;; def POP1():

POP1:

;; [FR0] = EXEXPR() ; RETURN

		JSR	EXEXPR			; EVALUATE EXPRESSION
		.if	BASIC_REVISION = 1
		JSR	ARGPOP			; POP INTO FR0
		RTS
		.else
		jmp	ARGPOP
		.endif

;---------------------------------------------------------------
; RTNVAR -- Return Variable to Variable Value Table from FR0
;---------------------------------------------------------------
; ON ENTRY	VNUM - DEST VAR INDEX
;		VTYPE - POPULATED VAR TYPE, ADR, LEN, DIM
;---------------------------------------------------------------

;; def RTNVAR():

RTNVAR:

;; WVVTPT = GVVTADR(VNUM)

		LDA	VNUM			; GET VAR NUMBER
		JSR	GVVTADR

;; FOR (X = 0 ; X <= 7 ; Y++, X++): [[WVVTPT]+Y] = [VTYPE+X]

		LDX	#0
@RV1:		LDA	VTYPE,X			; MOVE FR0 TO
		STA	(WVVTPT),Y		; VAR VALUE TABLE
		INY
		INX
		CPX	#8
		BCC	@RV1
;; RETURN
		RTS				; DONE

;---------------------------------------------------------------
; Get Value's Value Table Entry Address
;---------------------------------------------------------------
; ON EXIT	Y - ZERO
; 		WVVTPT - VARIABLE ADR
;---------------------------------------------------------------

;; def GVVTADR(A):
;;   WVVTPT = VVTP + A * 8
;;   RETURN

GVVTADR:	LDY	#0			; CLEAR ADR HI
		STY	WVVTPT+1
		ASL				; MULT VAR NO
		ASL				; BY 8
		ROL	WVVTPT+1
		ASL
		ROL	WVVTPT+1
		CLC				; THEN
		ADC	VVTP			; ADD VVTP VALUE
		STA	WVVTPT			; TO FORM ENTRY
		LDA	VVTP+1			; ADR
		ADC	WVVTPT+1
		STA	WVVTPT+1
		RTS

;---------------------------------------------------------------
; Operator Precedence Table
;---------------------------------------------------------------

NTOK		.set	$0E

		NextToken	CBCD		; $0E = BCD Number (followed by 6 bytes)
		NextToken	CSTOK		; $0F = String (followed by Length)

OPRTAB:		CommandPrec $00, CDQ		; "      Double-Quote (UNUSED?)
		CommandPrec $00, CSOE		;        Expression Stack Marker
		CommandPrec $00, CCOM		; ,      in PRINT statement
		CommandPrec $00, CDOL		; $      Dollar Sign
		CommandPrec $00, CEOS		; :      End of expr / statement
		CommandPrec $00, CSC		; ;      in PRINT statement
		CommandPrec $00, CCR		;        End of expr / line
		CommandPrec $00, CGTO		; GOTO   ON...GOTO
		CommandPrec $00, CGS		; GOSUB  ON...GOSUB
		CommandPrec $00, CTO		; TO     FOR ... TO
		CommandPrec $00, CSTEP		; STEP   FOR ... TO ... STEP
		CommandPrec $00, CTHEN		; THEN   IF...THEN
		CommandPrec $00, CPND		; #      OPEN #

		SetToken	CSROP		; First "real" operator

		CommandPrec $88, CLE		; <=     IF A<=B
		CommandPrec $88, CNE		; <>     IF A<>B
		CommandPrec $88, CGE		; >=     IF A<=B
		CommandPrec $88, CGT		; <      IF A<B
		CommandPrec $88, CLT		; >      IF A>B
		CommandPrec $88, CEQ		; =      IF A=B
		CommandPrec $CC, CEXP		; ^      A=B^C   (Up Arrow $5E+$80)
		CommandPrec $AA, CMUL		; *      A=B*C
		CommandPrec $99, CPLUS		; +      A=B+C
		CommandPrec $99, CMINUS		; -      A=B-C
		CommandPrec $AA, CDIV		; /      A=B/C
		.if	BASIC_REVISION = 1
		CommandPrec $77, CNOT		; NOT    Boolean logic
		.else
		CommandPrec $DD, CNOT		; NOT    Boolean logic
		.endif
		CommandPrec $55, COR		; OR
		CommandPrec $66, CAND		; AND
		CommandPrec $F2, CLPRN		; (      ( math
		CommandPrec $4E, CRPRN		; )      close for all parens
		CommandPrec $F1, CAASN		; =      S1=S2
		CommandPrec $F1, CSASN		; =      N$=A$
		CommandPrec $EE, CSLE		; <=     cmp A$<=B$
		CommandPrec $EE, CSNE		; <>     cmp A$<>B$
		CommandPrec $EE, CSGE		; >=     cmp A$>=B$
		CommandPrec $EE, CSGT		; >      cmp A$>B$
		CommandPrec $EE, CSLT		; <      cmp A$<B$
		CommandPrec $EE, CSEQ		; =      cmp A$=B$
		CommandPrec $DD, CUPLUS		; +      POSITIVE
		CommandPrec $DD, CUMINUS	; -      NEGATIVE
		CommandPrec $F2, CSLPRN		; (      A$(
		CommandPrec $F2, CALPRN		; (      SC(
		CommandPrec $F2, CDLPRN		; (      DIM SC(
		CommandPrec $F2, CFLPRN		; (      CHR$(, PEEK(
		CommandPrec $F2, CDSLPR		; (      DIM S$(
		CommandPrec $43, CACOM		; ,      Array Subscript Separator

				; FUNCTIONS
		SetToken	CFFUN		; FIRST FUNCTION CODE
		CommandPrec $F2, CSTR		; STR$()
		CommandPrec $F2, CCHR		; CHR$()
		CommandPrec $F2, CUSR		; USR()
		CommandPrec $F2, CASC		; ASC()
		CommandPrec $F2, CVAL		; VAL()
		CommandPrec $F2, CLEN		; LEN()
		CommandPrec $F2, CADR		; ADR()

		SetToken	CNFNP		; Numeric Functions
		CommandPrec $F2, CATN		; ATN()
		CommandPrec $F2 ; CCOS		; COS()
		CommandPrec $F2 ; CPEEK		; PEEK()
		CommandPrec $F2 ; CSIN		; SIN()
		CommandPrec $F2 ; CRND		; RND()
		CommandPrec $F2 ; CFRE		; FRE()
		CommandPrec $F2 ; CFEXP		; EXP()
		CommandPrec $F2 ; CLOG		; LOG()
		CommandPrec $F2 ; CCLOG		; CLOG()
		CommandPrec $F2 ; CSQR		; SQR()
		CommandPrec $F2 ; CSGN		; SGN()

		.byte $F2,$F2,$F2,$F2,$F2,$F2	; ABS(), INT(), PADDLE(), STICK(), PTRIG(), STRIG()

;---------------------------------------------------------------
; Miscellaneous Operators' Executors
;---------------------------------------------------------------

;---------------------------------------------------------------
; Math Operator +
;---------------------------------------------------------------

;; def XPPLUS(): ARGP2() ; FRADD() ; ARGPUSH([VTYPE]) ; RETURN

		.if	BASIC_REVISION = 1
XPPLUS:		JSR	ARGP2
		JSR	FRADD
		JMP	ARGPUSH
		.endif

;---------------------------------------------------------------
; Math Operator -
;---------------------------------------------------------------

;; def XPMINUS(): ARGP2() ; FRSUB() ; ARGPUSH([VTYPE]) ; RETURN

XPMINUS:	JSR	ARGP2
		JSR	FRSUB
		JMP	ARGPUSH

;---------------------------------------------------------------
; Operator *
;---------------------------------------------------------------

;; def XPMUL(): ARGP2() ; FRMUL() ; ARGPUSH([VTYPE]) ; RETURN

XPMUL:		JSR	ARGP2
		JSR	FRMUL
		JMP	ARGPUSH

;---------------------------------------------------------------
; Operator /
;---------------------------------------------------------------

;; def XPDIV(): ARGP2() ; FRDIV() ; ARGPUSH([VTYPE]) ; RETURN

XPDIV:		JSR	ARGP2
		JSR	FRDIV
		JMP	ARGPUSH

;---------------------------------------------------------------
; Unary minus
;---------------------------------------------------------------

;; def XPUMINUS(): ARGPOP() ; [FR0] = -[FR0] ; ARGPUSH([VTYPE])

XPUMINUS:	JSR	ARGPOP			; GET ARGUMENT INTO FR0

		LDA	FR0			; GET BYTE WITH SIGN

		.if	BASIC_REVISION > 1
		beq	@XPM1			; Avoid Negative Zero
		.endif

		EOR	#$80			; FLIP SIGN BIT
		STA	FR0			; RETURN BYTE WITH SIGN CHANGED
@XPM1:		JMP	ARGPUSH			; PUSH ON STACKS

;---------------------------------------------------------------
; Unary plus
;---------------------------------------------------------------

;; def XPUPLUS(): RETURN

		.if	BASIC_REVISION = 1
XPUPLUS:	RTS
		.endif

;---------------------------------------------------------------
; Comparison Operator <=
;---------------------------------------------------------------

;; def XPLE(): RETURN [FR0] <= [FR1]

XPLE:
XPSLE:		JSR	XCMP
		BMI	XTRUE
		BEQ	XTRUE
		BPL	XFALSE

;---------------------------------------------------------------
; Comparison Operator <>
;---------------------------------------------------------------

;; def XPNE(): RETURN [FR0] != [FR1]

XPNE:
XPSNE:		JSR	XCMP
		.if	BASIC_REVISION = 1
		BEQ	XFALSE
		BNE	XTRUE
		.else
		jmp	_TFNEAR
		.endif

;---------------------------------------------------------------
; Comparison Operator <
;---------------------------------------------------------------

;; def XPLT(): RETURN [FR0] < [FR1]

XPLT:
XPSLT:		JSR	XCMP
		BMI	XTRUE
		BPL	XFALSE

;---------------------------------------------------------------
; Comparison Operator >
;---------------------------------------------------------------

;; def XPGT(): RETURN [FR0] > [FR1]

XPGT:
XPSGT:		JSR	XCMP
		BMI	XFALSE
		BEQ	XFALSE
		BPL	XTRUE

;---------------------------------------------------------------
; Comparison Operator >=
;---------------------------------------------------------------

;; def XPGE(): RETURN [FR0] >= [FR1]

XPGE:
XPSGE:		JSR	XCMP
		BMI	XFALSE
		BPL	XTRUE

;---------------------------------------------------------------
; Comparison Operator =
;---------------------------------------------------------------

;; def XPEQ(): RETURN [FR0] == [FR1]

XPEQ:		JSR	XCMP
		.if	BASIC_REVISION = 1
		BEQ	XTRUE
		BNE	XFALSE
		.else
		jmp	_TFNEAR2
		.endif

;---------------------------------------------------------------
; Logical AND
;---------------------------------------------------------------

;; def XPAND(): IF [FR0] AND [FR1]: GOTO XTRUE ; ELSE: GOTO XFALSE

XPAND:		JSR	ARGP2
		LDA	FR0
		AND	FR1

		.if	BASIC_REVISION = 1
		BEQ	XFALSE
		BNE	XTRUE
		.else
		jmp	_TFNEAR
		.endif

;---------------------------------------------------------------
; Logical OR
;---------------------------------------------------------------

;; def XPOR(): IF [FR0] OR [FR1]: GOTO XTRUE ; ELSE: GOTO XFALSE

XPOR:		JSR	ARGP2
		LDA	FR0
		ORA	FR1

		; 0 FALSE, else TRUE
_TFNEAR:	BEQ	XFALSE
		BNE	XTRUE

;---------------------------------------------------------------
; Logical NOT
;---------------------------------------------------------------

;; def XPNOT(): IF [FR0]: GOTO XFALSE ; ELSE: GOTO XTRUE

XPNOT:		JSR	ARGPOP
		LDA	FR0

		; 0 FALSE, else TRUE
_TFNEAR2:	BEQ	XTRUE
		; FALL THROUGH TO XFALSE

XFALSE:

;; A = Y = 0
		LDA	#0
		TAY
		BEQ	XTF

XTRUE:

;; A = 0x40  # FR0 = +

		LDA	#$40

XTI:

;; Y = 1  # FR0 = +-1

		LDY	#1

XTF:

;; [FR0] = { A, Y, 0, 0, 0, 0 } ; A = 0

		STA	FR0
		STY	FR0+1
		LDX	#FR0+2			; POINT TO PART TO CLEAR
		LDY	#4			; GET # OF BYTES TO CLEAR
		JSR	ZXLY			; CLEAR REST OF FR0

;; VTYPE = 0
		STA	VTYPE

XPUSH:

;; ARGPUSH([VTYPE]) ; RETURN

		JMP	ARGPUSH

;---------------------------------------------------------------
; XPSGN -- Sign Function
;---------------------------------------------------------------

;; def XPSGN():
;;   IF [FR0] == 0: { ARGPUSH([VTYPE]) ; RETURN }
;;   IF [FR0] > 0: GOTO XTRUE

XPSGN:		JSR	ARGPOP
		LDA	FR0
		BEQ	XPUSH
		BPL	XTRUE

;; A = 0xC0 ; GOTO XTI  # FR0 = -1

		LDA	#$C0			; GET MINUS EXPONENT
		BMI	XTI

;---------------------------------------------------------------
; XCMP -- Compare Executor
;---------------------------------------------------------------

XCMP:

;; Y = [OPSTKX] - 1 ; A = [[ARGSTK]+Y]
;; GOTO A < CSLE ? FRCMPP :  STRCMP

		LDY	OPSTKX			; GET OPERATOR THAT
		DEY				; GOT US HERE
		LDA	(ARGSTK),Y
		CMP	#CSLE			; IF OP WAS ARITHMETIC
		BCC	FRCMPP			; THEN DO FP REG COMP
		JMP	STRCMP			; ELSE DO STRING COMPARE

;---------------------------------------------------------------
; FRCMP -- Compare Two Floating Point Numbers
;---------------------------------------------------------------
;	ON ENTRY	FR0 & FR1 CONTAIN FLOATING POINT #'S
;---------------------------------------------------------------
;	ON EXIT		CC = + FR0 > FR1
;			CC = - FR0 < FR1
;			CC = 0 FR0 = FR1
;---------------------------------------------------------------

;; def FRCMPP():

FRCMPP:

;; ARGP2()
		JSR	ARGP2

FRCMP:

;; FRSUB()
		JSR	FRSUB			; SUBTRACT FR1 FROM FR0

;; RETURN [FR0]
		LDA	FR0			; GET FR0 EXPONENT
		RTS				; RETURN WITH CC SET

;---------------------------------------------------------------
; FRADD -- Floating Point Add
;	DOES NOT RETURN IF ERROR
;---------------------------------------------------------------

FRADD:

;; [FR0] += [FR1]  # Error on overflow

		JSR	FADD			; ADD TWO #
		BCS	_ERROV			; BR IF ERROR
		RTS

;---------------------------------------------------------------
; FRSUB -- Floating Point Subtract
;	DOES NOT RETURN IF ERROR
;---------------------------------------------------------------

FRSUB:

;; [FR0] -= [FR1]  # Error on underflow

		JSR	FSUB			; SUB TWO #
		BCS	_ERROV			; BR IF ERROR
		RTS

;---------------------------------------------------------------
; FRMUL -- Floating Point Multiply
;	DOES NOT RETURN IF ERROR
;---------------------------------------------------------------

FRMUL:

;; [FR0] *= [FR1]  # Error on over/underflow

		JSR	FMUL			; MULT TWO #
		BCS	_ERROV			; BR IF ERROR
		RTS

;---------------------------------------------------------------
; FRDIV -- Floating Point Divide
;	DOES NOT RETURN IF ERROR
;---------------------------------------------------------------

FRDIV:

;; [FR0] /= [FR1]  # Error on over/underflow

		JSR	FDIV			; DIVIDE TWO #
		BCS	_ERROV			; BR IF ERROR
		RTS

_ERROV:		JSR	EROVFL

;---------------------------------------------------------------
; CVFPI -- Convert Floating Point to Integer
;	DOES NOT RETURN IF ERROR
;---------------------------------------------------------------

CVFPI:

;; [FR0] = FPI()  # Error on bad value

		JSR	FPI			; GO CONVERT TO INTEGER
		BCS	@ERRVAL			; IF ERROR, BR
		RTS				; ELSE RETURN
@ERRVAL:

;; ERVAL()  # VALUE ERROR / Out of Range

		JSR	ERVAL			; VALUE ERROR

;---------------------------------------------------------------
; XPAASN -- Arithmetic Assignment Operator
;---------------------------------------------------------------

XPAASN:

;; IF [OPSTKX] != 0xFF: { [ADFLAG] = 0x80 ; RETURN }

		LDA	OPSTKX			; GET OP STACK INDEX
		CMP	#$FF			; AT STACK START
		BNE	@AAMAT			; BR IF NOT, [MAT ASSIGN]
						;	DO SCALER ASSIGN

;; [FR0] = [FR1] = ARGP2()

		JSR	ARGP2			; GO POP TO P2 ARGS
		LDX	#5			; MOVE FR1 VALUE
@AASN1:		LDA	FR1,X			; TO FR0
		STA	FR0,X
		DEX
		BPL	@AASN1

;; GOTO RTNVAR

		JMP	RTNVAR			; FR0 TO VVT & RETURN

@AAMAT:

;; [ADFLAG] = 0x80 ; RETURN

		LDA	#$80			; SET ASSIGN FLAG BIT ON
		STA	ADFLAG			; IN ASSIGN/DIM FLAG
		RTS				; GO POP REM OFF OPS

;---------------------------------------------------------------
; XPACOM -- Array Comma Operator
;---------------------------------------------------------------

;; def XPACOM(): [COMCNT]++

XPACOM:		INC	COMCNT			; INCREMENT COMMA COUNT

;---------------------------------------------------------------
; XPRPRN -- Right Parenthesis Operator
; XPFLPRN - FUNCTION LEFT PAREN OPERATOR
;---------------------------------------------------------------

;; def XPRPRN(): Y = [OPSTKX] ; POP() ; POP() ; _EXOPOP() ; RETURN

XPRPRN:

;; def XPFLPRN(): Y = [OPSTKX] ; POP() ; POP() ; _EXOPOP() ; RETURN

XPFLPRN:	LDY	OPSTKX			; GET OPERATOR STACK TOP
		PLA
		PLA
		JMP	EXOPOP			; GO POP AND EXECUTE NEXT OPERATOR

;---------------------------------------------------------------
; XPDLPRN -- DIM Left Parenthesis Operator
;---------------------------------------------------------------

;; def XPDSLP(): [ADFLAG] = 0x40 ; XPALPRN() ; RETURN

XPDSLP:

;; def XPDLPRN(): [ADFLAG] = 0x40 ; XPALPRN() ; RETURN

XPDLPRN:	LDA	#$40			; SET DIM FLAG BIT
		STA	ADFLAG			; IN ADFLAG
						;	FALL THRU TO XPALPRN

;---------------------------------------------------------------
; XPALPRN -- Array Left Parenthesis Operator
;---------------------------------------------------------------

XPALPRN:

;; IF [ADFLAG] & 0x80: [ATEMP] = [ARSLVL]--

		BIT	ADFLAG			; IF NOT ASSIGN
		BPL	@ALP1			; THEN BRANCH
						;     ELSE
		LDA	ARSLVL			; SAVE STACK LEVEL
		STA	ATEMP			; OF THE VALUE ASSIGNMENT
		DEC	ARSLVL			; AND PSEUDO POP IT

@ALP1:

;; Y = A = 0 ; IF [COMCNT] == 0: GOTO @ALP2

		LDA	#0			; INIT FOR I2 = 0
		TAY
		CMP	COMCNT			; IF COMMA COUNT = 0 THEN
		BEQ	@ALP2			; BR WITH I2 = 0
						;     ELSE

;; [COMCNT]--

		DEC	COMCNT

;; [FR0] = POPFPI() ; IF [FR0] > 32767: GOTO @ALPER

		JSR	POPFPI			;  ELSE POP I2 AND MAKE INT
		LDA	FR0+1
		BMI	@ALPER			; ERROR IF > 32,767

;; [INDEX2] = [FR0]

		LDY	FR0
@ALP2:		STA	INDEX2+1		; SET I2 VALUE
		STY	INDEX2

;; [FR0] = POPFPI()
;; IF [FR0] > 32767: GOTO @ALPER

		JSR	POPFPI			; POP I2 AND MAKE INT
		LDA	FR0			; MOVE I1
		STA	ZTEMP1			; TO ZTEMP1
		LDA	FR0+1
		BMI	@ALPER			; ERROR IF > 32,767

;; [ZTEMP1] = [FR0]

		STA	ZTEMP1+1
		JSR	ARGPOP			; POP THE ARRAY ENTRY

;; IF [ADFLAG] & 0x40: { [ADFLAG] = 0 ; RETURN }

		BIT	ADFLAG			; IF NOT EXECUTING DIM
		BVC	@ALP3			; THEN CONTINUE
		LDA	#0			; TURN OFF DIM BIT
		STA	ADFLAG			; IN ADFLAG
		RTS				; AND RETURN

@ALP3:

;; IF [VTYPE] & 0x01 == 0: ERRDIM()

		ROR	VTYPE			; IF ARRAY HAS BEEN
		BCS	@ALP4			; DIMED THEN CONTINUE
@ALPER:		JSR	ERRDIM			; ELSE DIM ERROR

@ALP4:

;; IF [ZTEMP1] >= [VTYPE+EVAD1]: GOTO @ALPER

		LDA	ZTEMP1+1		; TEST INDEX 1
		CMP	VTYPE+EVAD1+1		; IN RANGE WITH
		BCC	@ALP5			; DIM1
		BNE	@ALPER
		LDA	ZTEMP1
		CMP	VTYPE+EVAD1
		BCS	@ALPER

@ALP5:

;; IF [INDEX2] >= [VTYPE+EVAD2]: GOTO @ALPER

		LDA	INDEX2+1		; TEST INDEX 2
		CMP	VTYPE+EVAD2+1		; IN RANGE WITH
		BCC	@ALP6			; DIM2
		BNE	@ALPER
		LDA	INDEX2
		CMP	VTYPE+EVAD2
		BCS	@ALPER

@ALP6:

;; AMUL1()

		JSR	AMUL1			; INDEX1 = INDEX1

;; [INDEX1] += INDEX2

		LDA	INDEX2			; INDEX1 = INDEX1 + INDEX2
		LDY	INDEX2+1
		JSR	AADD

;; [ZTEMP1] *= 6

		JSR	AMUL2			; ZTEMP1 = ZTEMP1 * 6

;; [ZTEMP1] += FR0

		LDA	FR0			; ZTEMP1 = ZTEMP1 + DISPL
		LDY	FR0+1
		JSR	AADD			; ZTEMP1 = ZTEMP1 + ADR

;; [ZTEMP1] += STARP

		LDA	STARP
		LDY	STARP+1
		JSR	AADD
						; ZTEMP1 NOW POINTS
						; TO ELEMENT REQD
;; IF [ADFLAG] & 0x80 == 0: GOTO @ALP8


		BIT	ADFLAG			; IF NOT ASSIGN
		BPL	@ALP8			; THEN CONTINUE
						;     ELSE ASSIGN

;; [ARSLVL] = [ATEMP]

		LDA	ATEMP			; RESTORE ARG LEVEL
		STA	ARSLVL			; TO VALUE AND

;; ARGPOP()

		JSR	ARGPOP			; POP VALUE

;; [[ZTEMP1]] = FR0

		LDY	#5
@ALP7:		LDA	FR0,Y			; MOVE VALUE
		STA	(ZTEMP1),Y		; TO ELEMENT SPACE
		DEY
		BPL	@ALP7

;; [ADFLAG] = 0

		INY				; TURN OFF
		STY	ADFLAG			; ADFLAG

;; RETURN

		RTS				; DONE

@ALP8:

;; [FR0] = [[ZTEMP1]]

		LDY	#5			; MOVE ELEMENT TO
@ALP9:		LDA	(ZTEMP1),Y		; FR0
		STA	FR0,Y
		DEY
		BPL	@ALP9

;; [VTYPE] = 0

		INY
		STY	VTYPE

;; GOTO ARGPUSH

		JMP	ARGPUSH			; PUSH FR0 BACK TO STACK
						;     AND RETURN

;---------------------------------------------------------------
; XPSLPRN -- String Left Parenthesis
; ON ENTRY	COMCNT - COMMA COUNT
;---------------------------------------------------------------

XPSLPRN:

;; IF [COMCNT] == 0: GOTO @XSLP2

		LDA	COMCNT			; IF NO INDEX 2
		BEQ	@XSLP2			; THEN BR

;; AY = @XSPV() ; [INDEX2] = AY

		JSR	@XSPV			; ELSE POP I2 AND
		STY	INDEX2+1		; SAVE IN INDEX 2
		STA	INDEX2

@XSLP2:

;; [ZTEMP1] = @XSPV() - 1

		JSR	@XSPV			; POP INDEX 1
		SEC				; ADD DECREMENT BY ONE
		SBC	#1			; AND PUT INTO ZTEMP1
		STA	ZTEMP1
		TYA
		SBC	#0
		STA	ZTEMP1+1

;; [VTYPE] = ARGPOP()

		JSR	ARGPOP			; POP ARG STRING

;; IF [ADFLAG] & 0x80 == 0: GOTO @XSLP3

		LDA	ADFLAG			; IF NOT A DEST STRING
		BPL	@XSLP3			; THEN BRANCH

;; [ADFLAG] |= COMCNT

		ORA	COMCNT
		STA	ADFLAG

;; AY = [VTYPE.DIM] ; GOTO @XSLP4

		LDY	VTYPE+EVSDIM+1		; INDEX 2 LIMIT
		LDA	VTYPE+EVSDIM		; IS DIM
		JMP	@XSLP4

@XSLP3:

;; AY = [VTYPE.LEN]

		LDA	VTYPE+EVSLEN		; INDEX 2 LIMIT
		LDY	VTYPE+EVSLEN+1		; IS STRING LENGTH

@XSLP4:

;; X = [COMCNT] ; IF X == 0: GOTO @XSLP6

		LDX	COMCNT			; IF NO INDEX 2
		BEQ	@XSLP6			; THEN BRANCH

;; [COMCNT]--

		DEC	COMCNT			; ELSE

;; IF INDEX2 > AY: GOTO @XSLER  # STRING TOO LONG

		CPY	INDEX2+1
		BCC	@XSLER
		BNE	@XSLP5			; INDEX 2 LIMIT
		CMP	INDEX2
		BCC	@XSLER

;; AY = [INDEX2]

@XSLP5:		LDY	INDEX2+1		; USE INDEX 2
		LDA	INDEX2			; AS LIMIT

@XSLP6:

;; [VTYPE.LEN] = AY - [ZTEMP1]
;; IF AY > [ZTEMP1]: GOTO @XSLER  # STRING LENGTH ERROR

		SEC				; LENGTH IS
		SBC	ZTEMP1
		STA	VTYPE+EVSLEN		; LIMIT - INDEX 1
		TAX
		TYA
		SBC	ZTEMP1+1
		STA	VTYPE+EVSLEN+1
		BCC	@XSLER			; LENGTH MUST BE
		TAY				; GE ZERO
		BNE	@XSLP7
		TXA
		BEQ	@XSLER

@XSLP7:

;; YA = GET_STRING_ABS_ADR()

		JSR	GSTRAD			; GET ABS ADR

;; [VTYPE.ADR] += [ZTEMP1]

		CLC
		LDA	VTYPE+EVSADR
		ADC	ZTEMP1			; STRING ADR
		STA	VTYPE+EVSADR		; STRING ADR + INDEX 1
		LDA	VTYPE+EVSADR+1
		ADC	ZTEMP1+1
		STA	VTYPE+EVSADR+1

;; IF [ADFLAG] & 0x80 == 0: { ARGPUSH([VTYPE]) ; RETURN }

		BIT	ADFLAG			; IF NOT ASSIGN
		BPL	@XSLP8			; THEN BR

;; RETURN
		RTS				; ELSE RETURN TO ASSIGN

@XSLP8:

;; ARGPUSH([VTYPE]) ; RETURN

		JMP	ARGPUSH

@XSLER:

;; ERRSSL()  # STRING LENGTH ERROR

		JSR	ERRSSL

;---------------------------------------------------------------
; XSPV -- Pop Index Value as Integer and Ensure Not Zero
;---------------------------------------------------------------

;; def @XSPV():

@XSPV:

;;   AY = [FR0] = POPFPI()
;;   IF AY == 0: GOTO @XSLER

		JSR	POPFPI			; GO GET THE INTEGER
		LDA	FR0			; GET VALUE LOW
		LDY	FR0+1			; GET VALUE HI
		BNE	@XSPVR			; RTN IF VH NOT ZERO
		TAX				; TEST VL
		BEQ	@XSLER			; BR VL,VH = 0

;; RETURN

@XSPVR:		RTS				; DONE

;---------------------------------------------------------------
; XSAASN -- String Assign Operator
;---------------------------------------------------------------

;; def XSAASN():

XSAASN:

;; YA = AAPSTR()

		JSR	AAPSTR			; POP STR WITH ABS ADR

;; def RISASN():

RISASN:

;; [MVFA] = [VTYPE.ADR]

		LDA	VTYPE+EVSADR
		STA	MVFA			; MVFA = ADR
		LDA	VTYPE+EVSADR+1
		STA	MVFA+1

;; [MVLNG] = [VTYPE.LEN]

		LDA	VTYPE+EVSLEN
		STA	MVLNG			; MVLNG = LENGTH
		LDY	VTYPE+EVSLEN+1
		STY	MVLNG+1


;; IF [OPSTKX] == 0xFF: GOTO @XSA1

		LDY	OPSTKX			; IF AT TOP OF
		CPY	#$FF			; OP STACK
		BEQ	@XSA1			; THEN BR
						;     ELSE

;; [ADFLAG] = 0x80: _EXOPOP()

		LDA	#$80			; SET ASSIGN BIT
		STA	ADFLAG			; IN ASSIGN/DIM FLAG
		JSR	EXOPOP			; AND PROCESS SUBSTRING

;; YA = [VTYPE.LEN]

		LDA	VTYPE+EVSLEN+1		; A,Y =
		LDY	VTYPE+EVSLEN		; DEST LEN
		ROL	ADFLAG			; TURN OFF ASSIGN
		BCS	@XSA2A			; AND BR

@XSA1:

;; AAPSTR()

		JSR	AAPSTR			; POP STR WITH ABS ADR

;; YA = [VTYPE.DIM]

		LDA	VTYPE+EVSDIM+1		; A,Y = DEST LENGTH
		LDY	VTYPE+EVSDIM

@XSA2A:

;; IF YA < [MVLNG]: [MVLNG] = YA  # MIN

		CMP	MVLNG+1			; IF DEST LENGTH
		BCC	@XSA3			; LESS THAN MOVE LENGTH
		BNE	@XSA4
		CPY	MVLNG			; THEN
		BCS	@XSA4
@XSA3:		STA	MVLNG+1			; SET MOVE LENGTH
		STY	MVLNG			; = DEST LENGTH

@XSA4:

;; YX = [VTYPE.ADR] + [MVLNG]

		CLC
		LDA	VTYPE+EVSADR		; MOVE LENGTH PLUS
		ADC	MVLNG			; START ADR IS
		TAY				; END ADR
		LDA	VTYPE+EVSADR+1
		ADC	MVLNG+1
		TAX

;; [ZTEMP3] = YX - [STARP]

		SEC				; END ADR MINUS
		TYA				; START OF STRING
		SBC	STARP			; SPACE IS DISPL
		STA	ZTEMP3			; TO END OF STRING
		TXA				; WHICH WE SAVE
		SBC	STARP+1			; IN ZTEMP3
		STA	ZTEMP3+1

;; [MVLNG] = 0x100 - [MVLNG]

		SEC				; SET MOVE LENGTH LOW
		LDA	#0			; = $100 - MVL [L]
		SBC	MVLNG			; BECAUSE OF THE WAY
		STA	MVLNG			; FMOVE WORKS

;; [MVFA] -= [MVLNG]

		SEC				; ADJUST MVFA TO
		LDA	MVFA			; CONFORM WITH MVL
		SBC	MVLNG			; CHANGE
		STA	MVFA
		LDA	MVFA+1
		SBC	#0
		STA	MVFA+1

;; [MVTA] = [VTYPE.ADR] - [MVLNG]

		SEC
		LDA	VTYPE+EVSADR		; MOVE THE DEST
		SBC	MVLNG			; STRING ADR TO
		STA	MVTA			; MVTA AND
		LDA	VTYPE+EVSADR+1		; MAKE IT CONFORM
		SBC	#0			; WITH MVL
		STA	MVTA+1

;; FMOVER()

		JSR	FMOVER			; GO DO THE VERY FAST MOVE

;; VTYPE = GETVAR([VNUM])

		LDA	VNUM			; GO GET ORIGINAL DEST
		JSR	GETVAR			; STRING

;; YX = [ZTEMP3] - [VTYPE.ADR]

		SEC				; DISPL TO END OF
		LDA	ZTEMP3			; MOVE MINUS DISPL
		SBC	VTYPE+EVSADR		; TO START OF STRING
		TAY				; IS OUR RESULT LENGTH
		LDA	ZTEMP3+1
		SBC	VTYPE+EVSADR+1
		TAX

;; IF [ADFLAG] & 0x02 != 0:
;;   [ADFLAG] = 0

		LDA	#2			; IF THE DESTINATION
		AND	ADFLAG			; LENGTH WAS IMPLICIT
		BEQ	@XSA5			; SET NEW LENGTH
		LDA	#0			; CLEAR
		STA	ADFLAG			; FLAG
						;     ELSE FOR EXPLICIT LENGTH

;; IF YX > [VTYPE.LEN]:
;;   [VTYPE.LEN] = YX  # MAX
;;   RTNVAR() ; RETURN
;; ELSE:
;;   RETURN

		CPX	VTYPE+EVSLEN+1		; IF NEW LENGTH
		BCC	@XSA6			; GREATER THAN
		BNE	@XSA5			; OLD LENGTH THEN
		CPY	VTYPE+EVSLEN		; SET NEW LENGTH
		BCS	@XSA5			; ELSE DO NOTHING
@XSA6:		RTS

@XSA5:		STY	VTYPE+EVSLEN
		STX	VTYPE+EVSLEN+1
		JMP	RTNVAR

;---------------------------------------------------------------
; AMUL2 -- Integer Multiplication of ZTEMP1 by 6
; ON EXIT	ZTEMP1 = ZTEMP1 * 6
;---------------------------------------------------------------

;; def AMUL2(): [ZTEMP1] *= 6

AMUL2:

;; # ZTEMP1 <<= 1

		ASL	ZTEMP1			; ZTEMP1 = ZTEMP1*2
		ROL	ZTEMP1+1

;; # AY = ZTEMP1

		LDY	ZTEMP1+1		; SAVE ZTEMP1*2 IN [A,Y]
		LDA	ZTEMP1

;; # ZTEMP1 <<= 1

		ASL	ZTEMP1			; ZTEMP1 = ZTEMP1*4
		ROL	ZTEMP1+1

;---------------------------------------------------------------
; AADD -- Integer Addition of [A,Y] to ZTEMP1
; ON EXIT	ZTEMP1 += AY
;---------------------------------------------------------------

;; def AADD(): [ZTEMP1] += AY

AADD:		CLC
		ADC	ZTEMP1			; ADD LOW ORDER
		STA	ZTEMP1
		TYA
		ADC	ZTEMP1+1		; ADD HIGH ORDER
		STA	ZTEMP1+1
		RTS				; DONE

;---------------------------------------------------------------
; AMUL -- Integer Multiplication of ZTEMP1 by DIM2
;---------------------------------------------------------------

AMUL1:

;; [ZTEMP4] = 0

		LDA	#0			; CLEAR PARTIAL PRODUCT
		STA	ZTEMP4
		STA	ZTEMP4+1

;; FOR (Y = 16 ; Y > 0 ; Y--):

		LDY	#$10			; SET FOR 16 BITS
@AM1:		LDA	ZTEMP1			; GET MULTIPLICAN
		LSR				; TEST MSB = ON
		BCC	@AM3			; BR IF OFF

;; [ZTEMP4] += [VTYPE+EVAD2]  # 16 Bits, FOR (X = -2 ; X <= -1 ; X++)

		CLC
		LDX	#$FE			; ADD MULTIPLIER
@AM2:		LDA	ZTEMP4+2,X		; TO PARTIAL PRODUCT
		ADC	VTYPE+EVAD2+2,X
		STA	ZTEMP4+2,X
		INX
		BNE	@AM2

;; [ZTEMP1].LONG >>= 1

@AM3:		LDX	#3			; MULT PRODUCT BY 2
@AM4:		ROR	ZTEMP1,X
		DEX
		BPL	@AM4

;; :NEXT Y
		DEY				; TEST MORE BITS
		BNE	@AM1			; BR IF MORE

;; RETURN
		RTS				; DONE

;---------------------------------------------------------------
; STRCMP -- String Compare
;---------------------------------------------------------------

STRCMP:		JSR	AAPSTR			; POP STRING WITH ABS ADR
		JSR	MV0TO1			; MOVE B TO FR1

;; YA = AAPSTR()

		JSR	AAPSTR			; POP STRING WITH ABS ADR
@SC1:

;; SAVED_P = DEC_WORD(VTYPE+EVSLEN)

		LDX	#VTYPE+EVSLEN		; GO DEC STR A LEN
		JSR	ZPADEC
		PHP				; SAVE RTN CODE

;; IF DEC_WORD(VTYPE1+EVSLEN) == 0: GOTO @SC2

		LDX	#VTYPE1+EVSLEN		; GO DEC STR B LEN
		JSR	ZPADEC
		BEQ	@SC2			; BR STR B LEN = 0

;; RESTORE_RESULT(SAVED_P)

		PLP				; GET STR A COND CODE

;; IF SAVED_P.ZERO_FLAG: GOTO @SCLT

		BEQ	@SCLT			; BR STR A LEN = 0

;; IF [VTYPE.ADR] == [VTYPE1+EVSADR]: GOTO @SC3

		LDY	#0			; COMPARE A BYTE
		LDA	(VTYPE+EVSADR),Y	; OF STRING A
		CMP	(VTYPE1+EVSADR),Y	; TO STRING B
		BEQ	@SC3			; BR IF SAME

;; IF [VTYPE.ADR] < [VTYPE1+EVSADR]: GOTO @SCLT

		BCC	@SCLT			; BE IF A<B

@SCGT:

;; RETURN 1
		LDA	#$01			; A>B
		RTS

@SCLT:

;; RETURN $80
		LDA	#$80			; A<B
		RTS

@SC2:

;; IF ! SAVED_P.ZERO_FLAG: GOTO @SCGT

		PLP				; IF STR A LEN NOT
		BNE	@SCGT			; ZERO THEN A>B

;; RETURN
		RTS				; ELSE A=B

@SC3:

;; [VTYPE.ADR]++

		INC	VTYPE+EVSADR		; INC STR A ADR
		BNE	@SC4
		INC	VTYPE+EVSADR+1
@SC4:

;; [VTYPE1+EVSADR]++

		INC	VTYPE1+EVSADR		; INC STR B ADR
		BNE	@SC1
		INC	VTYPE1+EVSADR+1
		BNE	@SC1

;---------------------------------------------------------------
; ZPADEC -- Decrement a Zero-Page Double Word
; ON ENTRY	X - ZERO PAGE ADDRESS
; ON EXIT 	EQ/NE - ZERO/NOT ZERO
;		[X] - DECREMENTED UNLESS ZERO
;---------------------------------------------------------------

ZPADEC:

;; IF [X]: [X]--

		LDA	0,X			; GET LOW BYTE
		BNE	@ZPAD1			; BR NOT ZERO
		LDA	1,X			; GET HIGH BYTE
		BEQ	@ZPADR			; BR IF ZERO
		DEC	1,X			; DEC HIGH BYTE
@ZPAD1:		DEC	0,X			; DEC LOW BYTE
		TAY				; SET NE COND CODE
@ZPADR:		RTS				; RETURN

;---------------------------------------------------------------
; Functions
;---------------------------------------------------------------

;---------------------------------------------------------------
; XPLEN -- Length Function
;---------------------------------------------------------------

XPLEN:

;; AAPSTR()
		JSR	AAPSTR			; POP STRING WITH ABS ADR

;; AY = [VTYPE.LEN]

		LDA	VTYPE+EVSLEN		; MOVE LENGTH
		LDY	VTYPE+EVSLEN+1

XPIFP:

;; FR0 = AY

		STA	FR0			; TO TOP OF FR0
		STY	FR0+1
XPIFP1:

;; FR0 = IFP(AY)

		JSR	IFP			; AND CONVERT TO FP
XPIFP2:

;; VTYPE = VNUM = 0

		LDA	#0			; CLEAR
		STA	VTYPE			; TYPE AND
		STA	VNUM			; NUMBER

;; ARGPUSH([VTYPE]) ; RETURN

		JMP	ARGPUSH			; PUSH TO STACK AND RETURN

;---------------------------------------------------------------
; XPPEEK -- PEEK Function
;---------------------------------------------------------------

;; def XPPEEK():

XPPEEK:

;; [FR0] = POPFPI() ; AY = [FR0] & 0x00FF

		JSR	POPFPI			; GET INT ARG
		LDY	#0
		LDA	(FR0),Y			; GET MEM BYTE

;; XPIFP(AY) ; RETURN

		JMP	XPIFP			; GO PUSH AS FP

;---------------------------------------------------------------
; XPFRE -- FRE Function
;---------------------------------------------------------------

;; def XPFRE():

XPFRE:

;; [VTYPE] = ARGPOP()

		JSR	ARGPOP			; POP DUMMY ARG

;; [FR0] = [HIMEM] - [MEMTOP]

		SEC
		LDA	HIMEM			; NUM FREE BYTES
		SBC	MEMTOP			; = HIMEM-MEMTOP
		STA	FR0
		LDA	HIMEM+1
		SBC	MEMTOP+1
		STA	FR0+1

;; PUSH_INT_TO_FP(FR0) ; RETURN

		JMP	XPIFP1			; GO PUSH AS FP

;---------------------------------------------------------------
; XPVAL -- VAL Function
;---------------------------------------------------------------

;; def XPVAL():

XPVAL:

;; SETSEOL()

		JSR	SETSEOL			; PUT EOL AT STR END

;; [CIX] = 0 ; AY = ASC_TO_FP()  # Advances CIX

		LDA	#0			; GET NUMERIC TERMINATOR
		STA	CIX			; SET INDEX INTO BUFFER = 0
		JSR	CVAFP			; CONVERT TO F.P.

;; IF RSTSEOL() == OK: GOTO XPIFP2  # to push FR0 and return

		JSR	RSTSEOL			; RESET END OF STR
		BCC	XPIFP2

;; ERROR_STRING_VAL()

		JSR	ERSVAL

;---------------------------------------------------------------
; XPASC -- ASC Function
;---------------------------------------------------------------

;; def XPASC():

XPASC:

;; AAPSTR()
		JSR	AAPSTR			; GET STRING ELEMENT

;; AY = [[VTYPE+EVSADR]].LO
;; PUSH_INT_TO_FP(AY) ; RETURN  # convert AY to FP and PUSH as ARG

		LDY	#0			; GET INDEX TO 1ST BYTE
		LDA	(VTYPE+EVSADR),Y	; GET BYTE
		JMP	XPIFP

;---------------------------------------------------------------
; XPADR -- ADR Function
; ON ENTRY	ARSLVL - A STRING ADDRESS
; ON EXIT	FR0 - ADR AS FP6
;		ARSLVL - THE FP6
;---------------------------------------------------------------

;; def XPADR():

XPADR:

;; AY = AAPSTR() ; PUSH_INT_TO_FP(AY) ; RETURN

		JSR	AAPSTR			; GET STRING
		JMP	XPIFP1			; FINISH

;---------------------------------------------------------------
; XPPDL -- Function Paddle
;---------------------------------------------------------------

;; def XPPDL():

XPPDL:

;; OFFSET = 0x00 ; GOTO _GRF
		LDA	#$00			; GET DISPL FROM BASE ADDR
		BEQ	_GRF

;---------------------------------------------------------------
; XPSTICK -- Function Joystick
;---------------------------------------------------------------

;; def XPSTICK():

XPSTICK:

;; OFFSET = 0x08 ; GOTO _GRF
		LDA	#$08			; GET DISPL FROM BASE ADDR
		BNE	_GRF

;---------------------------------------------------------------
; XPPTRIG -- Function Paddle Trigger
;---------------------------------------------------------------

;; def XPPTRIG():

XPPTRIG:

;; OFFSET = 0x0C ; GOTO _GRF
		LDA	#$0C			; GET DISPL FROM BASE ADDR
		BNE	_GRF

;---------------------------------------------------------------
; XPSTRIG -- Function Joystick Trigger
;---------------------------------------------------------------

;; def XPSTRIG():

XPSTRIG:

;; OFFSET = 0x014
		LDA	#$14			; GET DISPL FROM BASE ADDR

_GRF:

;; [FR0] = POPFPI()

		PHA
		JSR	POPFPI			; GET INTEGER FROM STACK
		LDA	FR0+1			; HIGH ORDER BYTE
		BNE	@ERGRF			; SHOULD BE =0
		LDA	FR0			; GET #

;; X = OFFSET + FR0

		PLA				; GET DISPL FROM BASE
		CLC
		ADC	FR0			; ADD MORE DISPL
		TAX

;; AY = [GRFBAS+X] ; PUSH_INT_TO_FP(AY)

		LDA	GRFBAS,X		; GET VALUE
		LDY	#0
		BEQ	XPIFP			; GO CONVERT & PUSH ON STACK

@ERGRF:

;; ERROR_BAD_VALUE() ; RETURN

		JSR	ERVAL

;---------------------------------------------------------------
; XPSTR -- STR Function
;---------------------------------------------------------------

;; def XPSTR():

XPSTR:

;; [VTYPE] = ARGPOP()

		JSR	ARGPOP			; GET VALUE IN FR0

;; CONVERT_FP_TO_ASC(FR0)

		JSR	CVFASC			; CONVERT TO ASCII

;---------------------------------------------------------------
; Build String Element
;---------------------------------------------------------------

;; [VTYPE.ADR] = [INBUFF]

		LDA	INBUFF			; SET ADDR
		STA	VTYPE+EVSADR
		LDA	INBUFF+1
		STA	VTYPE+EVSADR+1

;; Y = BASIC_STR_LEN([INBUFF])  # Removes 0x80 from last char. String is nul-terminated.

		LDY	#$FF			; INIT FOR LENGTH COUNTER
@XSTR1:		INY				; BUMP COUNT
		LDA	(INBUFF),Y		; GET CHAR
		BPL	@XSTR1			; IF MSB NOT ON, REPEAT
		AND	#$7F			; TURN OFF MSB
		STA	(INBUFF),Y		; RETURNS CHAR TO BUFFER
		INY				; INC TO GET LENGTH

;; [VTYPE.LEN] = Y

		STY	VTYPE+EVSLEN		; SET LENGTH LOW

		BNE	_CHR			; JOIN CHR FUNCTION

;---------------------------------------------------------------
; XPCHR -- CHR Function
;---------------------------------------------------------------

TEMPSTR		EQU	LBUFF+64		; LBUFF FREE DURING RUNNER

;; def XPCHR():

XPCHR:

;; [VTYPE] = ARGPOP()

		JSR	ARGPOP			; GET VALUE IN FR0

;; A = FP_TO_INT().LO

		JSR	CVFPI			; CONVERT TO INTEGER
		LDA	FR0			; GET INTEGER LOW

;; [TEMPSTR] = A

		STA	TEMPSTR			; SAVE

;---------------------------------------------------------------
; Build String Element
;---------------------------------------------------------------

;; [VTYPE+EVSADR] = TEMPSTR  # Address of string

		LDA	#>(TEMPSTR)		; SET ADDR
		STA	VTYPE+EVSADR+1		; X
		LDA	#<(TEMPSTR)		; X
		STA	VTYPE+EVSADR		; X

;; [VTYPE.LEN.LO] = 1

		LDA	#1			; SET LENGTH LOW
		STA	VTYPE+EVSLEN		; X

_CHR:

;; [VTYPE.LEN.HI] = 0

		LDA	#0			; SET LENGTH HIGH
		STA	VTYPE+EVSLEN+1		; X

;; [VNUM] = 0

		STA	VNUM			; CLEAR VARIABLE #

;; [VTYPE] = EVSTR+EVSDTA+EVDIM

		LDA	#EVSTR+EVSDTA+EVDIM	; GET TYPE FLAGS
		STA	VTYPE			; SET VARIABLE TYPE

;; ARGPUSH([VTYPE]) ; RETURN

		JMP	ARGPUSH			; PUSH ON STACK

;---------------------------------------------------------------
; XPRND -- RND Function
;---------------------------------------------------------------

;; def XPRND():

XPRND:

;; [FR1] = 65535 ; ARGPOP()

		LDXY	RNDDIV,FLD1R		; MOVE 65536 TO FR1
		JSR	ARGPOP			; CLEAR DUMMY ARG

;; [FR0] = [RNDLOC] ; INT_TO_FP([FR0])

		LDY	RNDLOC			; GET 2 BYTE RANDOM #
		STY	FR0			; X
		LDY	RNDLOC			; X
		STY	FR0+1			; X
		JSR	CVIFP			; CONVERT TO INTEGER

;; [FR0] /= [FR1]

		JSR	FRDIV			; DO DIVIDE

;; ARGPUSH([VTYPE]) ; RETURN

		JMP	ARGPUSH			; PUT ON STACK

RNDDIV:		.byte	$42,$06,$55,$36,0,0	; 65536.0

;---------------------------------------------------------------
; XPABS -- ABS Function
;---------------------------------------------------------------

;; def XPABS():

XPABS:

;; [VTYPE] = ARGPOP() ; [FR0] = ABS([FR0]) ; ARGPUSH[[VTYPE]] ; RETURN

		JSR	ARGPOP			; GET ARGUMENT
		LDA	FR0			; GET BYTE WITH SIGN
		AND	#$7F			; AND OUT SIGN
		STA	FR0			; SAVE
		JMP	ARGPUSH			; PUSH ON STACK

;---------------------------------------------------------------
; XPUSR -- USR Function
; Push arguments and argument count on CPU stack, call address given by last argument.
;---------------------------------------------------------------

;; def XPUSR():

XPUSR:

;; @USR() ; [FR0] = INT_TO_FP() ; ARGPUSH([VTYPE]) ; RETURN

		JSR	@USR			; PUT RETURN ADDR IN CPU STACK
		JSR	CVIFP			; CONVERT FR0 TO FP
		JMP	ARGPUSH			; PUSH ON STACK

;; def @USR():

@USR:

;; [ZTEMP2] = [COMCNT]

		LDA	COMCNT			; GET COMMA COUNT
		STA	ZTEMP2			; SET AS # OF ARG FOR LOOP CONTROL
@USR1:

;; [FR0] = POPFPI()  # Get argument. Last is address.

		JSR	POPFPI			; GET AN INTEGER FROM OP STACK

;; IF ZTEMP2-- >= 0: { PUSH([FR0]) ; GOTO @USR1 }

		DEC	ZTEMP2			; DECR # OF ARGUMENTS
		BMI	@USR2			; IF DONE THEM ALL, BRANCH

		LDA	FR0			; GET ARGUMENT LOW
		PHA				; PUSH ON STACK
		LDA	FR0+1			; GET ARGUMENT HIGH
		PHA				; PUSH ON STACK
		JMP	@USR1			; GET NEXT ARGUMENT
@USR2:

;; PUSH([COMCNT])

		LDA	COMCNT			; GET # OF ARGUMENTS
		PHA				; PUSH ON CPU STACK

;; [FR0]() ; RETURN

		JMP	(FR0)			; GO TO USER ROUTINE

;---------------------------------------------------------------
; XPINT -- INT Function
;---------------------------------------------------------------

;; def XPINT():

XPINT:

;; [FR0] = ARGPOP() ; [FR0] = XINT() ; ARGPUSH([VTYPE]) ; RETURN

		JSR	ARGPOP			; GET NUMBER
		JSR	XINT			; GET INTEGER
		JMP	ARGPUSH			; PUSH ON ARGUMENT STACK

;---------------------------------------------------------------
; XINT -- Take Integer Part of FR0
;---------------------------------------------------------------

XINT:

;; A = ([FR0] & 0x7F - 0x3F) ; IF A < 0: A = 0

		LDA	FR0			; GET EXPONENT
		AND	#$7F			; AND OUT SIGN BIT
		SEC
		SBC	#$3F			; GET LOCATION OF 1ST FRACTION BYTE
		BPL	@XINT1			; IF > OR = 0, THEN BRANCH
		LDA	#0			; ELSE SET =0

@XINT1:

;; X = A

		TAX				; PUT IN X AS INDEX INTO FR0

;; A = Y = 0

		LDA	#0			; SET ACCUM TO ZERO FOR OR'ING
		TAY				; ZERO Y
@INT2:

;; WHILE X < FMPREC: DO

		CPX	#FMPREC			; IS D.P. LOC > OF = 5?
		BCS	@XINT3			; IF YES, LOOP DONE

;;   A |= [FR0M+X]

		ORA	FR0M,X			; OR IN THE BYTE

;;   [FR0M+X] = 0

		STY	FR0M,X			; ZERO BYTE

;; NEXT X
		INX				; POINT TO NEXT BYTE
		BNE	@INT2			; UNCONDITIONAL BRANCH
@XINT3:

;; X = [FR0] ; IF X >= 0: { NORM() ; RETURN }

		LDX	FR0			; GET EXPONENT
		BPL	@XINT4			; BR IF # IS PLUS

;; X = A ; IF X == 0: { NORM() ; RETURN }

		TAX				; GET TOTAL OF OR'ED BYTES & SET CC
		BEQ	@XINT4			; IF ALL BYTES WERE ZERO BRANCH

;---------------------------------------------------------------
;	# IS NEGATIVE AND NOT A WHOLE # [ADD -1]
;---------------------------------------------------------------

;; [FR1] = -1 ; [FR0] += [FR1] ; RETURN

		LDX	#FR1
		JSR	ZF1			; ZERO FR1
		LDA	#$C0			; PUT -1 IN FR1
		STA	FR1			; X
		LDA	#1			; X
		STA	FR1+1			; X
		.if	BASIC_REVISION > 3
		jmp	FRADD
		nop
		.else
		JSR	FRADD			; ADD IT
		RTS
		.endif
@XINT4:

;; NORM() ; RETURN

		JMP	NORM			; GO NORMALIZE

;---------------------------------------------------------------
; XPSIN -- Sine Function
;---------------------------------------------------------------

XPSIN:		JSR	ARGPOP			; GET ARGUMENT
		JSR	SIN
		.if	BASIC_REVISION = 1
		BCS	_TBAD
		BCC	_TGOOD
		.else
		jmp	_TEVAL
		.endif

;---------------------------------------------------------------
; XPCOS -- Cosine Function
;---------------------------------------------------------------

XPCOS:		JSR	ARGPOP			; GET ARGUMENT
		JSR	COS
		.if	BASIC_REVISION = 1
		BCS	_TBAD
		BCC	_TGOOD
		.else
		jmp	_TEVAL
		.endif

;---------------------------------------------------------------
; XPATN -- Arc Tangent Function
;---------------------------------------------------------------

XPATN:		JSR	ARGPOP
		JSR	ATAN
		.if	BASIC_REVISION = 1
		BCS	_TBAD
		BCC	_TGOOD
		.else
		jmp	_TEVAL
		.endif

		.if	BASIC_REVISION = 1

;---------------------------------------------------------------
; XPLOG -- LOG Function
;---------------------------------------------------------------

XPLOG:		JSR	ARGPOP
		JSR	LOG
		BCS	_TBAD
		BCC	_TGOOD

;---------------------------------------------------------------
; XPL10 -- LOG Base 10
;---------------------------------------------------------------

XPL10:		JSR	ARGPOP
		JSR	LOG10
		BCS	_TBAD
		BCC	_TGOOD

		.else

;---------------------------------------------------------------
; XPLOG -- LOG Function
;---------------------------------------------------------------

XPLOG:		jsr	ARGPOP
		lda	FR0
		beq	_TBAD
		jsr	LOG
LOGCHK:		bcs	_TBAD
		lda	FR0
		eor	#$3B
		bne	_TGOOD
		lda	FR0+1
		and	#$F8
		bne	_TGOOD
		sta	FR0
		beq	_TGOOD

;---------------------------------------------------------------
; XPL10 -- LOG Base 10
;---------------------------------------------------------------

XPL10:		jsr	ARGPOP			; GET ARGUMENT
		lda	FR0
		beq	_TBAD
		jsr	LOG10
		jmp	LOGCHK

		.endif

;---------------------------------------------------------------
; XPEXP -- EXP Function
;---------------------------------------------------------------

XPEXP:		JSR	ARGPOP			; GET ARGUMENT
		JSR	EXP
		.if	BASIC_REVISION = 1
		BCS	_TBAD
		BCC	_TGOOD
		.else
		jmp	_TEVAL
		.endif

		.if	BASIC_REVISION = 1

;---------------------------------------------------------------
; XPSQR -- Square Root Function
;---------------------------------------------------------------

XPSQR:		JSR	ARGPOP			; GET ARGUMENT
		JSR	SQR
		BCS	_TBAD

		; FALL THRU TO _TGOOD

_TGOOD:		JMP	ARGPUSH

_TBAD:		JSR	ERVAL

;---------------------------------------------------------------
; XPPOWER -- Exponential Operator [A**B]
;---------------------------------------------------------------

XPPOWER:	JSR	ARGP2			; GET ARGUMENT IN FR0,FR1
		LDA	FR0			; IS BASE = 0 ?
		BNE	@N0			; IF BASE NOT 0, BRANCH
		LDA	FR1			; TEST EXPONENT
		BEQ	@P0			; IF = 0 ; BRANCH
		BPL	_TGOOD			; IF >0, ANSWER = 0
		BMI	_TBAD			; IF <0, VALUE ERROR
@P0:
		JMP	XTRUE			; IF =0, ANSWER = 1
@N0:
		BPL	@SPEVEN			; IF BASE + THEN NO SPECIAL PROCESS

		AND	#$7F			; AND OUT SIGN BIT
		STA	FR0			; SET AS BASE EXPONENT

		LDA	FR1			; GET EXPONENT OF POWER
		AND	#$7F			; AND OUT SIGN BIT
		SEC
		SBC	#$40			; IS POWER <1?
		BMI	_TBAD			; IF YES, ERROR

		LDX	#6			; GET INDEX TO LAST DIGIT

		CMP	#5			; IF # CAN HAVE DECIMAL
		BCC	@SP4			; PORTION, THEN BR
		LDY	#1
		BNE	@SP3

@SP4:		STA	ZTEMP1			; SAVE EXP -40
		SEC
		LDA	#5			; GET # BYTES POSSIBLE
		SBC	ZTEMP1			; GET # BYTES THAT COULD BE DECIMAL
		TAY				; SET COUNTER

@SP3:		DEX
		DEY				; DEC COUNTER
		BEQ	@SP2			; IF DONE GO TEST EVEN/ODD
		LDA	FR1,X			; GET BYTE OF EXPONENT
		BNE	_TBAD			; IF NOT =0, THEN VALUE ERROR
		BEQ	@SP3			; REPEAT

@SP2:		LDY	#$80			; GET ODD FLAG
		LDA	FR1,X			; GET BYTE OF EXPONENT
		LSR				; IS IT ODD[LAST BIT OFF]?
		BCS	@POWR			; IF YES, BR

@SPEVEN:	LDY	#0
@POWR:		TYA
		PHA

;---------------------------------------------------------------
; Save Exponent [from FR1]
;---------------------------------------------------------------
		LDX	#FMPREC			; GET POINTER INTO FR1
@POWR1:		LDA	FR1,X			; GET A BYTE
		PHA				; PUSH ON CPU STACK
		DEX				; POINT TO NEXT BYTE
		BPL	@POWR1			; BR IF MORE TO DO

		JSR	LOG10			; TAKE LOG OF BASE
		BCS	_TBAD

;---------------------------------------------------------------
; Pull Exponent into FR1 [from CPU Stack]
;---------------------------------------------------------------
		LDX	#0			; GET POINTER INTO FR1
		LDY	#FMPREC			; SET COUNTER
@POWR2:		PLA
		STA	FR1,X			; PUT IN FR1
		INX				; INCR POINTER
		DEY				; DEC COUNTER
		BPL	@POWR2			; BR IF MORE TO DO

		JSR	FRMUL			; GET LOG OF NUMBER
		JSR	EXP10			; GET NUMBER
		BCS	EROV

		PLA				; GET EVEN/ODD FLAG
		BPL	_TGOOD			; IF EVEN, GO PUT ON STACK

		ORA	FR0			; IF ODD MAKE ANSWER-
		STA	FR0			; X
		BNE	_TGOOD			; PUSH ON STACK

		.else	; BASIC_REVISION > 1

;---------------------------------------------------------------
; XPSQR -- Square Root Function (Rev.2+)
;---------------------------------------------------------------

XPSQR:		jsr	ARGPOP
		jsr	SQR
_TEVAL:		bcc	_TGOOD
_TBAD:		jsr	ERVAL

;---------------------------------------------------------------
; XPPOWER -- Exponential Operator [A**B] (Rev.2+)
;---------------------------------------------------------------

XPPOWER:	jsr	ARGP2
		lda	FR1
		beq	_XPP1
		rol
		ldy	FR0
		bne	_XPP2
		bcs	_TBAD

_TGOOD:		jmp	ARGPUSH
_XPP1:		jmp	XTRUE
_XPP2:		ldx	#FR0
		jsr	GNLINE
		ror
		pha
		ldx	#FR1
		jsr	GNLINE
		tya
		bpl	@XPP5
		and	#$7F
		sta	FR0
		bcs	@XPP3
		pla
		bcc	_TBAD
@XPP3:		lda	FR1
		bpl	@XPP4
		clc
@XPP4:		php
		ldx	ZTEMP4
		cpx	#5
		bcs	@XPP7
		lda	FR1+1,X
		ror
		bcc	@XPP7
		lda	#$80
		bne	@XPP8
@XPP5:		lda	FR1
		bpl	@XPP6
		clc
@XPP6:		php
@XPP7:		lda	#0
@XPP8:		pha
		ldx	#5

@XPP9:		lda	FR1,X
		pha
		dex
		bpl	@XPP9
		jsr	LOG10
		ldx	#0
		ldy	#5
@XPP10:		pla
		sta	FR1,X
		inx
		dey
		bpl	@XPP10
		jsr	FRMUL
		jsr	EXP10
		bcs	EROV
		pla
		ora	FR0
		sta	FR0
		plp
		pla
		bpl	_TGOOD
		bcc	_TGOOD
		ldx	#FR0
		jsr	GNLINE
		bcs	_TGOOD
		lda	FR0
		sec
		and	#$7F
		sbc	#$3F
		cmp	#6
		bcs	@XPOUT
		tax
		tay
		sed
		sec
@XPACC:		lda	FR0,X
		adc	#0
		sta	FR0,X
		dex
		bne	@XPACC
		cld
		bcc	@XPP11
		inc	FR0
		inc	FR0+1
@XPP11:		iny
		cpy	#6
		bcs	@XPOUT
		stx	FR0,Y
		bcc	@XPP11
@XPOUT:		jmp	ARGPUSH

		.endif	; BASIC_REVISION > 1

EROV:		JSR	EROVFL

;---------------------------------------------------------------
; XDIM & XCOM -- Execute DIM and COMMON Statements
;---------------------------------------------------------------

;DIM:
XDIM:
XCOM:

@DC1:		LDY	STINDEX			; IF NOT AT
		CPY	NXTSTD			; STATEMENT END
		BCC	@DC2			; THEN CONTINUE
		RTS				; RETURN
@DC2:		JSR	XLET			; GO SET UP VIA EXECUTE EXPR
		LDA	VTYPE			; GET VAR TYPE
		ROR				; SHIFT DIM BIT TO CARRY
		BCC	@DC3			; CONTINUE IF NOT YET DIMMED
@DCERR:		JSR	ERRDIM

@DC3:		SEC				; TURN ON
		ROL				; DIM FLAG
		STA	VTYPE			; AND RESET
		BMI	@DCSTR			; AND BR IF STRING

		LDY	ZTEMP1			; INC I1 BY 1
		LDX	ZTEMP1+1		; AND SET AS DIM1
		INY
		BNE	@DC4
		INX
		BMI	@DCERR			; BR IF OUT OF BOUNDS
@DC4:		STY	VTYPE+EVAD1
		STX	VTYPE+EVAD1+1
		STY	ZTEMP1			; ALSO PUT BACK ONTO
		STX	ZTEMP1+1		; INDEX 1 FOR MULT
		LDY	INDEX2			; INC INDEX 2 BY 1
		LDX	INDEX2+1		; AND SET AS DIM 2
		INY
		BNE	@DC5
		INX
		BMI	@DCERR			; BR IF OUT OF BOUNDS
@DC5:		STY	VTYPE+EVAD2
		STX	VTYPE+EVAD2+1
		JSR	AMUL1			; ZTEMP1 = ZTEMP1*D2
		JSR	AMUL2			; ZTEMP1 = ZTEMP1*6
						;     RESULT IS AN ARRAY
						;     SPACE REQD
		LDY	ZTEMP1			; A,Y = LENGTH
		LDA	ZTEMP1+1
		BMI	@DCERR

		.if	BASIC_REVISION = 1
		JMP	@DCEXP			; GO EXPAND
		.else
		bpl	@DCEXP
		.endif

@DCSTR:		LDA	#0			; SET CURRENT LENGTH =0
		STA	EVSLEN+VTYPE
		STA	EVSLEN+1+VTYPE
		LDY	ZTEMP1			; MOVE INDEX
		STY	VTYPE+EVSDIM		; TO STR DIM
		LDA	ZTEMP1+1		; [ALSO LOAD A,Y]
		STA	VTYPE+EVSDIM+1		; FOR EXPAND
		BNE	@DCEXP			; ENSURE DIM
		CPY	#0			; NOT ZERO
		BEQ	@DCERR			; FOR STRING

@DCEXP:		LDX	#ENDSTAR		; POINT TO END ST & ARRAY SPACE
		JSR	EXPAND			; GO EXPAND
		SEC
		LDA	SVESA			; CALCULATE DISPL INTO
		SBC	STARP			; ST/ARRAY SPACE
		STA	VTYPE+EVSADR		; AND PUT INTO VALUE BOX
		LDA	SVESA+1
		SBC	STARP+1
		STA	VTYPE+EVSADR+1

		JSR	RTNVAR			; RETURN TO VAR VALUE TABLE
		JMP	@DC1			; AND GO FOR NEXT ONE

;---------------------------------------------------------------
; XPOKE -- Execute POKE
;---------------------------------------------------------------

XPOKE:		JSR	GETINT			; GET INTEGER ADDR
		LDA	FR0			; SAVE POKE ADDR
		STA	POKADR
		LDA	FR0+1
		STA	POKADR+1

		JSR	GET1INT			; GET 1 BYTE INTEGER TO POKE

		LDA	FR0			; GET INTEGER TO POKE
		LDY	#0			; GET INDEX
		STA	(POKADR),Y		; GET INDEX
		RTS

;---------------------------------------------------------------
; XDEG -- Execute DEG
;---------------------------------------------------------------

XDEG:		LDA	#DEGON			; GET DEGREES FLAG

		.if	BASIC_REVISION = 1
		STA	RADFLG			; SET FOR TRANSCENDENTALS
		RTS
		.else
		bne	_XRAD			; BR TO SET
		.endif

;---------------------------------------------------------------
; XRAD -- Execute RAD
;---------------------------------------------------------------

XRAD:		LDA	#RADON			; GET RADIAN FLAG
_XRAD:		STA	RADFLG			; SET FOR TRANSCENDENTALS
		RTS

;---------------------------------------------------------------
; XREST -- Execute RESTORE Statement
;---------------------------------------------------------------

XREST:		LDA	#0			; ZERO DATA DISPL
		STA	DATAD

		JSR	TSTEND			; TEST END OF STMT
		BCC	@XR1			; BR IF NOT END
		TAY				; RESTORE TO LN=0
		BEQ	@XR2

@XR1:		JSR	GETPINT			; GET LINE NUM

		LDA	FR0+1			; LOAD LINE NUM
		LDY	FR0

@XR2:		STA	DATALN+1		; SET LINE
		STY	DATALN
		RTS				; DONE

;---------------------------------------------------------------
; XREAD -- Execute READ Statement
;---------------------------------------------------------------

XREAD:		LDA	STINDEX			; SAVE STINDEX
		PHA
		JSR	XGS			; SAVE READ STMT VIA GOSUB

		LDA	DATALN			; MOVE DATALN TO TSLNUM
		STA	TSLNUM
		LDA	DATALN+1
		STA	TSLNUM+1
		JSR	GETSTMT			; GO FIND TSLNUM

		LDA	STMCUR			; MOVE STMCUR TO INBUFF
		STA	INBUFF
		LDA	STMCUR+1
		STA	INBUFF+1

		JSR	XRTN			; RESTORE READ STMT VIA RETURN
		PLA				; GET SAVED STINDEX
		STA	STINDEX			; SET IT

_XRD1:		LDY	#0			; SET CIX=0
		STY	CIX			; SET CIX
		JSR	_XRNT1			; GET LINE NUM LOW
		STA	DATALN			; SET LINE NUM LOW
		JSR	_XRNT
		STA	DATALN+1		; SET LINE NUM HIGH
		JSR	_XRNT
		STA	ZTEMP1			; SET LINE LENGTH
@XRD2:
		JSR	_XRNT
		STA	ZTEMP1+1		; SET STMT LENGTH
		JSR	_XRNT			; GET STMT LINE TOKEN
		.if	BASIC_REVISION = 1
		CMP	#kDATA			; IS IT DATA
		.else
		eor	#1
		.endif
		BEQ	_XRD4			; BR IF DATA

		LDY	ZTEMP1+1		; GET DISPL TO NEXT STMT
		CPY	ZTEMP1			; IS IT EOL
		BCS	@XRD2A			; BR IF EOL
		DEY
		STY	CIX			; SET NEW DISPL
		BCC	@XRD2			; AND CONTINUE THIS STMT

@XRD2A:		STY	CIX
		DEC	CIX

_XRD3:		LDY	#1			; WAS THIS STMT THE
		LDA	(INBUFF),Y		; DIRECT ONE
		BMI	_XROOD			; BR IF IT WAS [OUT OF DATA]

		SEC
		LDA	CIX			; INBUFF + CIX + 1
		ADC	INBUFF			; = ADR NEXT PGM LINE
		STA	INBUFF
		LDA	#0
		STA	DATAD
		ADC	INBUFF+1
		STA	INBUFF+1
		BCC	_XRD1			; GO SCAN THIS NEXT LINE

_XRD4:		.if	BASIC_REVISION = 1
		LDA	#0			; CLEAR ELEMENT COUNT
		.endif
		sta	ZTEMP1

@XRD5:		LDA	ZTEMP1			; GET ELEMENT COUNT
		CMP	DATAD			; AT PROPER ELEMENT
		BCS	@XRD7			; BR IF AT
						;     ELSE SCAN FOR NEXT
@XRD6:		JSR	_XRNT			; GET CHAR
		BNE	@XRD6			; BR IF NOT CR OR COMMA
		BCS	_XRD3			; BR IF CR
		INC	ZTEMP1			; INC ELEMENT COUNT
		BNE	@XRD5			; AND GO NEXT

@XRD7:		LDA	#$40			; SET READ BIT
		STA	DIRFLG
		INC	CIX			; INC OVER DATA TOKEN

		.if	BASIC_REVISION = 1
		JMP	_XINA			; GO DO IT
		.else
		bcs	_XINA
		.endif

_XRNT:		INC	CIX			; INC INDEX
_XRNT1:		LDY	CIX			; GET INDEX
		LDA	(INBUFF),Y		; GET CHAR COUNT
		CMP	#CRPRN			; IS IT A COMMA
		CLC				; CARRY CLEAR FOR COMMA
		BEQ	@XRNT2			; BR IF COMMA
		CMP	#CR			; IS IT CR
@XRNT2:		RTS

_XROOD:		JSR	ERROOD

;---------------------------------------------------------------
; XINPUT -- Execute INPUT
;---------------------------------------------------------------

XINPUT:		LDA	#'?'			; SET PROMPT CHAR
		STA	PROMPT
		JSR	GETTOKEN		; GET FIRST TOKEN
		DEC	STINDEX			; BACK UP OVER IT
		BCC	_XIN0			; BR IF NOT OPERATOR
		JSR	GIOPRM			; GO GET DEVICE NUM
		STA	ENTDTD			; SET DEVICE NUM

_XIN0:		JSR	INTLBF
		.if	BASIC_REVISION = 1
		JSR	GLINE			; GO GET INPUT LINE
		JSR	_XITB			; TEST BREAK
		.else
		jsr	_GLINE2
		jsr	TSTBRK			; TEST BREAK
		beq	_XITB
		.endif
		LDY	#0
		STY	DIRFLG			; SET INPUT MODE
		STY	CIX			; SET CIX=0
_XINA:		JSR	GETTOKEN		; GO GET TOKEN
		INC	STINDEX			; INC OVER TOKEN

		LDA	VTYPE			; IS A STR
		BMI	_XISTR			; BR IF STRING

		JSR	CVAFP			; CONVERT TO FP
		BCS	_XIERR
		JSR	_XRNT1			; GET END TOKEN
		BNE	_XIERR			; ERROR IF NO CR OR COMMA
		JSR	RTNVAR			; RETURN VAR
		JMP	_XINX			; GO FIGURE OUT WHAT TO DO NEXT
_XITB:
		.if	BASIC_REVISION = 1
		JSR	TSTBRK			; GO TEST BREAK
		BNE	@XITBT			; BR IF BRK
		RTS				; DONE
		.endif

@XITBT:		JMP	XSTOP			; STOP
_XIERR:		LDA	#0			; RESET
		STA	ENTDTD			; ENTER DVC
		JSR	ERRINP			; GO ERROR

_XISTR:		JSR	EXPINT			; INIT EXECUTE EXPR
		JSR	ARGPUSH			; PUSH THE STRING
		DEC	CIX			; DEC CIX TO CHAR
		LDA	CIX			; BEFORE SOS
		STA	ZTEMP1			; SAVE THAT CIX
		LDX	#$FF			; SET CHAR COUNT = -1

@XIS1:		INX				; INC CHAR COUNT
		JSR	_XRNT			; GET NEXT CHAR
		BNE	@XIS1			; BR NOT CR OR COMMA
		BCS	@XIS2			; BR IF CR
		BIT	DIRFLG			; IS IT COMMA, IF NOT READ
		BVC	@XIS1			; THEN CONTINUE

@XIS2:		LDY	ZTEMP1			; GET SAVED INDEX
		LDA	STINDEX			; SAVE INDEX
		PHA
		TXA				; ACU = CHAR COUNT
		LDX	#INBUFF			; POINT TO INBUFF
		JSR	RISC			; GO MAKE STR VAR
		PLA
		STA	STINDEX			; RESTORE INDEX
		JSR	RISASN			; THEN DO STA ASSIGN

_XINX:		BIT	DIRFLG			; IS THIS READ
		BVC	@XIN			; BR IF NOT

		INC	DATAD			; INC DATA DISPL
		JSR	TSTEND			; TEST END READ STMT
		BCS	@XIRTS			; BR IF READ END

@XIR1:		JSR	_XRNT1			; GET END DATA CHAR
		BCC	@XINC			; BR IF COMMA
		JMP	_XRD3			; GO GET NEXT DATA LINE

@XIN:		JSR	TSTEND
		BCC	@XIN1

@XIRTS:		JSR	INTLBF			; RESTORE LBUFF
		LDA	#0			; RESTORE ENTER
		STA	ENTDTD			; DEVICE TO ZERO
		RTS				; DONE

@XIN1:		JSR	_XRNT1			; IF NOT END OF DATA
		BCC	@XINC			; THEN BRANCH
		JMP	_XIN0			; AND CONTINUE

@XINC:		INC	CIX			; INC INDEX
		JMP	_XINA			; AND CONTINUE

;---------------------------------------------------------------
; XPRINT -- Execute PRINT Statement
;---------------------------------------------------------------

XPRINT:		LDA	PTABW			; GET TAB VALUE
		STA	SCANT			; SCANT
		LDA	#0			; SET OUT INDEX = 0
		STA	COX

@XPR0:		LDY	STINDEX			; GET STMT DISPL
		LDA	(STMCUR),Y		; GET TOKEN

		CMP	#CCOM
		BEQ	@XPTAB			; BR IF TAB
		CMP	#CCR
		BEQ	@XPEND			; BR IF EOL
		CMP	#CEOS
		BEQ	@XPEND			; BR IF EOL

		CMP	#CSC
		BEQ	@XPNULL			; BR IF NULL
		CMP	#CPND
		BEQ	@XPRIOD

		JSR	EXEXPR			; GO EVALUATE EXPRESSION
		JSR	ARGPOP			; POP FINAL VALUE
		DEC	STINDEX			; DEC STINDEX
		BIT	VTYPE			; IS THIS A STRING
		BMI	@XPSTR			; BR IF STRING

		.if	BASIC_REVISION > 1
		lda	FR0+1
		cmp	#$10
		bcc	@XPRA
		lda	FR0+5
		and	#$F0
		sta	FR0+5
		.endif

@XPRA:		JSR	CVFASC			; CONVERT TO ASCII
		LDA	#0
		STA	CIX

@XPR1:		LDY	CIX			; OUTPUT ASCII CHARACTERS
		LDA	(INBUFF),Y		; FROM INBUFF
		PHA				; UNTIL THE CHAR
		INC	CIX			; WITH THE MSB ON
		JSR	@XPRC			; IS FOUND
		PLA
		BPL	@XPR1
		BMI	@XPR0			; THEN GO FOR NEXT TOKEN

@XPSTR:		JSR	GSTRAD			; GO GET ABS STRING ARRAY
		LDA	#0
		STA	CIX
@XPR2C:		LDA	VTYPE+EVSLEN		; IF LEN LOW
		BNE	@XPR2B			; NOT ZERO BR
		DEC	VTYPE+EVSLEN+1		; DEC LEN HI
		BMI	@XPR0			; BR IF DONE
@XPR2B:		DEC	VTYPE+EVSLEN		; DEC LEN LOW

@XPR2:		LDY	CIX			; OUTPUT STRING CHARS
		LDA	(FR0),Y			; FOR THE LENGTH
		INC	CIX			; OF THE STRING
		BNE	@XPR2A
		INC	FR0+1

@XPR2A:		JSR	@XPRC1
		JMP	@XPR2C

@XPTAB:
@XPR3:		LDY	COX			; DO UNTIL COX+1 < SCANT
		INY
		CPY	SCANT
		BCC	@XPR4
@XPIC3:		CLC
		LDA	PTABW			; SCANT = SCANT+TAB
		ADC	SCANT
		STA	SCANT
		BCC	@XPTAB
@XPR4:		LDY	COX			; DO UNTIL COX = SCANT
		CPY	SCANT
		BCS	@XPNULL
		LDA	#$20			; PRINT BLANKS
		JSR	@XPRC
		JMP	@XPR4

		.if	BASIC_REVISION > 1
@XPEND:		jmp	@XPRCR
		.endif

@XPRIOD:	JSR	GIOPRM			; GET DEVICE NUM
		STA	LISTDTD			; SET AS LIT DEVICE
		DEC	STINDEX			; DEC INDEX
		JMP	@XPR0			; GET NEXT TOKEN

@XPR4A:
@XPNULL:	INC	STINDEX			; INC STINDEX

		.if	BASIC_REVISION = 1
		JMP	@XPR0
@XPEND:
@XPEOL:
@XPEOS:		LDY	STINDEX			; AT END OF PRINT
		DEY
		LDA	(STMCUR),Y		; IF PREV CHAR WAS
		CMP	#CSC			; SEMI COLON THEN DONE
		BEQ	@XPRTN			; ELSE PRINT A CR
		CMP	#CCOM			; OR A COMMA
		BEQ	@XPRTN			; THEN DONE
		.else
		ldy	STINDEX
		lda	(STMCUR),Y
		cmp	#CCR
		beq	@XPRTN			; DONE IF EOL
		cmp	#CEOS
		beq	@XPRTN			; DONE IF END OF STATEMENT
		jmp	@XPR0
		.endif

@XPRCR:		LDA	#CR
		JSR	@XPRC1			; THEN DONE

@XPRTN:		LDA	#0			; SET PRIMARY
		STA	LISTDTD			; LIST DVC = 0
		RTS				; AND RETURN

@XPRC:		AND	#$7F			; MSB OFF
@XPRC1:		INC	COX			; INC OUT INDEX
		JMP	PRCHAR			; OUTPUT CHAR

;---------------------------------------------------------------
; XLPRINT -- Print to Printer
;---------------------------------------------------------------

XLPRINT:	LDA	#<PSTR			; POINT TO FILE SPEC
		STA	INBUFF			; X
		LDA	#>PSTR			; X
		STA	INBUFF+1		; X

		LDX	#7			; GET DEVICE
		STX	LISTDTD			; SET LIST DEVICE
		LDA	#0			; GET AUX 2
		LDY	#8			; GET OPEN TYPE

		JSR	SOPEN			; DO OPEN
		JSR	IOTEST			; TEST FOR ERROR

		JSR	XPRINT			; DO THE PRINT

		JMP	CLSYS1			; CLOSE DEVICE

PSTR:		.byte	"P:",CR

;---------------------------------------------------------------
; XLIST -- Execute LIST Command
;---------------------------------------------------------------

XLIST:		LDY	#0			; SET TABLE SEARCH LINE NO
		STY	TSLNUM			; TO ZERO
		STY	TSLNUM+1
		DEY
		STY	LELNUM			; SET LIST END LINE NO
		LDA	#$7F			; TO $7FFF
		STA	LELNUM+1
		STA	DSPFLG			; SET NON-DISPLAY MODE
		LDA	#CR			; POINT CR
		JSR	PRCHAR

		JSR	XGS			; SAVE CURLINE VIA GOSUB
@XL0:		LDY	STINDEX			; GET STMT INDEX
		INY				; INC TO NEXT CHAR
		CPY	NXTSTD			; RT NEXT STMT
		BCS	@LSTART			; BR IF AT, NO PARMS

		LDA	STINDEX			; SAVE STINDEX
		PHA				; ON STACK
		JSR	POP1			; POP FIRST ARGUMENT
		PLA				; RESTORE STINDEX TO
		STA	STINDEX			; RE-DO FIRST ARG
		LDA	VTYPE			; GET VAR TYPE
		BPL	@XL1			; BR IF NOT FILE SPEC STRING
		JSR	FLIST			; GO OPEN FILE
		JMP	@XL0			; GO BACK TO AS IF FIRST PARM

@XL1:		JSR	GETPINT			; GO GET START LNO
		STA	TSLNUM+1
		LDA	FR0			; MOVE START LNO
		STA	TSLNUM			; TO TSLNUM

		LDY	STINDEX			; GET STMT INDEX
		CPY	NXTSTD			; AT NEXT STMT
		BEQ	@LSE			; BR IF AT, NO PARMS
		JSR	GETPINT			; GO GET LINE NO

@LSE:		LDA	FR0			; MOVE END LINE NO
		STA	LELNUM			; TO LIST END LINE NO
		LDA	FR0+1
		STA	LELNUM+1

@LSTART:	JSR	GETSTMT			; GO FIND FIRST LINE

@LNXT:		JSR	TENDST			; AT END OF STMTS
		BMI	@LRTN			; BR AT END

@LTERNG:	LDY	#1			; COMPARE CURRENT STMT
		LDA	(STMCUR),Y		; LINE NUM WITH END
		CMP	LELNUM+1		; LINE NUM
		BCC	@LGO
		BNE	@LRTN
		DEY
		LDA	(STMCUR),Y
		CMP	LELNUM
		BCC	@LGO
		BNE	@LRTN

@LGO:		JSR	LLINE			; GO LIST THE LINE
		JSR	TSTBRK			; TEST FOR BREAK
		.if	BASIC_REVISION = 1
		BNE	@LRTN			; BR IF BREAK
		.else
		beq	@LRTN			; BR IF BREAK
		.endif
		JSR	GETLL
		JSR	GNXTL			; GO INC TO NEXT LINE
		JMP	@LNXT			; GO DO THIS LINE

@LRTN:		LDA	LISTDTD			; IF LIST DEVICE
		BEQ	@LRTN1			; IS ZERO, BR
		JSR	CLSYSD			; ELSE CLOSE DEVICE
		LDA	#0			; AND RESET
		STA	LISTDTD			; DEVICE TO ZERO
@LRTN1:		STA	DSPFLG			; SET DISPLAY MODE
		JMP	XRTN			; THEN RESTORE LIST LINE AND RETURN

;---------------------------------------------------------------
; LSCAN -- Scan a Table for LIST Token
;	ENTRY PARMS
;	    X = SKIP LENGTH
;	  A,Y = TABLE ADR
;	SCANT = TOKEN
;---------------------------------------------------------------

LSCAN:		STX	SRCSKP			; SAVE SKIP LENGTH
		JSR	@LSST			; SAVE SRC ADR

@LSC0:		LDY	SRCSKP			; GET SKIP FACTOR
		DEC	SCANT			; DECREMENT SRC COUNT
		BMI	@LSINC			; BR IF DONE

@LSC1:		LDA	(SRCADR),Y		; GET CHARACTER
		BMI	@LSC2			; BR IF LAST CHARACTER
		INY				; INC TO NEXT
		BNE	@LSC1			; BR ALWAYS
@LSC2:		INY				; INC TO AFTER LAST CHAR
		JSR	@LSINC			; INC SRC ADR BY Y
		JMP	@LSC0			; GO TRY NEXT

@LSINC:		CLC
		TYA				; Y PLUS
		ADC	SRCADR			; SRCADR
		STA	SRCADR			; IS
		TAY				; NEW
		LDA	SRCADR+1		; SRCADR
		ADC	#0

@LSST:		STA	SRCADR+1		; STORE NEW SRCADR
		STY	SRCADR			; AND
		RTS				; RETURN

;---------------------------------------------------------------
; LPRTOKEN -- Print a Token
;---------------------------------------------------------------

LPRTOKEN:
@LPRTOKEN:	LDY	#$FF			; INITIALIZE INDEX TO ZERO
		STY	SCANT

@LPT1:		INC	SCANT			; INC INDEX
		LDY	SCANT			; GET INDEX
		LDA	(SRCADR),Y		; GET TOKEN CHAR
		PHA				; SAVE CHAR
		CMP	#CR			; IF ATARI CR
		BEQ	@LPT1A			; THEN DON'T AND
		AND	#$7F			; TURN OFF MSB
		BEQ	@LPT2			; BR IF NON-PRINTING
@LPT1A:
		JSR	PRCHAR			; GO PRINT CHAR
@LPT2:
		PLA				; GET CHAR
		BPL	@LPT1			; BR IF NOT END CHAR
		RTS				; GO BACK TO MY BOSS

;---------------------------------------------------------------
; LPTWB -- Print Token with Bank Before and After
;---------------------------------------------------------------

LPTWB:		LDA	#$20			; GET BLANK
		JSR	PRCHAR			; GO PRINT IT
_LPTTB:		JSR	LPRTOKEN		; GO PRINT TOKEN
@LPBLNK:	LDA	#$20			; GET BLANK
		JMP	PRCHAR			; GO PRINT IT AND RETURN

;---------------------------------------------------------------
; LLINE -- List a Line
;---------------------------------------------------------------

LLINE:		LDY	#0
		LDA	(STMCUR),Y		; MOVE LINE NO
		STA	FR0			; TO FR0
		INY
		LDA	(STMCUR),Y
		STA	FR0+1
		JSR	CVIFP			; CONVERT TO FP
		JSR	CVFASC			; CONVERT TO ASCII
		LDA	INBUFF			; MOVE INBUFF ADDR
		STA	SRCADR			; TO SRCADR
		LDA	INBUFF+1
		STA	SRCADR+1
		JSR	_LPTTB			; AND PRINT LINE NO

LDLINE:		LDY	#2
		LDA	(STMCUR),Y		; GET LINE LENGTH
		STA	LLNGTH			; AND SAVE
		INY
@LL1:		LDA	(STMCUR),Y		; GET STMT LENGTH
		STA	NXTSTD			; AND SAVE
		INY				; INC TO STMT TYPE
		STY	STINDEX			; AND SAVE DISPL
		JSR	@LSTMT			; GO LIST STMT
		LDY	NXTSTD			; DONE LINE
		CPY	MAXCIX
		BCC	@LL1			; BR IF NOT
		RTS				; ELSE RETURN

;---------------------------------------------------------------
; LSTMT -- List a Statement
;---------------------------------------------------------------

@LSTMT:		JSR	@LGCT			; GET CURRENT TOKEN
		CMP	#kILET			; IF IMP LET
		BEQ	@LADV			; BR
		JSR	LSTMC			; GO LIST STMT CODE

		JSR	@LGCT			; GO GET CURRENT TOKEN
		CMP	#kERROR			; BR IF ERROR STMT
		BEQ	@LDR
		CMP	#kDATA+1		; WAS IT DATA OR REM
		BCS	@LADV			; BR IF NOT

@LDR:		JSR	@LGNT			; OUTPUT DATA/REM
		JSR	PRCHAR			; THEN PRINT THE CR
		JMP	@LDR

@LADV:		JSR	@LGNT			; GET NEXT TOKEN
		BPL	@LNVAR			; BR IF NOT VARIABLE

		AND	#$7F			; TURN OFF MSB
		STA	SCANT			; AND SET AS SCAN COUNT
		LDX	#0			; SET VNT FOR
		LDA	VNTP+1			; VAR NAME
		LDY	VNTP
		JSR	LSCAN
		JSR	LPRTOKEN		; PRINT VAR NAME
		CMP	#'('+$80		; NAME END IN LPAREN
		BNE	@LADV			; BR IF NOT
		JSR	@LGNT			; DON'T PRINT NEXT TOKEN
		JMP	@LADV			; IF IT IS A PAREN

@LNVAR:		CMP	#CSTOK			; TOKEN: $0F
		BEQ	@LSTC			; BR IF 0F, STR CONST

		BCS	@LOP			; BR IF TOKEN >$0F
						;     ELSE IT'S NUM CONST
		JSR	NCTOFR0			; GO MOVE FR0
		DEC	STINDEX			; BACK INDEX TO LAST CHAR
		JSR	CVFASC			; CONVERT FR0 TO ASCII
		LDA	INBUFF			; POINT SRCADR
		STA	SRCADR			; TO INBUFF WHERE
		LDA	INBUFF+1		; CHAR IS
		STA	SRCADR+1
@LSX:		JSR	LPRTOKEN		; GO PRINT NUMBER
		JMP	@LADV			; GO FOR NEXT TOKEN

@LSTC:		JSR	@LGNT			; GET NEXT TOKEN
		STA	SCANT			; WHICH IS STR LENGTH
		LDA	#'"'			; PRINT DOUBLE QUOTE CHAR
		JSR	PRCHAR
		LDA	STENUM
		BEQ	@LS3

@LS2:		JSR	@LGNT			; OUTPUT STR CONST
		JSR	PRCHAR			; CHAR BY CHAR
		DEC	STENUM			; UNTIL COUNT =0
		BNE	@LS2

@LS3:		LDA	#'"'			; THEN OUTPUT CLOSING
		JSR	PRCHAR			; DOUBLE QUOTE
		JMP	@LADV

@LOP:		SEC
		SBC	#$10			; SUBTRACT THE 10
		STA	SCANT			; SET FOR SCAN COUNT
		LDX	#0
		LDA	#>OPNTAB
		LDY	#<OPNTAB
		JSR	LSCAN			; SCAN OP NAME TABLE
		JSR	@LGCT			; GO GET CURRENT TOKEN
		CMP	#CFFUN			; IS IT FUNCTION
		BCS	@LSX			; BR IF FUNCTION
		LDY	#0
		LDA	(SRCADR),Y		; GET FIRST CHAR
		AND	#$7F			; TURN OFF MSB
		JSR	TSALPH			; TEST FOR ALPHA
		BCS	@LSX			; BR NOT ALPHA
		JSR	LPTWB			; LIST ALPHA WITH
		JMP	@LADV			; BLANKS FOR AND AFTER

@LGNT:						; GET NEXT TOKEN
		INC	STINDEX			; INC TO NEXT
@LGCT:		LDY	STINDEX			; GET DISPL
		CPY	NXTSTD			; AT END OF STMT
		BCS	@LGNTE			; BR IF AT END
		LDA	(STMCUR),Y		; GET TOKEN
		RTS				; AND RETURN

@LGNTE:		PLA				; POP CALLERS ADR
		PLA				; AND
		RTS				; GO BACK TO LIST LINE

LSTMC:		STA	SCANT			; SET INSCAN COUNT
		LDX	#2			; AND
		LDA	#>SNTAB
		LDY	#<SNTAB			; STATEMENT NAME TABLE
		JSR	LSCAN
		JMP	_LPTTB			; GO LIST FOLLOWING BLANK

;---------------------------------------------------------------
; XFOR -- Execute FOR
;---------------------------------------------------------------

XFOR:		JSR	_SAVDEX			; SAVE STINDEX
		JSR	EXEXPR			; DO ASSIGNMENT
		LDA	VNUM			; GET VARIABLE #
		ORA	#$80			; OR IN HIGH ORDER BIT
		PHA				; SAVE ON CPU STACK
		JSR	FIXRSTK			; FIX RUN STACK
;---------------------------------------------------------------
;	BUILD STACK ELEMENT
;---------------------------------------------------------------
		LDA	#FBODY			; GET # OF BYTES
		JSR	_REXPAND		; EXPAND RUN STACK

		JSR	POP1			; EVAL EXP & GET INTO FR0
;---------------------------------------------------------------
;	PUT LIMIT [INFR0] ON STACK
;---------------------------------------------------------------
		LDX	#FR0			; POINT TO FR0
		LDY	#FLIM			; GET DISPL
		JSR	_MV6RS			; GO MOVE LIMIT
;---------------------------------------------------------------
;	SET DEFAULT STEP
;---------------------------------------------------------------
		JSR	ZFR0			; CLEAR FR0 TO ZEROS
		LDA	#1			; GET DEFAULT STEP
		STA	FR0+1			; SET DEFAULT STEP VALUE
		LDA	#$40			; GET DEFAULT EXPONENT
		STA	FR0			; STORE
;---------------------------------------------------------------
;	TEST FOR END OF STMT
;---------------------------------------------------------------
		JSR	TSTEND			; TEST FOR END OF START
		BCS	@NSTEP			; IF YES, WE ARE AT END OF STMT
;---------------------------------------------------------------
;	ELSE GET STEP VALUE
;---------------------------------------------------------------
		JSR	POP1			; EVAL EXP & GET INTO FR0
@NSTEP:
;---------------------------------------------------------------
;	PUT STEP [IN FR0] ON STACK
;---------------------------------------------------------------
		LDX	#FR0			; POINT TO FR0
		LDY	#FSTEP			; GET DISPL
		JSR	_MV6RS			; GO MOVE STEP

		PLA				; GET VARIABLE #

;---------------------------------------------------------------
;	PSHRSTK - PUSH COMMON PORT OF FOR/GOSUB
;		- ELEMENT ON RUN STACK
;---------------------------------------------------------------
;	ON ENTRY	A - VARIABLE # OR 0 [FOR GOSUB]
;			TLSNUM - LINE #
;			STINDEX - DISPL TO STMT TOKEN +1
;---------------------------------------------------------------

PSHRSTK:

;---------------------------------------------------------------
;	EXPAND RUN STACK
;---------------------------------------------------------------
		PHA				; SAVE VAR # / TYPE
		LDA	#GFHEAD			; GET # OF BYTES TO EXPAND
		JSR	_REXPAND		; EXPAND [OLD TOP RETURN IN ZTEMP1]
;---------------------------------------------------------------
;	PUT ELEMENT ON STACK
;---------------------------------------------------------------
		PLA				; GET VARIABLE #/TYPE
		LDY	#GFTYPE			; GET DISPL TO TYPE IN HEADER
		STA	(TEMPA),Y		; PUT VAR#/TYPE ON STACK

		LDA	(STMCUR),Y		; GET LINE # LOW
		INY				; POINT TO NEXT HEADER BYTE
		STA	(TEMPA),Y		; PUT LINE # LOW IN HEADER
		LDA	(STMCUR),Y		; GET LINE # HIGH
		INY
		STA	(TEMPA),Y		; PUT IN HEADER

		LDX	SAVDEX			; GET SAVED INDEX INTO LINE
		DEX				; POINT TO TOKEN IN LINE
		TXA				; PUT IN A
		INY				; POINT TO DISPL IN HEADER
		STA	(TEMPA),Y		; PUT IN HEADER
		RTS

;---------------------------------------------------------------
; XGOSUB -- Execute GOSUB
;---------------------------------------------------------------

XGOSUB:		JSR	XGS

;---------------------------------------------------------------
; XGOTO -- Execute GOTO
;---------------------------------------------------------------

XGOTO:		JSR	GETPINT			; GET POSITIVE INTEGER IN FR0

;---------------------------------------------------------------
;	GET LINE ADRS & POINTERS
;---------------------------------------------------------------

XGO2:		LDA	FR0+1			; X
		STA	TSLNUM+1		; X
		LDA	FR0			; PUT LINE # IN TSLNUM
		STA	TSLNUM			; X

XGO1:		JSR	GETSTMT			; LINE POINTERS AND STMT ADDRESS
		BCS	@ERLN			; IF NOT FOUND ERROR
		PLA				; CLEAN UP STACK
		PLA
		JMP	EXECNL			; GO TO EXECUTE CONTROL

@ERLN:		JSR	RESCUR			; RESTORE STMT CURRENT

		JSR	ERRNOLN			; LINE # NOT FOUND

RESCUR:		LDA	SAVCUR			; RESTORE STMCUR
		STA	STMCUR			; X
		LDA	SAVCUR+1		; X
		STA	STMCUR+1		; X
		RTS

;---------------------------------------------------------------
; XGS -- Perform GOSUB[GOSUB, LIST, READ]
;---------------------------------------------------------------

XGS:		JSR	_SAVDEX			; GET STMT INDEX

XGS1:		LDA	#0			; GET GOSUB TYPE
		.if	BASIC_REVISION = 1
		JMP	PSHRSTK			; PUT ELEMENT ON RUN STACK
		.else
		beq	PSHRSTK
		.endif

;---------------------------------------------------------------
; XNEXT -- Execute NEXT
;---------------------------------------------------------------

XNEXT:

;---------------------------------------------------------------
;	GET VARIABLE #
;---------------------------------------------------------------
		LDY	STINDEX			; GET STMT INDEX
		LDA	(STMCUR),Y		; GET VARIABLE
		STA	ZTEMP2+1		; SAVE
;---------------------------------------------------------------
;	GET ELEMENT
;---------------------------------------------------------------

@XN:		JSR	POPRSTK			; PULL ELEMENT FROM RUN STACK
						;     VAR#/TYPE RETURN IN A
		BCS	@ERNFOR			; IF AT TOP OF STACK, ERROR
		BEQ	@ERNFOR			; IF TYPE = GOSUB, ERROR
		CMP	ZTEMP2+1		; DOES STKVAR# = OUR VAR #
		BNE	@XN
;---------------------------------------------------------------
;	GET STEP VALUES IN FR1
;---------------------------------------------------------------
		LDY	#FSTEP			; GET DISPL INTO ELEMENT
		JSR	_PL6RS			; GET STEP INTO FR1
;---------------------------------------------------------------
;	SAVE TYPE OF STEP [+ OR -]
;---------------------------------------------------------------
		LDA	FR1			; GET EXP FR1 [CONTAINS SIGN]
		PHA				; PUSH ON CPU STACK
;---------------------------------------------------------------
;	GET VARIABLE VALUE
;---------------------------------------------------------------
		LDA	ZTEMP2+1		; GET VAR #
		JSR	GETVAR			; GET VARIABLE VALUE
;---------------------------------------------------------------
;	GET NEW VALUE
;---------------------------------------------------------------
		JSR	FRADD			; ADD STEP TO VALUE
		JSR	RTNVAR			; PUT IN VARIABLE TABLE
;---------------------------------------------------------------
;	GET LIMIT IN FR1
;---------------------------------------------------------------
		LDY	#FLIM			; GET DISPL TO LIMIT IN ELEMENT
		JSR	_PL6RS			; GET LIMIT INTO FR1
		PLA				; GET SIGN OF STEP
		BPL	@STPPL			; BR IF STEP +
;---------------------------------------------------------------
;	COMPARE FOR NEGATIVE STEP
;---------------------------------------------------------------
		JSR	FRCMP			; COMPARE VALUE TO LIMIT
		BPL	@NEXT			; IF VALUE >= LIMIT, CONTINUE
		RTS				; ELSE DONE
;---------------------------------------------------------------
;	COMPARE FOR POSITIVE STEP
;---------------------------------------------------------------

@STPPL:		JSR	FRCMP			; COMPARE VALUE TO LIMIT
		BEQ	@NEXT			; IF = CONTINUE
		BMI	@NEXT			; IF < CONTINUE
@NRTN:		RTS				; ELSE RETURN

@NEXT:		LDA	#GFHEAD+FBODY		; GET # BYTES IN FOR ELEMENT
		JSR	_REXPAND		; GO PUT IT BACK ON STACK

		.if	BASIC_REVISION = 1
		JSR	_GETTOK			; GET TOKEN [RETURNS IN A]
		CMP	#kFOR			; IS TOKEN = FOR?
		BNE	_ERGFD			; IF NOT IT'S AN ERROR
		RTS
		.else
		jsr	_XRTN3
		cmp	#kFOR
		beq	@NRTN
		jmp	_XRTN1
		.endif

@ERNFOR:	JSR	ERNOFOR

		.if	BASIC_REVISION = 1

;---------------------------------------------------------------
; XRTN -- Execute RETURN (Rev.1)
;---------------------------------------------------------------

XRTN:		JSR	POPRSTK			; GET ELEMENT FROM RUN STACK
		BCS	@ERRTN			; IF AT TOP OF STACK, ERROR
		BNE	XRTN			; IF TYPE NOT GOSUB, REPEAT

		JSR	_GETTOK			; GET TOKEN FROM LINE [IN A]
		CMP	#kGOSUB			; IS IT GOSUB?
		BEQ	@XRTS			; BR IF GOSUB
		CMP	#kON
		BEQ	@XRTS			; BR IF ON
		CMP	#kLIST
		BEQ	@XRTS			; BR IF LIST
		CMP	#kREAD			; MAYBE IT'S READ
		BNE	_ERGFD			; IF NOT, ERROR
@XRTS:		RTS

@ERRTN:
		JSR	ERBRTN			; BAD RETURN ERROR

;---------------------------------------------------------------
;	_GETTOK - GET TOKEN POINTED TO BY RUN STACK ELEMENT
;---------------------------------------------------------------
;	ON EXIT		A - CONTAINS TOKEN
;---------------------------------------------------------------

_GETTOK:	JSR	SETLINE			; SET UP TO PROCESS LINE
		BCS	_ERGFD			; IF LINE # NOT FOUND, ERROR

		LDY	SVDISP			; GET DISPL TO TOKEN
		DEY				; POINT TO NEXT STMT DISPL
		LDA	(STMCUR),Y		; GET NEXT STMT DISPL
		STA	NXTSTD			; SAVE

		INY				; GET DISPL TO TOKEN AGAIN
		LDA	(STMCUR),Y		; GET TOKEN
		RTS

_ERGFD:		JSR	RESCUR			; RESTORE STMT CURRENT
		JSR	ERGFDEL
		.endif

;---------------------------------------------------------------
; XRUN -- Execute RUN
;---------------------------------------------------------------

XRUN:

;---------------------------------------------------------------
;	TEST FOR END OF STMT
;---------------------------------------------------------------
		.if	BASIC_REVISION = 1
		JSR	TSTEND			; CHECK FOR END OF STMT
		BCS	@NOFILE			; IF END OF STMT, BR
		JSR	FRUN			; ELSE HAVE FILE NAME
@NOFILE:
		.else
		jsr	TSTEND
		bcs	@XRNOP
		jsr	FRUN
@XRNOP:		nop
		.endif

;---------------------------------------------------------------
;	GET 1ST LINE # OF PROGRAM
;---------------------------------------------------------------
		LDA	#0			; GET SMALLEST POSSIBLE LINE NUM
		STA	TSLNUM			; X
		STA	TSLNUM+1		; X
		JSR	SETLINE			; SET UP LINE POINTERS
		JSR	TENDST			; TEST FOR END OF STMT TABLE
		BMI	_RUNEND			; IF AT END, BR
		JSR	RUNINIT			; CLEAR SOME STORAGE
						;     FALL THRU TO CLR
; XCLR -- Execute CLR

XCLR:		JSR	ZVAR			; GO ZERO VARS
		JSR	RSTPTR			; GO RESET STACK PTRS
		LDA	#0			; CLEAR DATA VALUES
		STA	DATALN
		STA	DATALN+1
		STA	DATAD
		RTS

_RUNEND:	JMP	SNX1			; NO PROGRAM TO RUN

;---------------------------------------------------------------
; XIF -- Execute IF
;---------------------------------------------------------------

XIF:		JSR	POP1			; EVAL EXP AND GET VALUE INTO FR0
		LDA	FR0M			; GET 1ST MANTISSA BYTE
		BEQ	@FALSE			; IF = 0, # = 0 AND IS FALSE
;---------------------------------------------------------------
;	EXPRESSION TRUE
;---------------------------------------------------------------
		JSR	TSTEND			; TEST FOR END OF STMT

		.if	BASIC_REVISION = 1
		BCS	@TREOS			; IF AT EOS, BRANCH
;---------------------------------------------------------------
;	TRUE AND NOT EOS
;---------------------------------------------------------------
		JMP	XGOTO			; JOIN GOTO
;---------------------------------------------------------------
;	TRUE AND EOS
;---------------------------------------------------------------

@TREOS:		RTS

		.else

		bcs	@TFRTS			; IF AT EOS, BRANCH
		jmp	XGOTO			; JOIN GOTO

		.endif

;---------------------------------------------------------------
;	EXPRESSION FALSE
;---------------------------------------------------------------

@FALSE:		LDA	LLNGTH			; GET DISPL TO END OF LINE
		STA	NXTSTD			; SAVE AS DISPL TO NEXT STMT
@TFRTS:		RTS

;---------------------------------------------------------------
; XEND -- Execute END
;---------------------------------------------------------------

XEND:		JSR	STOP
		JMP	SNX1

;---------------------------------------------------------------
; XSTOP -- Execute STOP
;---------------------------------------------------------------

XSTOP:		JSR	STOP			; GO SET UP STOP LINE #

;---------------------------------------------------------------
;	PRINT MESSAGE
;---------------------------------------------------------------
		JSR	PRCR			; PRINT CR
		LDA	#<_MSTOP		; SET POINTER FOR MESSAGE
		STA	SRCADR			; X
		LDA	#>_MSTOP		; X
		STA	SRCADR+1		; X

		JSR	LPRTOKEN		; PRINT IT
		JMP	_ERRM2

STOP:
		JSR	TENDST			; GET CURRENT LINE # HIGH
		BMI	_STOPEND		; IF -, THIS IS DIRECT STMT
						; DON'T STOP
		STA	STOPLN+1		; SAVE LINE # HIGH FOR CON
		DEY				; DEC INDEX
		LDA	(STMCUR),Y		; GET LINE # LOW
		STA	STOPLN			; SAVE FOR CON
_STOPEND:	JMP	SETDZ			; SET L/D DEVICE =0

		.if	BASIC_REVISION = 1
_MSTOP:		BasicString "STOPPED "
		.endif

;---------------------------------------------------------------
; XCONT -- Execute CONT
;---------------------------------------------------------------

XCONT:		JSR	TENDST			; IS IT INDIRECT STMT?
		BPL	_STOPEND		; IF YES, BR
		LDA	STOPLN			; SET STOP LINE # AS LINE # FOR GET
		STA	TSLNUM			; X
		LDA	STOPLN+1		; X
		STA	TSLNUM+1		; X

		JSR	GETSTMT			; GET ADR OF STMT WE STOPPED AT
		JSR	TENDST			; AT END OF STMT TAB ?
		BMI	_RUNEND
		JSR	GETLL			; GET NEXT LINE ADDR IN CURSTM
		JSR	GNXTL			; X
		JSR	TENDST			; SEE IF WE ARE AT END OF STMT TABLE
		BMI	_RUNEND			; BR IF MINUS
		JMP	SETLN1			; SET UP LINE POINTERS

;---------------------------------------------------------------
; XTRAP -- Execute TRAP
;---------------------------------------------------------------

XTRAP:		JSR	GETINT			; CONVERT LINE # TO POSITIVE INT
		LDA	FR0			; SAVE LINE # LOW AS TRAP LINE
		STA	TRAPLN			; IN CASE OF LATER ERROR
		LDA	FR0+1			; X
		STA	TRAPLN+1		; X
		RTS

;---------------------------------------------------------------
; XON -- Execute ON
;---------------------------------------------------------------

XON:		JSR	_SAVDEX			; SAVE INDEX INTO LINE
		JSR	GET1INT			; GET 1 BYTE INTEGER
		LDA	FR0			; GET VALUE
		BEQ	@ERV			; IF ZERO, FALL THROUGH TO NEXT STMT

		LDY	STINDEX			; GET STMT INDEX
		DEY				; BACK UP TO GOSUB/GOTO
		LDA	(STMCUR),Y		; GET CODE
		CMP	#CGTO			; IS IT GOTO?
		.if	BASIC_REVISION > 1
		php
		.endif
		BEQ	@GO			; IF YES, DON'T PUSH ON RUN STACK

;---------------------------------------------------------------
;	THIS IS ON - GOSUB:  PUT ELEMENT ON RUN STACK
;---------------------------------------------------------------
		JSR	XGS1			; PUT ELEMENT ON RUN STACL
						; FOR RETURN
@GO:		LDA	FR0			; GET INDEX INTO EXPRESSIONS
		STA	ONLOOP			; SAVE FOR LOOP CONTROL
@ON1:
		JSR	GETPINT			; GET + INTEGER
		DEC	ONLOOP			; IS THIS THE LINE # WE WANT?

		.if	BASIC_REVISION = 1
		BEQ	@ON2			; IF YES, GO DO IT
		.else
		beq	@ERP
		.endif

		JSR	TSTEND			; ARE THERE MORE EXPRESSIONS
		BCC	@ON1			; IF YES, THEN EVAL NEXT ONE

		.if	BASIC_REVISION = 1
		RTS				; ELSE FALL THROUGH TO NEXT STMT
@ON2:		JMP	XGO2			; JOIN GOTO
@ERV:		RTS				; FALL THROUGH TO NEXT STMT.
		.else
		plp
		beq	@ERV
		jsr	XPOP
@ERV:		rts
@ERP:		plp
		jmp	XGO2
		.endif

		;
		; Execution Control Statement Subroutines
		;

;---------------------------------------------------------------
; SETLINE -- Set Up Line Pointers
;---------------------------------------------------------------
;	ON ENTRY	TSLNUM - LINE #
;---------------------------------------------------------------
;	ON EXIT		LLNGTH - CONTAIN PROPER VALUES
;			NXTSTD - X
;			CARRY SET BY GETSTMT IF LINE # NOT FOUND
;---------------------------------------------------------------

SETLINE:	JSR	GETSTMT			; GET STMCUR

SETLN1:		LDY	#2			; GET DISPL IN LINE TO LENGTH
		LDA	(STMCUR),Y		; GET LINE LENGTH
		STA	LLNGTH			; SET LINE LENGTH
		INY
		STY	NXTSTD
		RTS

;---------------------------------------------------------------
; FIXRTSK -- Fix Run Stack -- Remove Old FORs
;---------------------------------------------------------------
;	ON ENTRY	A - VARIABLE # IN CURRENT FOR
;---------------------------------------------------------------
;	ON EXIT		RUNSTK CLEAR OF ALL FOR'S
;---------------------------------------------------------------

FIXRSTK:	STA	ZTEMP2+1		; SAVE VAR # OF THIS FOR
		JSR	_SAVRTOP		; SAVE TOP OF RUN STACK IN ZTEMP1

@FIXR:		JSR	POPRSTK			; POP AN ELEMENT FROM RUNSTK
		BCS	@TOP			; IF AT TOP - WE ARE DONE
		BEQ	@TOP			; IF CC = 08 ELEMENT WAS GOSUB
		CMP	ZTEMP2+1		; IS STK VAR # = OUR VAR #?
		.if	BASIC_REVISION = 1
		BEQ	_FNVAR			; IF YES, WE ARE DONE
		.else
		beq	_FXRTS
		.endif
		BNE	@FIXR			; ELSE LOOK AT NEXT ELEMENT

;---------------------------------------------------------------
;	FOR VAR # NOT ON STACK ABOVE TOP OF GOSUB
;		[RESTORE TOP OF STACK]
;---------------------------------------------------------------

@TOP:		LDA	TEMPA			; RESTORE TOPRSTK
		STA	TOPRSTK			; X
		LDA	TEMPA+1			; X
		STA	TOPRSTK+1		; X
_FXRTS:		RTS

		.if	BASIC_REVISION = 1
;---------------------------------------------------------------
;	FOR VAR # FOUND ON STACK
;---------------------------------------------------------------
_FNVAR:		RTS
		.endif

;---------------------------------------------------------------
; POPRSTK -- Pop Element from Run Stack
;---------------------------------------------------------------
;	ON EXIT		A - TYPE OF ELEMENT  OR VAR #
;			X - DISPL INTO LINE OF FOR/GOSUB TOKEN
;			CUSET - CARRY SET STACK WAS EMPTY
;			EQ SET - ELEMENT IS GOSUB
;			TSLNUM - LINE #
;---------------------------------------------------------------

XPOP:
POPRSTK:
;---------------------------------------------------------------
;	TEST FOR STACK EMPTY
;---------------------------------------------------------------
		LDA	RUNSTK+1		; GET START OF RUN STACK HIGH
		CMP	TOPRSTK+1		; IS IT < TOP OF STACK HIGH
		BCC	@NTOP			; IF YES, WE ARE NOT AT TOP
		LDA	RUNSTK			; GET START OF RUN STACK LOW
		CMP	TOPRSTK			; IS IT < TOP OF STACK LOW

		.if	BASIC_REVISION = 1
		BCC	@NTOP			; IF YES, WE ARE NOT AT TOP

		SEC				; ELSE AT TOP: SET CARRY
		RTS				; RETURN

;---------------------------------------------------------------
;	GET 4 BYTE HEADER
;		[COMMON TO GOSUB AND FOR]
;---------------------------------------------------------------

@NTOP:		LDA	#GFHEAD			; GET LENGTH OF HEADER
		JSR	@RCONT			; TAKE IT OFF STACK
		.else
		bcs	_FXRTS
@NTOP:		lda	#GFHEAD
		ldx	#TOPRSTK
		jsr	CONTLOW
		.endif

		LDY	#GFDISP			; GET INDEX TO SAVED LINE DISPL
		LDA	(TOPRSTK),Y		; GET SAVED LINE DISPL
		STA	SVDISP			; SAVE
		DEY				; POINT TO LINE # IN HEADER
		LDA	(TOPRSTK),Y		; GET LINE # HIGH
		STA	TSLNUM+1		; SAVE LINE # HIGH
		DEY				; GET DISPL TO LINE # LOW
		LDA	(TOPRSTK),Y		; GET LINE # LOW
		STA	TSLNUM			; SAVE LINE # LOW

		DEY				; POINT TO TYPE
		LDA	(TOPRSTK),Y		; GET TYPE
		BEQ	@FND			; IF TYPE = GOSUB, SET ELEMENT

;---------------------------------------------------------------
;	GET 12 BYTE FOR BODY
;---------------------------------------------------------------
		PHA				; SAVE VAR #
		LDA	#FBODY			; GET # BYTES TO POP

		.if	BASIC_REVISION = 1
		JSR	@RCONT			; POP FROM RUN STACK
		.else
		ldx	#TOPRSTK		; X = PTR TO RUN STACK
		jsr	CONTLOW
		.endif

		PLA				; GET VAR #
@FND:		CLC				; CLEAR CARRY [ENTRY POPPED]
		RTS

		.if	BASIC_REVISION = 1

;---------------------------------------------------------------
; @RCONT -- Contract Run Stack
;---------------------------------------------------------------
;	ON ENTRY	A - # OF BYTES TO SUBTRACT
;---------------------------------------------------------------

@RCONT:		TAY				; Y=LENGTH
		LDX	#TOPRSTK		; X = PTR TO RUN STACK
		JMP	CONTLOW
		.endif

;---------------------------------------------------------------
; _REXPAND -- Expand Run Stack
;---------------------------------------------------------------
;	ON ENTRY	A - # OF BYTES TO ADD
;---------------------------------------------------------------
;	ON EXIT		ZTEMP1 - OLD TOPRSTK
;---------------------------------------------------------------

_REXPAND:	JSR	_SAVRTOP		; SAVE RUN STACK TOP
		TAY				; Y=LENGTH
		LDX	#TOPRSTK		; C=PTR TO TOP RUN STACK
		JMP	EXPLOW			; GO EXPAND

;---------------------------------------------------------------
; _SAVRTOP -- Save Top of Run Stack in ZTEMP1
;---------------------------------------------------------------

_SAVRTOP:	LDX	TOPRSTK			; SAVE TOPRSTK
		STX	TEMPA			; X
		LDX	TOPRSTK+1		; X
		STX	TEMPA+1
		RTS

;---------------------------------------------------------------
; _SAVDEX -- Save Line Displacement
;---------------------------------------------------------------

_SAVDEX:	LDY	STINDEX			; GET STMT INDEX
		STY	SVONTX			; SAVE IT
		RTS

;---------------------------------------------------------------
; _MV6RS -- Move 6-Byte Value to Run Stack
;---------------------------------------------------------------
;	ON ENTRY	X - LOCATION TO MOVE FROM
;			Y- DISPL FROM ZTEMP1 TO MOVE TO
;			ZTEMP1 - LOCATION OF RUN STK ELEMENT
;---------------------------------------------------------------

_MV6RS:		LDA	#6			; GET # OF BYTES TO MOVE
		STA	ZTEMP2			; SAVE AS COUNTER

@MV:		LDA	0,X			; GET A BYTE
		STA	(TEMPA),Y		; PUT ON STACK
		INX				; POINT TO NEXT BYTE
		INY				; POINT TO NEXT LOCATION
		DEC	ZTEMP2			; DEC COUNTER
		BNE	@MV			; IF NOT = 0 DO AGAIN
		RTS

_PL6RS:		LDA	#6			; GET # OF BYTES TO MOVE
		STA	ZTEMP2			; SAVE AS COUNTER
		LDX	#FR1

@PL:		LDA	(MEMTOP),Y		; GET A BYTE
		STA	$0,X			; SAVE IN Z PAGE
		INX				; INC TO NEXT LOCATION
		INY				; INC TO NEXT BYTE
		DEC	ZTEMP2			; DEC COUNTER
		BNE	@PL			; IF NOT =0, DO AGAIN
		RTS

RSTPTR:		LDA	STARP			; GET BASE OF STR/ARRAY SPACE LOW
		STA	RUNSTK			; RESET
		STA	MEMTOP
		STA	APMH			; SET APPLICATION HIMEM
		LDA	STARP+1			; GET BASE STR/ARRAY SPACE HIGH
		STA	RUNSTK+1		; RESET
		STA	MEMTOP+1		; X
		STA	APMH+1			; SET APPLICATION HIMEM
		RTS

;---------------------------------------------------------------
; ZVAR -- Zero Variable
;---------------------------------------------------------------

ZVAR:		LDX	VVTP			; MOVE VARIABLE TABLE POINTER
		STX	ZTEMP1			; X
		LDY	VVTP+1			; X
		STY	ZTEMP1+1		; X

;---------------------------------------------------------------
;	ARE WE AT END OF TABLE ?
;---------------------------------------------------------------

@ZVAR1:		LDX	ZTEMP1+1
		CPX	STMTAB+1
		BCC	@ZVAR2
		LDX	ZTEMP1
		CPX	STMTAB
		BCC	@ZVAR2
		RTS

;---------------------------------------------------------------
;	ZERO A VARIABLE
;---------------------------------------------------------------

@ZVAR2:		LDY	#0			; TURN OFF
		LDA	(ZTEMP1),Y		; DIM FLAG
		AND	#$FE
		STA	(ZTEMP1),Y
		LDY	#2			; INDEX PAST VARIABLE HEADER
		LDX	#6			; GET # OF BYTES TO ZERO
		LDA	#0			; CLEAR A

@ZVAR3:		STA	(ZTEMP1),Y		; ZERO BYTE
		INY				; POINT TO NEXT BYTE
		DEX				; DEC POINTER
		BNE	@ZVAR3			; IF NOT = 0, ZERO NEXT BYTE

		LDA	ZTEMP1			; GET CURRENT VARIABLE POINTER LOW
		CLC
		ADC	#8			; INCR TO NEXT VARIABLE
		STA	ZTEMP1
		LDA	ZTEMP1+1		; GET CURRENT VARIABLE POINTER HIGH
		ADC	#0			; ADD IN CARRY
		STA	ZTEMP1+1		; SAVE NEW VARIABLE POINTER HIGH
		BNE	@ZVAR1			; UNCONDITIONAL BRANCH

;---------------------------------------------------------------
; RUNINIT -- Initialize Storage Locations for RUN
;---------------------------------------------------------------

RUNINIT:
		.if	BASIC_REVISION = 1
		LDY	#0			; CLEAR A
		STY	STOPLN			; CLEAR LINE # STOPPED AT
		STY	STOPLN+1		; X
		STY	ERRNUM			; CLEAR ERROR #
		STY	RADFLG			; CLEAR FLAG FOR TRANSCENDENTALS
		STY	DATAD			; CLEAR DATA POINTERS
		STY	DATALN			; X
		STY	DATALN+1		; X
		DEY
		STY	TRAPLN+1		; SET TRAP FLAG TO NO TRAP
		STY	BRKBYT			; SET BRK BYTE OFF [$FF]
		.else
		; fill B6..BB -> 0
		ldx	#5
		ldy	#0
B8F5:		sty	DATAD,X
		dex
		bpl	B8F5

		sty	$FB
		dey
		sty	$BD
		sty	BRKKEY
		.endif

		jmp	CLSALL

;---------------------------------------------------------------
; TSTEND -- Test for End of Statement
;---------------------------------------------------------------
;	ON EXIT		CC SET
;			CARRY SET - END OF STMT
;			CARRY CLEAR - NOT END OF STMT
;---------------------------------------------------------------

TSTEND:		LDX	STINDEX
		INX
		CPX	NXTSTD
		RTS

		;
		; Error Message Routine
		;

;---------------------------------------------------------------
; Error Messages
;---------------------------------------------------------------

ERRNSF:		INC	ERRNUM		; FILE NOT SAVE FILE		21 = Bad LOAD
ERRDNO:		INC	ERRNUM		; #DN0 > 7			20 = Bad Device #
ERRPTL:		INC	ERRNUM		; LOAD PGM TOO BIG		19 = Insufficient RAM
ERSVAL:		INC	ERRNUM		; STRING NOT VALID		18 = Bad string / val
XERR:		INC	ERRNUM		; EXECUTION OF GARBAGE		17 = Bad line - "ERROR -  "; execute "ERROR-"
ERBRTN:		INC	ERRNUM		; BAD RETURNS			16 = RETURN without GOSUB
ERGFDEL:	INC	ERRNUM		; GOSUB/FOR LINE DELETED	15 = Deleted NEXT/RETURN
ERLTL:		INC	ERRNUM		; LINE TOO LONG			14 = Statement Too Long / Complex
ERNOFOR:	INC	ERRNUM		; NO MATCHING FOR		13 = NEXT without FOR
ERRNOLN:	INC	ERRNUM		; LINE NOT FOUND [GOSUB/GOTO]	12 = Line Not Found
EROVFL:		INC	ERRNUM		; FLOATING POINT OVERFLOW	11 = FP Overflow
ERRAOS:		INC	ERRNUM		; ARG STACK OVERFLOW		10 = Too many GOSUBs
ERRDIM:		INC	ERRNUM		; ARRAY/STRING DIM ERROR	 9 = DIM error
ERRINP:		INC	ERRNUM		; INPUT STMT ERROR		 8 = INPUT, READ type mismatch
ERRLN:		INC	ERRNUM		; VALUE NOT <32768		 7 = Bad Value
ERROOD:		INC	ERRNUM		; READ OUT OF DATA		 6 = Out of DATA
ERRSSL:		INC	ERRNUM		; STRING LENGTH ERROR		 5 = String overflow
ERRVSF:		INC	ERRNUM		; VARIABLE TABLE FULL		 4 = Out of Variables
ERVAL:		INC	ERRNUM		; VALUE ERROR			 3 = Out of Range
MEMFULL:	INC	ERRNUM		; MEMORY FULL			 2 = Memory Full
ERON:		INC	ERRNUM		; NO LINE # FOR EXP IN ON	 1 = Line Not Found for ON EXP

;---------------------------------------------------------------
; Error Routine
;---------------------------------------------------------------

ERROR:		LDA	#0
		STA	DSPFLG			; FLAG
		JSR	STOP			; SET LINE # STOPPED AT

		LDA	TRAPLN+1		; GET TRAP LINE # HIGH
		BMI	@ERRM1			; IF NO LINE # PRINT MESSAGE
		;
		;	TRAP SET - GO TO SPECIFIED LINE
		;
		STA	TSLNUM+1		; SET TRAP LINE # HIGH FOR GET STMT
		LDA	TRAPLN			; GET TRAP LINE # LOW
		STA	TSLNUM			; SET FOR GET STMT
		LDA	#$80			; TURN OFF TRAP
		STA	TRAPLN+1
		LDA	ERRNUM			; GET ERROR #
		STA	ERRSAV			; SAVE IT
		LDA	#0			; CLEAR
		STA	ERRNUM			; ERROR #
		JMP	XGO1			; JOIN GOTO
		;
		;	NO TRAP - PRINT ERROR
		;
@ERRM1:

;---------------------------------------------------------------
; Print Error Message Part 1 [**ERR]
;---------------------------------------------------------------
		JSR	PRCR			; PRINT CR
		LDA	#kERROR			; GET TOKEN FOR ERROR
		JSR	LSTMC			; GO PRINT CODE

;---------------------------------------------------------------
; Print Error Number
;---------------------------------------------------------------
		LDA	ERRNUM			; GET ERROR #
		STA	FR0			; SET ERROR # OF FR0 AS INTEGER
		LDA	#0			; SET ERROR # HIGH
		STA	FR0+1			; X

		JSR	PRINUM			; GO PRINT ERROR #

_ERRM2:		JSR	TENDST			; TEST FOR DIRECT STMT
		BMI	@ERRDONE		; IF DIRECT STMT, DONE

;---------------------------------------------------------------
; Print Message Part 2 [AT LINE]
;---------------------------------------------------------------
		LDA	#<ATLINE_STR		; SET POINTER TO MSG FOR PRINT
		STA	POKADR			; X
		LDA	#>ATLINE_STR		; X
		STA	POKADR+1		; X

		JSR	LPRTOKEN

;---------------------------------------------------------------
; Print Line Number
;---------------------------------------------------------------
		LDY	#1			; SET DISPL
		LDA	(STMCUR),Y		; GET LINE # HIGH
		STA	FR0+1			; SET IN FR0 FOR CONVERT
		DEY				; GET CURRENT LINE # LOW
		LDA	(STMCUR),Y		; GET UNUSED LINE # LOW
		STA	FR0			; SET IN FR0 LOW FOR CONVERT

		JSR	PRINUM			; PRINT LINE #

@ERRDONE:	JSR	PRCR			; PRINT CR
		LDA	#0			; CLEAR A
		STA	ERRNUM			; CLEAR ERROR #
		.if	BASIC_REVISION > 1
		JSR	SETDZ
		.endif
		jmp	SYNTAX

;---------------------------------------------------------------
; Print Integer Number in FR0
;---------------------------------------------------------------

PRINUM:		JSR	CVIFP			; CONVERT TO FLOATING POINT
		JSR	CVFASC			; CONVERT TO ASCII

		LDA	INBUFF			; GET ADR OF # LOW
		STA	POKADR			; SET FOR PRINT ROUTINE
		LDA	INBUFF+1		; GET ADR OF # HIGH
		STA	POKADR+1		; SET FOR PRINT ROUTINE
		.if	BASIC_REVISION = 1
		JSR	LPRTOKEN		; GO PRINT ERROR #
		RTS
		.else
		jmp	LPRTOKEN		; GO PRINT ERROR #
		.endif

ATLINE_STR:	BasicString " AT LINE "

;---------------------------------------------------------------
; XSETCOLOR -- Execute SETCOLOR
;---------------------------------------------------------------

XSETCOLOR:	JSR	GET1INT			; GET REGISTER #
		LDA	FR0			; GET #
		CMP	#5			; IS IT <5?
		BCS	@ERCOL			; IF NOT, ERROR
		PHA				; SAVE

		JSR	GETINT			; GET VALUE

		LDA	FR0			; GET VALUE*16+6
		ASL				; X
		ASL				; X
		ASL				; X
		ASL				; X
		PHA				; SAVE ON STACKS
		JSR	GETINT			; GET VALUE 3
		PLA				; GET VALUE 2*16 FROM STACK
		CLC
		ADC	FR0			; ADD IN VALUE 3
		TAY				; SAVE VAUE 2*16 + VALUE 5
		PLA				; GET INDEX
		TAX				; PUT IN X
		TYA				; GET VALUE

		STA	CREGS,X			; SET VALUE IN REGS
		RTS

@ERCOL:
_ERSND:		JSR	ERVAL

;---------------------------------------------------------------
; XSOUND -- Execute SOUND
;---------------------------------------------------------------

XSOUND:		JSR	GET1INT			; GET 1 BYTE INTEGER
		LDA	FR0			; X
		CMP	#4			; IS IT <4?
		BCS	_ERSND			; IF NOT, ERROR
		ASL				; GET VALUE *2
		PHA

		LDA	#0			; SET TO ZERO
		STA	SREG1			; X

		LDA	#3			; SKCTL_KEYBOARD_DEBOUNCE + SKCTL_KEYBOARD_SCANNING
		STA	SKCTL

		JSR	GETINT			; GET EXP2
		PLA				; GET INDEX
		PHA				; SAVE AGAIN
		TAX				; PUT IN INDEX REG
		LDA	FR0			; GET VALUE
		STA	SREG2,X			; SAVE IT

		JSR	GETINT			; GET EXP3
		LDA	FR0			; GET 16*EXP3
		ASL				; X
		ASL				; X
		ASL				; X
		ASL				; X
		PHA				; SAVE IT

		JSR	GETINT			; GET EXP4
		PLA				; GET 16*EXP3
		TAY				; SAVE IT
		PLA				; GET INDEX
		TAX				; PUT IN X
		TYA				; GET EXP3*16
		CLC
		ADC	FR0			; GET 16*EXP3+EXP4
		STA	SREG3,X			; STORE IT
		RTS

;---------------------------------------------------------------
; XPOS -- Execute POSITION
;---------------------------------------------------------------

XPOS:		JSR	GETINT			; GET INTEGER INTO FR0
		LDA	FR0			; SET X VALUE
		STA	SCRX			; X
		LDA	FR0+1			; X
		STA	SCRX+1			; X

		JSR	GET1INT			; SET Y VALUE
		LDA	FR0			; X
		STA	SCRY			; X
		RTS

;---------------------------------------------------------------
; XCOLOR -- Execute COLOR
;---------------------------------------------------------------

XCOLOR:		JSR	GETINT			; GET INTEGER INTO FR0
		LDA	FR0
		STA	COLOR
		RTS

;---------------------------------------------------------------
; XDRAWTO -- Execute DRAWTO
;---------------------------------------------------------------

XDRAWTO:	JSR	XPOS			; GET X,Y POSITION
		LDA	COLOR			; GET COLOR
		STA	SVCOLOR			; SET IT

		LDA	#ICDRAW			; GET COMMAND
		LDX	#6			; SET DEVICE
		JSR	GLPCX			; SET THEM

		LDA	#$0C			; SET AUX 1
		STA	ICAUX1,X
		LDA	#0			; SET AUX 2
		STA	ICAUX2,X
		JSR	IO7
		JMP	IOTEST

;---------------------------------------------------------------
; XGR -- Execute GRAPHICS
;---------------------------------------------------------------

XGR:		LDX	#6			; GET DEVICE
		STX	IODVC			; SAVE DEVICE
		JSR	CLSYS1			; GO CLOSE IT
		JSR	GETINT			; GET INTEGER INTO FR0

		LDXY	SSTR			; SET INBUFF TO POINT TO FILE SPEC STRING
		STX	INBUFF			; X
		STY	INBUFF+1		; X

		LDX	#6			; GET DEVICE #
		LDA	FR0			; SET SOME BITS FOR GRAPHICS
		AND	#$F0
		EOR	#ICGR
		TAY
		LDA	FR0			; GET AUX2 [GRAPHICS TYPE]
		JSR	SOPEN			; OPEN
		JMP	IOTEST			; TEST I/O OK

SSTR:		.byte	"S:",CR

;---------------------------------------------------------------
; XPLOT -- Execute PLOT
;---------------------------------------------------------------

XPLOT:		jsr	XPOS			; SET X,Y POSITION
		lda	COLOR			; GET COLOR
		ldx	#6			; GET DEVICE #
		jmp	PRCX			; GO PRINT IT

;---------------------------------------------------------------
; GETLINE -- Get a Line of Input
;	GLINE - GET LINE [PROMPT ONLY]
;	GNLINE - GET NEW LINE [CR, PROMPT]
;---------------------------------------------------------------

		.if	BASIC_REVISION = 1

		LDX	ENTDTD			; IF ENTER DEVICE NOT ZERO
		BNE	GLGO			; THEN DO PROMPT
		LDA	#CR			; PUT EOL
		JSR	PUTCHAR

GLINE:		LDX	ENTDTD			; IF ENTER DEVICE NOT ZERO
		BNE	GLGO			; THEN DON'T PROMPT
		LDA	PROMPT			; PUT PROMPT
		JSR	PUTCHAR

GLGO:		LDX	ENTDTD
		LDA	#ICGTR
		JSR	GLPCX
		JSR	IO1			; GO DO I/O
		JMP	IOTEST			; GO TEST RESULT

		.else

GNLINE:		sec				; SET CARRY FOR SUBTRACT
		lda	0,X			; GET BYTE AT X
		and	#$7F			; CLEAR MSB
		sbc	#$40			; < 64 ?
		bcc	@rts			; DONE
		sta	ZTEMP1			; ZTEMP1 = 00...3F
		sta	ZTEMP4			; ZTEMP4 = 00...3F
		txa				; A = X + ZTEMP1
		adc	ZTEMP1
		inx				; X += 6
		inx
		inx
		inx
		inx
		inx
		stx	ZTEMP1			; ZTEMP1 = X

		tax				; X = A
@loop:		inx				; IF ++X > ZTEMP1
		cpx	ZTEMP1
		bcs	@rts			; THEN DONE
		lda	0,X			; IF BYTE AT X == 0: GOTO LOOP
		beq	@loop
@rts:		rts

		.endif

;---------------------------------------------------------------
; PUTCHAR -- Put One Character to List Device
;---------------------------------------------------------------

PRCHAR:
PUTCHAR:
		LDX	LISTDTD			; GET LIST DEVICE
PRCX:		PHA				; SAVE IO BYTE
		JSR	GLPX			; SET DEVICE

		LDA	ICAUX1,X		; SET UP ZERO PAGE IOCB
		STA	ICAUX1-IOCB+ZICB	; X
		LDA	ICAUX2,X		; X
		STA	ICAUX2-IOCB+ZICB	; X

		PLA
		TAY
		JSR	@PDUM
;	RETURN HERE FROM ROUTINE
		TYA				; TEST STATUS
		JMP	IOTES2

;; PUSH(ICPUT[X])

@PDUM:		LDA	ICPUT+1,X		; GO TO PUT ROUTINE
		PHA				; X
		LDA	ICPUT,X			; X
		PHA				; X
		TYA				; X
		LDY	#$92			; LOAD VALUE FOR CIO ROUTINE
		RTS

;; LDDVX(cmd=A, dvc=X)

GLPCX:		STA	IOCMD
GLPX:		STX	IODVC			; AS I/O DEVICE
		JMP	LDDVX			; LOAD DEVICE X

;---------------------------------------------------------------
; XENTER -- Execute ENTER
;---------------------------------------------------------------

XENTER:		LDA	#$04			; OPEN INPUT
		JSR	ELADVC			; GO OPEN ALT DEVICE
		STA	ENTDTD			; SET ENTER DEVICE
		JMP	SYNTAX

;---------------------------------------------------------------
; FLIST -- Open LIST File
;---------------------------------------------------------------

FLIST:		LDA	#$08			; OPEN OUTPUT
		JSR	ELADVC			; GO OPEN ALT DEVICE
		STA	LISTDTD			; SET LIST DEVICE
		RTS				; DONE

ELADVC:		pha
		ldy	#7			; USE DEVICE 7
		sty	IODVC			; SET DEVICE

		jsr	LDDVX			; BEFORE
		lda	#ICCLOSE		; GO CLOSE DEVICE
		jsr	IO8			; OPEN OF NEW ONE

		ldy	#ICOIO			; CMD IS OPEN
		sty	IOCMD
		pla
		ldy	#0			; GET AUX2
		jsr	XOP2			; GO OPEN
		lda	#7			; LOAD DEVICE
		rts				; AND RETURN

		.if	BASIC_REVISION > 1
FTWO:		.byte	$40,$02,$00,$00,$00,$00 ; 2.0
		.endif

;---------------------------------------------------------------
; RUN from File
;---------------------------------------------------------------

FRUN:		LDA	#$FF			; SET RUN MODE
		BNE	_LD0

;---------------------------------------------------------------
; XLOAD -- Execute LOAD Command
;---------------------------------------------------------------

XLOAD:		LDA	#0			; SET LOAD MODE
_LD0:		PHA				; SAVE R/L TYPE
		LDA	#$04			; GO OPEN FOR INPUT
		JSR	ELADVC			; THE SPECIFIED DEVICE
		PLA				; GET THE R/L TYPE

XLOAD1:		PHA				; SAVE R/L TYPE
		LDA	#ICGTC			; CMD IS GET TEXT CHARS
		STA	IOCMD
		STA	LOADFLG			; SET LOAD IN PROGRESS

		JSR	LDDVX			; LOAD DEVICE X REG
		LDY	#ENDSTAR-OUTBUFF	; Y=REC LENGTH
		JSR	IO3
		JSR	IOTEST
		LDA	MISCRAM+OUTBUFF
		ORA	MISCRAM+OUTBUFF+1
		BNE	@LDFER

		LDX	#STARP			; START AT STARP DISPL
@LD1:		CLC
		LDA	OUTBUFF			; ADD LOMEM TO
		ADC	MISCRAM,X		; LOAD TABLE DISPL

		.if	BASIC_REVISION > 1
		php
		clc
		.if	BASIC_REVISION > 2
		adc	#0
		.else
		adc	#$10
		.endif
		.endif

		TAY
		LDA	OUTBUFF+1
		ADC	MISCRAM+1,X

		.if	BASIC_REVISION > 1
		plp
		adc	#0
		.endif

		CMP	HIMEM+1			; IF NEW VALUE NOT
		BCC	@LD3			; LESS THEN HIMEM
		BNE	@LD2			; THEN ERROR
		CPY	HIMEM
		BCC	@LD3
@LD2:		JMP	ERRPTL

@LD3:		STA	1,X			; ELSE SET NEW TABLE VALIUE
		STY	0,X
		DEX				; DECREMENT TO PREVIOUS TBL ENTRY
		DEX
		CPX	#VNTP			; IF NOT AT LOWER ENTRY
		BCS	@LD1			; THEN CONTINUE

		JSR	_LSBLK			; LOAD USER AREA
		JSR	XCLR			; EXECUTE CLEAR
		LDA	#0			; RESET LOAD IN PROGRESS
		STA	LOADFLG			; X
		PLA				; LOAD R/S STATUS
		BEQ	@LD4			; BR IF LOAD
		RTS				; RETURN TO RUN

@LD4:		JMP	SNX1			; GO TO SYNTAX

@LDFER:		LDA	#0			; RESET LOAD IN PROGRESS
		STA	LOADFLG			; X
		JSR	ERRNSF			; NOT SAVE FILE

		.if	BASIC_REVISION > 1

;---------------------------------------------------------------
; XCLOAD -- Execute CLOAD (Rev.2+)
;---------------------------------------------------------------

XCLOAD:		lda	#$04
		jsr	_COPEN
		lda	#0
		beq	XLOAD1
		.endif

;---------------------------------------------------------------
; XSAVE -- Execute SAVE Command
;---------------------------------------------------------------

XSAVE:		LDA	#$08			; GO OPEN FOR OUTPUT
		JSR	ELADVC			; THE SPECIFIED DEVICE

XSAVE1:		LDA	#ICPTC			; I/O CMD IS PUT TEXT CHARS
		STA	IOCMD			; SET I/O CMD

		LDX	#OUTBUFF		; MOVE RAM TABLE PTRS
@SV1:		SEC				; [OUTBUFF THRU ENSTAR]
		LDA	0,X			; TO LBUFF
		SBC	OUTBUFF			; AS DISPLACEMENT
		STA	MISCRAM,X		; FROM LOW MEM
		INX
		LDA	0,X
		SBC	OUTBUFF+1
		STA	MISCRAM,X
		INX
		CPX	#$8E
		BCC	@SV1

		JSR	LDDVX			; OUTPUT LBUFF
		LDY	#ENDSTAR-OUTBUFF	; FOR PROPER LENGTH
		JSR	IO3
		JSR	IOTEST			; TEST GOOD I/O

;---------------------------------------------------------------
; _LSBLK -- LOAD or SAVE User Area as a Block
;---------------------------------------------------------------

_LSBLK:		JSR	LDDVX			; LOAD DEVICE X REG
		LDA	VNTP			; SET VAR NAME TBL PTR
		STA	INBUFF			; AS START OF BLOCK ADR
		LDA	VNTP+1
		STA	INBUFF+1
		LDY	MISCRAM+STARP+1		; A,Y = BLOCK LENGTH
		DEY
		TYA
		LDY	MISCRAM+STARP
		JSR	IO4			; GO DO BLOCK I/O
		JSR	IOTEST
		JMP	CLSYS1			; GO CLOSE DEVICE

		.if	BASIC_REVISION = 1

;---------------------------------------------------------------
; XCSAVE -- Execute CSAVE (Rev.1)
;---------------------------------------------------------------

XCSAVE:		LDA	#8			; SET OPEN FOR OUTPUT
		JSR	COPEN			; OPEN CASSETTE

		JMP	XSAVE1			; DO SAVE


;---------------------------------------------------------------
; XCLOAD -- Execute CLOAD (Rev.1)
;---------------------------------------------------------------

XCLOAD:		LDA	#4			; GET OPEN FOR INPUT
		JSR	COPEN			; OPEN CASSETTE
		LDA	#0			; GET LOAD TYPE
		JMP	XLOAD1			; DO LOAD

		.endif

;---------------------------------------------------------------
; COPEN -- OPEN Cassette
;---------------------------------------------------------------
;	ON ENTRY:	A - TYPE OF OPEN [IN OR OUT]
;	ON EXIT:	A - DEVICE #7
;---------------------------------------------------------------
		.if	BASIC_REVISION > 1
_COPEN:		nop
		nop
		.endif
COPEN:		PHA
		LDX	#<@CSTR
		STX	INBUFF
		LDX	#>@CSTR
		STX	INBUFF+1

		LDX	#7
		PLA
		TAY				; SET COMMAND TYPE
		LDA	#$80			; GET AUX 2

		JSR	SOPEN			; GO OPEN
		JSR	IOTEST
		LDA	#7			; GET DEVICE
		RTS
@CSTR:
		.byte	"C:",CR

		.if	BASIC_REVISION > 1

;---------------------------------------------------------------
; XCSAVE -- Execute CSAVE (Rev.2+)
;---------------------------------------------------------------

XCSAVE:		lda	#8
		jsr	_COPEN
		bne	XSAVE1
		.endif

;---------------------------------------------------------------
; SOPEN -- OPEN System Device
;---------------------------------------------------------------
;	ON ENTRY	X - DEVICE
;			Y - AUX1
;			A - AUX2
;			INBUFF - POINTS TO FILE SPEC
;---------------------------------------------------------------

SOPEN:		PHA				; SAVE AUX2
		LDA	#ICOIO			; GET COMMAND
		JSR	GLPCX			; GET DEVICE/COMMAND
		PLA				; SET AUX2 & AUX 1
		STA	ICAUX2,X		; X
		TYA
		STA	ICAUX1,X

		JSR	IO5			; DO COMMAND
		JMP	INTLBF			; RESET INBUFF

;---------------------------------------------------------------
; XXIO -- Execute XIO Statement
;---------------------------------------------------------------

XXIO:		JSR	GIOCMD			; GET THE COMMAND BYTE
		JMP	XOP1			; CONTINUE AS IF OPEN

;---------------------------------------------------------------
; XOPEN -- Execute OPEN Statement
;---------------------------------------------------------------

XOPEN:		LDA	#ICOIO			; LOAD OPEN CODE
XOP1:		STA	IOCMD
		JSR	GIODVC			; GET DEVICE
		JSR	GIOCMD			; GET AUX1
		PHA
		JSR	GIOCMD			; GET AUX2
		TAY				; AUX2 IN Y
		PLA				; AUX1 IN A

XOP2:		PHA				; SAVE AUX1
		TYA
		PHA				; SAVE AUX2

		JSR	EXEXPR			; GET FS STRING
		JSR	SETSEOL			; GIVE STRING AN EOL
		JSR	LDDVX			; LOAD DEVICE X REG
		PLA
		STA	ICAUX2,X		; SET AUX2
		PLA				; GET AUX1
		STA	ICAUX1,X
		JSR	IO1			; GO DO I/O

		JSR	RSTSEOL			; RESTORE STRING EOL
		JSR	INTLBF
		JMP	IOTEST			; GO TEST I/O STATUS

;---------------------------------------------------------------
; XCLOSE -- Execute CLOSE
;---------------------------------------------------------------

XCLOSE:		LDA	#ICCLOSE		; CLOSE CMD

;---------------------------------------------------------------
; GDVCIO -- General Device I/O
;---------------------------------------------------------------

GDVCIO:		STA	IOCMD			; SET CMD
		JSR	GIODVC			; GET DEVICE
GDIO1:		JSR	IO7			; GO DO I/O
		JMP	IOTEST			; GO TEST STATUS

;---------------------------------------------------------------
; XTATUS -- Execute STATUS
;---------------------------------------------------------------

XSTATUS:	JSR	GIODVC			; GET DEVICE
		LDA	#ICSTAT			; STATUS CMD
		JSR	IO8			; GO GET STATUS
		JSR	LDIOSTA			; LOAD STATUS
		JMP	ISVAR1			; GO SET VAR

;---------------------------------------------------------------
; XNOTE -- Execute NOTE
;---------------------------------------------------------------

XNOTE:		LDA	#NOTE			; NOTE CMD
		JSR	GDVCIO			; GO DO
		LDA	ICAUX3,X		; GET SECTOR N/. LOW
		LDY	ICAUX4,X		; AND HI
		JSR	ISVAR			; GO SET VAR
		JSR	LDDVX			; GET DEVICE X REG
		LDA	ICAUX5,X		; GET DATA LENGTH
		JMP	ISVAR1			; GO SET VAR

;---------------------------------------------------------------
; XPOINT -- Execute POINT
;---------------------------------------------------------------

XPOINT:		JSR	GIODVC			; GET I/O DEVICE NUM
		JSR	GETPINT			; GET SECTOR NUM
		JSR	LDDVX			; GET DEVICE X
		LDA	FR0			; SET SECTOR NUM
		STA	ICAUX3,X
		LDA	FR0+1
		STA	ICAUX4,X
		JSR	GETPINT			; GET DATA LENGTH
		JSR	LDDVX			; LOAD DEVICE X
		LDA	FR0			; GET AL
		STA	ICAUX5,X		; SET DATA LENGTH
		LDA	#POINT			; SET POINT CMD
		STA	IOCMD
		.if	BASIC_REVISION = 1
		JMP	GDIO1			; GO DO
		.else
		bne	GDIO1
		.endif

;---------------------------------------------------------------
; XPUT -- Execute PUT
;---------------------------------------------------------------

XPUT:		JSR	GIODVC			; GET DEVICE #

		JSR	GETINT			; GET DATA
		LDA	FR0			; X
		LDX	IODVC			; LOAD DEVICE #
		JMP	PRCX			; GO PRINT

;---------------------------------------------------------------
; XGET -- Execute GET
;---------------------------------------------------------------

XGET:		.if	BASIC_REVISION > 1
		jsr	INTLBF
		.endif
		JSR	GIODVC
GET1:		LDA	#ICGTC			; GET COMMAND
		STA	IOCMD			; SET COMMAND
		LDY	#1
		JSR	IO3			; DO IO
		JSR	IOTEST			; TEST I/O
		LDY	#0			; GET CHAR
		LDA	(INBUFF),Y		; X
		JMP	ISVAR1			; ASSIGN VAR

;---------------------------------------------------------------
; XLOCATE -- Execute LOCATE
;---------------------------------------------------------------

XLOCATE:	JSR	XPOS			; GET X,Y POSITION
		LDX	#6			; GET DEVICE #
		JSR	GLPX			; X

		BNE	GET1			; GO GET

;---------------------------------------------------------------
; GIODVC -- Get I/O Device Number
;---------------------------------------------------------------

GIODVC:		JSR	GIOPRM			; GET PARM
		STA	IODVC			; SET AS DEVICE
		BEQ	DNERR			; BR IF DVC=0

;---------------------------------------------------------------
; LDDVX -- Load X Register with I/O Device Offset
;---------------------------------------------------------------

LDDVX:		LDA	IODVC			; GET DEVICE
		ASL				; MULT BY 16
		ASL
		ASL
		ASL
		TAX				; PUT INTO X
		.if	BASIC_REVISION = 1
		BMI	DNERR			; BR DN0>7
		RTS				; AND RETURN
		.else
		bpl	NOCD0
		.endif

DNERR:		JSR	ERRDNO

;---------------------------------------------------------------
; IOTEST -- Test I/O Status
;---------------------------------------------------------------

IOTEST:		JSR	LDIOSTA			; LOAD I/O STATUS
IOTES2:		.if	BASIC_REVISION = 1
		BMI	@SICKIO			; BR IF BAD
		RTS				; ELSE RETURN
		.else
		bpl	NOCD0
		.endif

@SICKIO:	LDY	#0			; RESET DISPLAY FLAG
		STY	DSPFLG

		CMP	#ICSBRK			; IF BREAK
		BNE	@SIO1			; SIMULATE ASYNC
		STY	BRKBYT			; BREAK
		LDA	LOADFLG			; IF LOAD FLAG SET
		.if	BASIC_REVISION = 1
		BEQ	@SIOS
		.else
		beq	NOCD0
		.endif
		JMP	COLDSTART		; DO COLDSTART

		.if	BASIC_REVISION = 1
@SIOS:		RTS
		.endif

@SIO1:		LDY	IODVC			; PRE-LOAD I/O DEVICE
		CMP	#EOFERR			; WAS ERROR EOF
		BEQ	@SIO4			; BR IF EOF
@SIO2:		STA	ERRNUM			; SET ERROR NUMBER

		CPY	#7			; WAS THIS DEVICE #7
		BNE	@SIO3			; BR IF NOT
		JSR	CLSYSD			; CLOSE DEVICE 7

@SIO3:		JSR	SETDZ			; SET L/D DEVICE = 0
		JMP	ERROR			; REPORT ERROR

@SIO4:		CPY	#7			; WAS EOF ON DEVICE 7
		BNE	@SIO2			; BR IF NOT
		LDX	#EPCHAR			; WERE WE IN ENTER
		CPX	PROMPT
		BNE	@SIO2			; BR NOT ENTER
		JSR	CLSYS1			; CLOSE DEVICE 7
		JMP	SNX2			; GO TO SYNTAX

;---------------------------------------------------------------
; CLSYSD -- Close System Device
;---------------------------------------------------------------

CLSYSD:
CLSYS1:		JSR	LDDVX
		BEQ	NOCD0			; DON'T CLOSE DEVICE0
		LDA	#ICCLOSE		; LOAD CLOSE CORD
		.if	BASIC_REVISION = 1
		JMP	IO8			; GO CLOSE
		.else
		bne	IO8
		.endif

;---------------------------------------------------------------
; LDIOSTA -- Load I/O Status
;---------------------------------------------------------------

LDIOSTA:	JSR	LDDVX			; GET DEVICE X REG
		LDA	ICSTA,X			; GET STATUS
NOCD0:		RTS				; RETURN

;---------------------------------------------------------------
; GIOPRM -- Get I/O Parameters
;---------------------------------------------------------------

GIOPRM:		INC	STINDEX			; SKIP OVER #
GIOCMD:		JSR	GETPINT			; GET POSITIVE INT
		LDA	FR0			; MOVE LOW BYTE TO
		RTS

;---------------------------------------------------------------
; I/O Call Routine
;---------------------------------------------------------------

IO1:		LDY	#$FF			; BUFL = 255
		BNE	IO3
		LDY	#0			; BUFL = 0
IO3:		LDA	#0			; BUFL < 256
IO4:		STA	ICBLH,X			; SET BUFL
		TYA
		STA	ICBLL,X
IO5:		LDA	INBUFF+1		; LOAD INBUFF VALUE
		LDY	INBUFF
		STA	ICBAH,X			; SET BUF ADR
		TYA
		STA	ICBAL,X
IO7:		LDA	IOCMD			; LOAD COMMAND
IO8:		STA	ICCOM,X			; SET COMMAND
		.if	BASIC_REVISION = 1
		JSR	CIOV			; GO DO I/O
		RTS
		.else
		jmp	CIOV
		.endif

;---------------------------------------------------------------
; ISVAR -- I/O Variable Set
;---------------------------------------------------------------

ISVAR1:		LDY	#0			; GET HIGH ORDER BYTE
ISVAR:		PHA				; PUSH INT VALUE LOW
		TYA
		PHA				; PUSH INT VALUE HI
		JSR	POP1			; GET VARIABLE
		PLA
		STA	FR0+1			; SET VALUE LOW
		PLA
		STA	FR0			; SET VALUE HI
		JSR	IFP			; CONVERT TO FP
		JMP	RTNVAR			; AND RETURN TO TABLE

;---------------------------------------------------------------
; CLSALL -- CLOSE all IOCBs [except 0]
;---------------------------------------------------------------

CLSALL:

;---------------------------------------------------------------
; TURN OFF SOUND
;---------------------------------------------------------------
		LDA	#0
		LDX	#7
@CL:		STA	SREG3-1,X
		DEX
		BNE	@CL

		LDY	#7			; START AT DEVICE 7
		STY	IODVC
@CLALL1:	JSR	CLSYSD			; CLOSE DEVICE
		DEC	IODVC			; DEV DEVICE #
		BNE	@CLALL1			; BR IF NOT ZERO
		RTS

		.if	BASIC_REVISION > 1

;---------------------------------------------------------------
; SETDZ -- Set Device 0 as LIST/ENTER Device (Rev.2+)
;---------------------------------------------------------------

SETDZ:		lda	#0
		sta	ENTDTD
		sta	LISTDTD
		rts
		.endif

;---------------------------------------------------------------
; PREADY -- Print READY Message
;---------------------------------------------------------------

PREADY:		ldx	#RML-1			; GET READY MSG LENGTH-1
PRDY1:		stx	CIX			; SET LEN REM
		lda	RMSG,X			; GET CHAR
		jsr	PRCHAR			; PRINT IT
		ldx	CIX			; GET LENGTH
		dex
		bpl	PRDY1			; BR IF MORE
		rts
RMSG:		.byte	CR,"YDAER",CR
RML		EQU	*-RMSG

;---------------------------------------------------------------
; PRCR -- Print Carriage Return
;---------------------------------------------------------------

PRCR:		ldx	#0			; SET FOR LAST CHAR
		beq	PRDY1			; AND GO DO IT

		.if	BASIC_REVISION = 1

;---------------------------------------------------------------
; SETDZ -- Set Device 0 as LIST/ENTER Device (Rev.1)
;---------------------------------------------------------------

SETDZ:		LDA	#0
		STA	ENTDTD
		STA	LISTDTD
		RTS
		.endif

;---------------------------------------------------------------
; SETSEOL -- Set an EOL [Temporarily] after a String EOL
;---------------------------------------------------------------

SETSEOL:	JSR	AAPSTR			; GET STRING WITH ABS ADR
		LDA	VTYPE+EVSADR		; PUT ITS ADR
		STA	INBUFF			; INTO INBUFF
		LDA	VTYPE+EVSADR+1
		STA	INBUFF+1

		LDY	VTYPE+EVSLEN		; GET LENGTH LOW
		LDX	VTYPE+EVSLEN+1		; IF LEN < 256
		BEQ	@SSE1			; THEN BR
		LDY	#$FF			; ELSE SET MAX

@SSE1:		LDA	(INBUFF),Y		; GET LAST STR CHAR+1
		STA	INDEX2			; SAVE IT
		STY	INDEX2+1		; AND ITS INDEX
		LDA	#CR			; THEN REPLACE WITH EOL
		STA	(INBUFF),Y
		STA	MEOLFLG			; INDICATE MODIFIED EOL
		RTS				; DONE

RSTSEOL:					; RESTORE STRING CHAR
		LDY	INDEX2+1		; LOAD INDEX
		LDA	INDEX2			; LOAD CHAR
		STA	(INBUFF),Y		; DONE
		LDA	#0
		STA	MEOLFLG			; RESET EOL FLAG
		RTS				; DONE

		.if	BASIC_REVISION = 1

PATCH:		.byte	$20			; DS PATSIZ

		.else

;---------------------------------------------------------------
; XRTN -- Execute RETURN (Rev.2+)
;---------------------------------------------------------------

XRTN:		jsr	XPOP
		bcs	_XRTN2
		bne	XRTN
		jsr	_XRTN3
		cmp	#12
		beq	_XRTNX
		cmp	#$1E
		beq	_XRTNX
		cmp	#4
		beq	_XRTNX
		cmp	#$22
		beq	_XRTNX

_XRTN1:		jsr	RESCUR
		jsr	ERGFDEL

_XRTN2:		jsr	ERBRTN

_XRTN3:		jsr	SETLINE
		bcs	_XRTN1
		ldy	SVONTL
		dey
		lda	(STMCUR),Y
		sta	NXTSTD
		iny
		lda	(STMCUR),Y
_XRTNX:		rts

		ldx	ENTDTD
		bne	GLGO2
		lda	#CR
		jsr	PRCHAR
_GLINE2:	ldx	ENTDTD
		bne	GLGO2
		lda	PROMPT
		jsr	PRCHAR
GLGO2:		ldx	ENTDTD
		lda	#ICGTR
		jsr	GLPCX
		jsr	IO1
		jmp	IOTEST

XPPLUS:		jsr	ARGP2
		jsr	FRADD
		jmp	ARGPUSH

		.endif

SINERR:		SEC				; ERROR - SET CARRY
		RTS

SIN:		LDA	#4			; FLAG SIN[X] ENTRY RIGHT NOW
		BIT	FR0
		BPL	BOTH
		LDA	#2			; SIN[-X]
		BNE	BOTH
COS:		LDA	#1			; FLAG COS[X] ENTRY
BOTH:		STA	SGNFLG
		LDA	FR0			; FORCE POSITIVE
		AND	#$7F
		STA	FR0
		LDA	#<PIOV2
		CLC
		ADC	DEGFLG
		TAX
		LDY	#>PIOV2
		JSR	FLD1R
		JSR	FDIV			; X/[PI/2] OR X/90
		BCC	@SINF7
@SINOVF:	RTS				; OVERFLOW
@SINF7:
		LDA	FR0
		AND	#$7F			; CHECK EXPONENT
		SEC
		SBC	#$40
		BMI	@SINF3			; QUADRANT 0 - USE AS IS
		CMP	#FPREC-2		; FIND QUAD NUM & REMAINDER
		BPL	SINERR			; OUT OF RANGE
		TAX				; X->LSB OR FR0
		LDA	FR0+1,X			; LSB
		STA	XFMFLG
		AND	#$10			; CHECK 10'S DIGIT
		BEQ	@SINF5
		LDA	#2			; ODD - ADD 2 TO QUAD #
@SINF5:		CLC
		ADC	XFMFLG
		AND	#3			; QUADRANT = 0,1,2,3
		ADC	SGNFLG			; ADJUST FOR SINE VS COSINE
		STA	SGNFLG
		STX	XFMFLG			; SAVE DEC PT LOC
		JSR	FMOVE			; COPY TO FR1
		LDX	XFMFLG
		LDA	#0
@SINF1:		STA	FR1+2,X			; CLEAR FRACTION
		INX
		CPX	#FPREC-3
		BCC	@SINF1
		JSR	FSUB			; LEAVE REMAINDER
@SINF3:		LSR	SGNFLG			; WAS QUAD ODD
		BCC	@SINF4			; NO
		JSR	FMOVE			; YES - USE 1.0 - REMAINDER
		LDXY	FPONE,FLD0R
		JSR	FSUB
@SINF4:						; NOW DO THE SERIES THING
		ldx	#<FPSCR			; SAVE ARG
		ldy	#>FPSCR
		jsr	FSTOR
		jsr	FMOVE			; X->FR1
		jsr	FMUL			; X**2->FR0
		bcs	SINERR
		lda	#NSCF
		ldx	#<SCOEF
		ldy	#>SCOEF
		jsr	PLYEVL			; EVALUATE P[X**2]
		ldx	#<FPSCR
		ldy	#>FPSCR
		jsr	FLD1R			; X-> FR1
		jsr	FMUL			; SIN[X] = X*P[X**2]
		lsr	SGNFLG			; WAS QUAD 2 OR 3?
		bcc	SINDON			; NO - THRU
		clc				; YES
		lda	FR0			; FLIP SIGN
		beq	SINDON			; [UNLESS ZERO]
		eor	#$80
		sta	FR0
SINDON:		rts				; RETURN

SCOEF:		.byte	$BD,$03,$55,$14,$99,$39 ; -.00000355149939
		.byte	$3E,$01,$60,$44,$27,$52 ; 0.000160442752
		.byte	$BE,$46,$81,$75,$43,$55 ; -.004681754355
		.byte	$3F,$07,$96,$92,$62,$39 ; 0.0796926239
		.byte	$BF,$64,$59,$64,$08,$67 ; -.645964867
PIOV2:		.byte	$40,$01,$57,$07,$96,$32	; PI/2  1.57079632
NSCF		EQU	(*-SCOEF)/FPREC
		.byte	$40,$90,$00,$00,$00,$00	; 90.00000000 DEG
PIOV18:		.byte	$3F,$01,$74,$53,$29,$25	; PI/180
FPONE:		.byte	$40,$01,$00,$00,$00,$00	;  1.0

;---------------------------------------------------------------
; ATAN[X] -- Arctangent
;---------------------------------------------------------------

ATAN:		LDA	#0			; ARCTAN[X]
		STA	SGNFLG			; SIGN FLAG OFF
		STA	XFMFLG			; & TRANSFORM FLAG
		LDA	FR0
		AND	#$7F
		CMP	#$40			; CHECK X VS 1.0
		BMI	ATAN1			; X<1.0 - USE SERIES DIRECTLY
		LDA	FR0			; X>=1.0 - SAVE SIGN AND TRANSFORM
		AND	#$80
		STA	SGNFLG			; REMEMBER SIGN
		INC	XFMFLG
		LDA	#$7F
		AND	FR0
		STA	FR0			; FORCE PLUS
		LDXY	FP9S,XFORM		; CHANGE ARG TO [X-1]/[X+1]

ATAN1:		LDXY	FPSCR,FSTOR		; ARCTAN[X], -1<X<1 BY SERIES OF APPROXIMATIONS ; X->FSCR
		JSR	FMOVE			; X->FR1
		JSR	FMUL			; X*X->FR0
		BCS	ATNOUT			; 0'FLOW
		LDA	#11
		LDXY	ATCOEF,PLYEVL		; P[X*X]
		BCS	ATNOUT
		LDXY	FPSCR,FLD1R		; X->FR1
		JSR	FMUL			; X*P[X*X]
		BCS	ATNOUT			; 0'FLOW
		LDA	XFMFLG			; WAS ARG XFORM'D
		BEQ	ATAN2			; NO
		LDXY	PIOV4,FLD1R		; YES-ADD ARCTAN[1.0] = PI/4
		JSR	FADD
		LDA	SGNFLG			; GET ORG SIGN
		ORA	FR0
		STA	FR0			; ATAN[-X] = - ATAN[X]
ATAN2:		LDA	DEGFLG			; RADIANS OR DEGREES
		BEQ	ATNOUT			; RAD - FINI
		LDXY	PIOV18,FLD1R		; DEG - DIVIDE BY PI/180
		JSR	FDIV
ATNOUT:		RTS

;---------------------------------------------------------------
; SQR[X] -- Square Root
;---------------------------------------------------------------

SQRERR:		SEC				; SET FAIL
		RTS

SQR:		LDA	#0
		STA	XFMFLG
		LDA	FR0
		BMI	SQRERR
		CMP	#$3F
		BEQ	@FSQR			; X IN RANGE OF APPROX - GO DO
		CLC
		ADC	#1
		STA	XFMFLG			; NOT IN RANGE - TRANSFORM
		STA	FR1			; MANTISSA = 1
		LDA	#1
		STA	FR1+1
		LDX	#FPREC-2
		LDA	#0
@SQR1:		STA	FR1+2,X
		DEX
		BPL	@SQR1
		JSR	FDIV			; X/100**N
@FSQR:						; SQR[X], 0.1<=X<1.0
		LDA	#6
		STA	SQRCNT
		LDXY	FSCR,FST0R		; STASH X IN FSCR
		JSR	FMOVE			; X->FR1
		LDXY	FTWO,FLD0R		; 2.0->FR0
		JSR	FSUB			; 2.0-X
		LDXY	FSCR,FLD1R		; X->FR1
		JSR	FMUL			; X*[2.0-X] :1ST APPROX
@SQRLP:		LDXY	FSCR1,FST0R		; Y->FSCR1
		JSR	FMOVE			; Y->FR1
		LDXY	FSCR,FLD0R
		JSR	FDIV			; X/Y
		LDXY	FSCR1,FLD1R
		JSR	FSUB			; [X/Y]-Y
		LDXY	FHALF,FLD1R
		JSR	FMUL			; 0.5*[[X/Y]-Y]=DELTAY
		LDA	FR0			; DELTA 0.0
		BEQ	@SQRDON
		LDXY	FSCR1,FLD1R
		JSR	FADD			; Y=Y+DELTA Y
		DEC	SQRCNT			; COUNT & LOOP
		BPL	@SQRLP
@SQRDON:	LDXY	FSCR1,FLD0R		; DELTA = 0 - GET Y BACK
;	WAS ARG TRANSFORMED
		LDA	XFMFLG
		BEQ	@SQROUT			; NO FINI
		SEC
		SBC	#$40
		CLC				; YES - TRANSFORM RESULT
		ROR				; DIVIDE EXP BY 2
		CLC
		ADC	#$40
		AND	#$7F
		STA	FR1
		LDA	XFMFLG
		ROR
		LDA	#1			; MANTISSA = 1
		BCC	@SQR2			; WAS EXP ODD OR EVEN
		LDA	#$10			; ODD - MANT = 10
@SQR2:		STA	FR1+1
		LDX	#FPREC-2
		LDA	#0
@SQR3:		STA	FR1+2,X			; CLEAR REST OF MANTISSA
		DEX
		BPL	@SQR3
		JSR	FMUL			; SQR[X] = SQR[X/100**N] * [10**N]

@SQROUT:
		.if	BASIC_REVISION = 10
BASICEXIT:
		.elseif	BASIC_REVISION > 1
		EntryPoint(MAXEND)
BASICEXIT:
		.endif

		RTS

		.if	BASIC_REVISION = 1
FTWO:		.byte	$40,$02,$00,$00,$00,$00    ; 2.0
		.endif

		.if	BASIC_REVISION = 1
		; BF99 - BFEF : Expected (filler) data for checksum
		.byte	$30,$30,$20,$20,$60,$70,$70
		.byte	$00,$00,$00,$00,$30,$30,$3C,$7C,$7C,$7C,$64,$64,$67,$E7,$C0,$C0
		.byte	$80,$80,$E0,$E0,$00,$00,$00,$00,$30,$30,$38,$38,$38,$38,$FC,$EC
		.byte	$EC,$0C,$04,$06,$06,$02,$03,$03,$00,$00,$00,$00,$30,$30,$30,$B0
		.byte	$B0,$D8,$F8,$78,$38,$18,$18,$18,$10,$10,$1C,$1C,$00,$00,$00,$00
		.byte	$30,$30,$30,$30,$30,$30,$30,$30,$20,$E0,$E0,$80,$80,$00,$00,$00
		.endif

		.if	BASIC_REVISION = 1
		EntryPoint(MAXEND)
		; BFF0-BFF8 : Expected (filler) data for checksum
		.byte	$00,$00,$00,$00,$CC,$B8,$A4,$90,$A0
BASICEXIT:	RTS						; BFF9
		.endif

		.if	BASIC_REVISION = 2
		; BFF1-BFF9 : Expected (filler) data for checksum
		.byte	$00,$00,$2C,$37,"inner"			; prehistoric debris!
		.endif

SUFFIX		EQU	$C000-6
		;
		; BFFA/B - BFFC/D - BFFE/F
		; Run address, stack address, final address
		;

		; Fill in any remaining space before suffix
		.res	SUFFIX-*
		EntryPoint(SUFFIX)		; Validate *

		.word	COLDSTART
		.byte	$00			; INSERTED : YES
		.byte	$05			; LOAD DOS : YES
		.word	BASICEXIT

		.end
