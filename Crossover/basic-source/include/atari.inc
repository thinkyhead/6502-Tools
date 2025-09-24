
		.macro EntryPoint value
		.if value <> *
		.error "Entrypoint not OK"
		.endif
		.endmacro

LNFLG		=	$00		; used on OS startup
NGFLAG		=	$01		; 'no good' flag for memory!
CASINI		=	$02		; cassette initialisation vector
RAMLO		=	$04		; RAM lo
TRAMSZ		=	$06		; temporary RAM size
CMCMD		=	$07		; command flag for modems
WARMST		=	$08		; warmstart flag
BOOTQ		=	$09		; boot flag success
DOSVEC		=	$0A		; DOS vector
DOSINI		=	$0C		; DOS initialisation vector
APPMHI		=	$0E		; application memory high limit
POKMSK		=	$10		; POKEY interrupt mask
BRKKEY		=	$11		; break key flag
RTCLOK		=	$12		; realtime clock

BUFADR		=	$15		; SIO BUF addr

ICCOMT		=	$17		; Command for CIO vector

ZIOCB		=	$20

ICHIDZ		=	$20		; handler index number
ICDNOZ		=	$21
ICCOMZ		=	$22
ICSTAZ		=	$23
ICBALZ		=	$24
ICBAHZ		=	$25
ICPTLZ		=	$26
ICPTHZ		=	$27
ICBLLZ		=	$28
ICBLHZ		=	$29
ICAX1Z		=	$2A
ICAX2Z		=	$2B
ICAX3Z		=	$2C
ICAX4Z		=	$2D
ICAX5Z		=	$2E
ICAX6Z		=	$2F

STATUS		=	$30		; SIO status
CHKSUM		=	$31		; SIO checksum
BUFRLO		=	$32		; SIO buffer lo
BUFRHI		=	$33		; SIO buffer hi
BFENLO		=	$34		; next byte past end of SIO/DCB buffer
BFENHI		=	$35		; next byte past end of SIO/DCB buffer

LTEMP		=	$36		; temp buffer for peripheral handler

BUFRFL		=	$38		; SIO buffer full flag
RECVDN		=	$39		; SIO
XMTDON		=	$3A		; SIO transmission done handler
CHKSNT		=	$3B		; SIO checksum sent flag
NOCKSM		=	$3C		; 'no checksum follows data' flag
BPTR		=	$3D		; cassette buffer pointer
FTYPE		=	$3E		; inter-record gap type
FEOF		=	$3F		; cassette end-of-file flag
FREQ		=	$40		; beep count retain register
SOUNDR		=	$41		; sound during IO
CRITIC		=	$42		; critical I/O region
ZCHAIN		=	$4A		;

DSTAT		=	$4C		; display status
ATRACT		=	$4D		; atract mode timer and flag
DRKMSK		=	$4E		; dark attract mask
COLRSH		=	$4F		; color shift mask

TEMPOR		=	$50		; temporary
HOLD1		=	$51		; temporary
LMARGN		=	$52		; left margin
RMARGN		=	$53		; right margin
ROWCRS		=	$54		; current screen cursor row
COLCRS		=	$55		; current screen cursor column
DINDEX		=	$57		; current screen mode
SAVMSC		=	$58		; memory screen pointer
OLDROW		=	$5A		; previous cursor row
OLDCOL		=	$5B		; previous cursor column
OLDCHR		=	$5D		; retains char under cursor
OLDADR		=	$5E

FKDEF		=	$60		; function key definition
PALNTS		=	$62		; PAL/NTSC flag
LOGCOL		=	$63
ADRESS		=	$64		; multi-purpose address
MLTTMP		=	$66
SAVADR		=	$68
RAMTOP		=	$6A		; RAM top
BUFCNT		=	$6B		; buffer count
BUFSTR		=	$6C		; editor low byte
BITMSK		=	$6E
SHFAMT		=	$6F
ROWAC		=	$70
COLAC		=	$72
ENDPT		=	$74
DELTAR		=	$76
DELTAC		=	$77
SWPFLG		=	$7B
HOLDCH		=	$7C
INSDAT		=	$7D
COUNTR		=	$7E

KEYDEF		=	$79		; pointer to keyboard definition table

FR0		=	$D4		; floating-point register 0
FRE		=	$DA		; floating-point extra register
FR1		=	$E0		; floating-point register 1
FR2		=	$E6		; floating-point register 2

FRX		=	$EC		; FP extra register
EEXP		=	$ED		; exponent value
NSIGN		=	$EE		; sign of fp number
ESIGN		=	$EF		; exponent sign
FCHRFLG		=	$F0		; first character flag
DIGRT		=	$F1		; digits to right of decimal point

CIX		=	$F2		; character index
INBUFF		=	$F3		; input buffer for FP

VDSLST		=	$200		; NMI / Display List
VPRCED		=	$202		; IRQ / POKEY Serial Proceed Line
VINTER		=	$204		; IRQ / POKEY Serial
VBREAK		=	$206		; IRQ / software BRK
VKEYBD		=	$208		; IRQ / keyboard
VSERIN		=	$20A		; IRQ / POKEY serial input ready
VSEROR		=	$20C		; IRQ / POKEY serial output ready
VSEROC		=	$20E		; IRQ / POKEY serial transmit complete
VTIMR1		=	$210		; IRQ / POKEY timer 1
VTIMR2		=	$212		; IRQ / POKEY timer 2
VTIMR4		=	$214		; IRQ / POKEY timer 4
VIMIRQ		=	$216		; IRQ / Immediate

CDTMV1		=	$218		; countdown timer 1 value
CDTMV2		=	$21A		; countdown timer 2 value
CDTMV3		=	$21C		; countdown timer 3 value
CDTMV4		=	$21E		; countdown timer 4 value
CDTMV5		=	$220		; countdown timer 5 value

VVBLKI		=	$222		; NMI / VBLANK Immediate
VVBLKD		=	$224		; NMI / VBLANK Deferred

CDTMA1		=	$226		; countdown timer 1 address
CDTMA2		=	$228		; countdown timer 2 address

CDTMF3		=	$22A		; countdown timer 3 flag
SRTIMR		=	$22B		; software repeat timer (keyboard)
CDTMF4		=	$22C		; countdown timer 4 flag
INTEMP		=	$22D		;
CDTM5		=	$22E		; UNUSED ???
SDMCTL		=	$22F		; shadow DMA control
SDLSTL		=	$230		; shadow display-list lo
SDLSTH		=	$231		; shadow display-list hi
SSKCTL		=	$232		; shadow serial control
LCOUNT		=	$233		; temp counter for periph hand. loader
LPENH		=	$234		; light pen horizontal value
LPENV		=	$235		; light pen vertical value

VBRKKY		=	$236		; IRQ / break key
VPIRQ		=	$238		; Parallel IRQ

CDEVIC		=	$23A		; SIO device code
CCOMND		=	$23B		; SIO command code
CAUX1		=	$23C		; SIO auxiliary byte 1
CAUX2		=	$23D		; SIO auxiliary byte 2
TEMP		=	$23E		; SIO temp register
ERRFLG		=	$23F		; SIO error flag

DFLAGS		=	$240		; -
DBSECT		=	$241		; number of boot sectors
BOOTAD		=	$242		; boot address

COLDST		=	$244		; coldstart flag

RECLEN		=	$245		; relocatable loader routine

DSKTIM		=	$246		; disk time-out
PDVMSK		=	$247		; parallel device mask
SHPDVS		=	$248		; shadow parallel device select
PDMSK		=	$249		; parallel bus interrupt mask

		.ifdef	OS_REVISION
		.if	(OS_REVISION=0)
RELADR          =	$238
		.else
RELADR		=	$24A		; reloc. loader relative address
		.endif
		.endif

PPTMPA		=	$24C		; reloc. loader one-byte temp storage
PPTMPX		=	$24D

CHSALT		=	$26B		; alternate character set
VSFLAG		=	$26C		; fine-scroll temporary register
KEYDIS		=	$26D		; keyboard disable

FINE		=	$26E		; fine-scroll enable
GPRIOR		=	$26F		; GPRIOR

PADDL0		=	$270
PADDL1		=	$271
PADDL2		=	$272
PADDL3		=	$273
PADDL4		=	$274
PADDL5		=	$275
PADDL6		=	$276
PADDL7		=	$277

STICK0		=	$278
STICK1		=	$279
STICK2		=	$27A
STICK3		=	$27B

PTRIG0		=	$27C
PTRIG1		=	$27D
PTRIG2		=	$27E
PTRIG3		=	$27F
PTRIG4		=	$280
PTRIG5		=	$281
PTRIG6		=	$282
PTRIG7		=	$283

STRIG0		=	$284
STRIG1		=	$285
STRIG2		=	$286
STRIG3		=	$287

HIBYTE		=	$288		; relocatable loader stuff
WMODE		=	$289		; --- read/write cassette mode
BLIM		=	$28A		; cassette data record buffer size
JVECK		=	$28C		; jump vector
NEWADR		=	$28E

TXTROW		=	$290		; ---
TXTCOL		=	$291		; ---
TINDEX		=	$293		; ---
TXTMSC		=	$294		; ---
TXTOLD		=	$296		; ---

CRETRY		=	$29C

HOLD3		=	$29D		; ---
SUBTMP		=	$29E		; ---
HOLD2		=	$29F		; ---
DMASK		=	$2A0		; ---
TMPLBT		=	$2A1		; ---

ESCFLG		=	$2A2
TABMAP		=	$2A3
LOGMAP		=	$2B2		; ---
INVFLG		=	$2B6		; ---
FILFLG		=	$2B7		; ---
TMPROW		=	$2B8		; ---
TMPCOL		=	$2B9		; ---
SCRFLG		=	$2BB		; ---
HOLD4		=	$2BC		; ---
DRETRY		=	$2BD
SHFLOK		=	$2BE		; ---
BOTSCR		=	$2BF		; ---

PCOLR0		=	$2C0
PCOLR1		=	$2C1
PCOLR2		=	$2C2
PCOLR3		=	$2C3
COLOR0		=	$2C4
COLOR1		=	$2C5
COLOR2		=	$2C6
COLOR3		=	$2C7
COLOR4		=	$2C8

RUNADR		=	$2C9		; relocatable loader run address
GBYTEA		=	$2CF		; relocatable loader stuff
LOADAD		=	$2D1		; relocatable loader stuff
ZLOADA		=	$2D3		; relocatable loader stuff
DSCTLN		=	$2D5		; disk sector length

KRPDEL		=	$2D9		; keyboard repeat delay
KEYREP		=	$2DA		; rate of keyboard repeat
NOCLIK		=	$2DB		; keyboard click disable
HELPFLG		=	$2DC		; HELP-key status
DMASAV		=	$2DD		; saves SDMCTL

PBPNT		=	$2DE		; printer buffer pointer
PBUFSZ		=	$2DF		; printer buffer size

GLBABS		=	$2E0
RUNAD		=	$2E0
INITAD		=	$2E2

RAMSIZ		=	$2E4		; ram size
HIMEM		=	$2E5		; memtop ---?
MEMLO		=	$2E7		; memlo

HNDLOD		=	$2E9		; reloc. loader routine handler flag

DVSTAT		=	$2EA		; peripheral status

CBAUDL		=	$2EE		; cassette baud rate
CBAUDH		=	$2EF		; cassette baud rate

CRSINH		=	$2F0		; cursor inhibit flag
KEYDEL		=	$2F1		; key delay flag
CH1		=	$2F2		; prior keyboard character code
CHACT		=	$2F3		; character thingy
CHBAS		=	$2F4		; character def ptr
NEWROW		=	$2F5		; ---
NEWCOL		=	$2F6		; ---
ROWINC		=	$2F8		; ---
COLINC		=	$2F9		; ---
CHAR		=	$2FA		;
ATACHR		=	$2FB		;
CH		=	$2FC		; internal hardware value for last key
FILDAT		=	$2FD		; color data for XIO FILL region
DSPFLG		=	$2FE		; display flag
SSFLAG		=	$2FF		; start/stop display screen flag

DDEVIC		=	$300		; device serial bus id
DUNIT		=	$301		; device unit number
DCOMND		=	$302		; device command
DSTATS		=	$303		; device status code
DBUFLO		=	$304		; data buffer lo
DBUFHI		=	$305		; data buffer hi
DTIMLO		=	$306		; device time-out (64 sec/60 units)
DUNUSE		=	$307		; unused
DBYTLO		=	$308		; byte count
DBYTHI		=	$309		; byte count
DAUX1		=	$30A		; aux (eg sector #)
DAUX2		=	$30B		; aux (eg sector #)


TIMER1		=	$30C
CASFLG		=	$30F
TIMER2		=	$310
TEMP1		=	$312		; temp storage
TEMP2		=	$313		; temp storage
PTIMOT		=	$314		; printer time-out value
TEMP3		=	$315		; temporary storage register
SAVIO		=	$316
TIMFLG		=	$317
STACKP		=	$318
TSTAT		=	$319

HATABS		=	$31A		; Handler Address Table

PUPBT1		=	$33D		; power-up bytes
PUPBT2		=	$33E
PUPBT3		=	$33F

IOCB		=	$340		; eight I/O control blocks

ICHID		=	$340
ICDNO		=	$341
ICCOM		=	$342
ICSTA		=	$343
ICBAL		=	$344
ICBAH		=	$345
ICPTL		=	$346
ICPTH		=	$347
ICBLL		=	$348
ICBLH		=	$349
ICAX1		=	$34A
ICAX2		=	$34B
ICAX3		=	$34C
ICAX4		=	$34D
ICAX5		=	$34E
ICAX6		=	$34F

PRNBUF		=	$3C0

SUPERF		=	$3E8		;
CKEY		=	$3E9		; cassette boot key flag
CASSBT		=	$3EA		; cassette boot flag
CARTCK		=	$3EB		; Cartridge checksum
DERRF		=	$3EC		; Display error flag

BASICF		=	$3F8		; BASIC flag
GINTLK		=	$3FA		; Cartridge interlock register
CHLINK		=	$3FB

CASBUF		=	$3FD		; cassette/disk record buffer

LBUFF		=	$580

PDVS		=	$D1FF		; parallel device select

		; cartridge entry points

BFF0		=	$BFF0
BFFA		=	$BFFA
BFFC		=	$BFFC
BFFE		=	$BFFE

		; floating point entry point

__DA48		=	$DA48
__DA51		=	$DA51
SKIP_SPACES	=	$DBA1
__DBAF		=	$DBAF
__DC00		=	$DC00
__DE95		=	$DE95
__DF6C		=	$DF6C
__DFAE		=	$DFAE
__DFEA		=	$DFEA

AFP		=	$D800
FASC		=	$D8E6
IFP		=	$D9AA
FPI		=	$D9D2
ZFR0		=	$DA44
ZF1		=	$DA46
FSUB		=	$DA60
FADD		=	$DA66
FMUL		=	$DADB
FDIV		=	$DB28
PLYEVL		=	$DD40
FLD0R		=	$DD89
FLD1R		=	$DD98
FSTOR		=	$DDA7
FMOVE		=	$DDB6
EXP		=	$DDC0
EXP10		=	$DDCC
LOG		=	$DECD
LOG10		=	$DED1

CIOV		=	$E456
SELFSV		=	$E471
