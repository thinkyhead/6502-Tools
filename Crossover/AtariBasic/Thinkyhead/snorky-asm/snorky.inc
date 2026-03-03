;
; Header for Snorky assembly routines
;
SAVMSC          =       $58             ; memory screen pointer
UNUSED9         =       $CB             ; unused 7 bytes area
FR0             =       $D4             ; floating-point register 0
SDMCTL          =       $022F           ; shadow DMA control
GPRIOR          =       $026F           ; GPRIOR
CHBAS           =       $02F4           ; character def ptr

GTIA            =       $D000
HPOSP0          =       GTIA+0
HPOSP1          =       GTIA+1
HPOSP2          =       GTIA+2
HPOSP3          =       GTIA+3

GRACTL          =       GTIA+$1D

PMBASE          =       $D407

        .define bgt bcs
        .define ble bcc

;
; SetWordVal dest,word
;
        .macro SetWordVal adr,word
        lda #<word
        sta adr
        lda #>word
        sta adr+1
        .endmacro

;
; Add16
;
        .macro Add16 adr
        clc
        Add16CC adr
        .endmacro
;
; Add16CC
;
        .macro Add16CC adr
        adc adr
        sta adr
        lda #0
        adc adr+1
        sta adr+1
        .endmacro

;
; BodyLineDraw
;
        .macro BodyLineDraw
        iny
@loop:
        BodyColDraw
        dey
        beq @loop
        iny
        .endmacro
;
; BodyColDraw
;
        .macro BodyColDraw
        lda (DSRC),y    ; Get source bits
        ora (DDST),y    ; OR with screen bits
        sta (DDST),y    ; Write back to screen
        .endmacro

;
; LidLineClear
;
        .macro LidLineClear
        iny
@loop:
        LidColClear
        dey
        beq @loop
        iny
        .endmacro
;
; LidColClear
;
        .macro LidColClear
        lda (DSRC),y    ; Get source bits
        eor #$FF        ; Invert
        and (DDST),y    ; AND with screen bits
        sta (DDST),y
        .endmacro

;
; LidLineDraw
;

        .macro LidLineDraw
        iny
@loop:
        LidColDraw
        dey
        beq @loop
        iny
        .endmacro
;
; LidColDraw
;
        .macro LidColDraw
        lda (DSRC),y    ; Get source bits
        eor #$FF        ; Invert
        and (DDST),y    ; AND with screen bits
        sta BTMP
        lda BMASK       ; Get bit mask $00, $55, $AA, $FF
        and (DSRC),y    ; Apply source bits
        ora BTMP        ; OR with screen bits
        sta (DDST),y    ; Write back to screen
        .endmacro
