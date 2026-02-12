;
; Header for Snorky assembly routines
;
SAVMSC          =       $58             ; memory screen pointer
FR0             =       $D4             ; floating-point register 0
SDMCTL          =       $022F           ; shadow DMA control
GPRIOR          =       $026F           ; GPRIOR

GTIA            =       $D000
HPOSP0          =       GTIA+0
HPOSP1          =       GTIA+1
HPOSP2          =       GTIA+2
HPOSP3          =       GTIA+3

GRACTL          =       GTIA+$1D

PMBASE          =       $D407

ZERO7 = $CB

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
; AddToPtr
;
        .macro AddToPtr adr
        clc
        AddToPtrCC adr
        .endmacro
;
; AddToPtrCC
;
        .macro AddToPtrCC adr
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
        lda (CUBE),y    ; Get source bits
        ora (DDST),y    ; OR with screen bits
        sta (DDST),y    ; Write back to screen
        .endmacro

;
; CubeLidLineClear
;
        .macro CubeLidLineClear
        iny
@loop:
        CubeLidColClear
        dey
        beq @loop
        iny
        .endmacro
;
; CubeLidColClear
;
        .macro CubeLidColClear
        lda (CUBE),y    ; Get source bits
        eor #$FF        ; Invert
        and (DDST),y    ; AND with screen bits
        sta (DDST),y
        .endmacro

;
; CubeLidLineDraw
;

        .macro CubeLidLineDraw
        iny
@loop:
        CubeLidColDraw
        dey
        beq @loop
        iny
        .endmacro
;
; CubeLidColDraw
;
        .macro CubeLidColDraw
        lda (CUBE),y    ; Get source bits
        eor #$FF        ; Invert
        and (DDST),y    ; AND with screen bits
        sta BTMP
        lda BMASK       ; Get bit mask $00, $55, $AA, $FF
        and (CUBE),y    ; Apply source bits
        ora BTMP        ; OR with screen bits
        sta (DDST),y    ; Write back to screen
        .endmacro
