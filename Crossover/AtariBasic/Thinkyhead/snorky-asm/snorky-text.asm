;
; Helper functions for Snorky, to adapt into a proper EXE.
; Build this with ca65 / ld65 using the 'make' command.
; The ld65 linker requires snorky.cfg.
;

        .include "snorky.inc"

; Use FR0 for scratch storage

        ; 9 bytes in front of FR0
        .ORG $CB
        .segment "BSS"
STRG:   .res 2
DSRC:   .res 2
DDST:   .res 2
YOFF:   .res 2
FONT:   .res 2
BMASK:  .res 1

        .ORG $0600
        .segment "CODE"

        .proc draw_string

;
; DrawString (relocatable)
; Draw a string using the ROM font
; TODO: Draw a string with narrow color font
;
; X=USR(SAY, X, Y, COLOR, STRING)
;
DrawString:
        pla             ; ARGC from USR()
        ;cmp #3
        ;bne _exit

        ; Get Screen Address

        lda SAVMSC      ; Screen LO
        sta DDST
        lda SAVMSC+1    ; Screen HI
        sta DDST+1

        ; Apply X Position

        pla
        pla             ; Get X
        lsr             ; Divide by 4
        lsr
        AddWord DDST

        ; Apply Y Position

        pla
        sta YOFF+1      ; 0
        pla             ; 0-191

        ; TODO: Sanity check Y for safety here

        asl
        sta YOFF        ; x 2 = 0-382
        rol YOFF+1

        ldy #2
_times4:
        lda YOFF
        asl             ; x 4 ... x 16
        rol YOFF+1
        asl             ; x 8 ... x 32
        rol YOFF+1
        sta YOFF

        ;clc
        adc DDST        ; 8x + 32x = 40x
        sta DDST
        lda YOFF+1
        adc DDST+1
        sta DDST+1

        dey
        bne _times4

        ; Use YOFF to track top scanline
        sta YOFF+1
        lda DDST
        sta YOFF

        ; Get Color and clone low bits 3 times

        pla
        pla
        and #%11
        sta BMASK ; x 1
        ldy #3
_clone2:
        asl
        asl
        ora BMASK
        sta BMASK
        dey
        bne _clone2

        ; Y is now 0, assumed below...

        ; Get String Address
        pla
        sta STRG+1
        pla
        sta STRG

        ; Character Set address
        lda #0
        sta FONT
        lda CHBAS
        sta FONT+1

        ;
        ; Loop through all the chars in the string
        ; Convert 00-31  ... +64
        ;         32-95  ... -32
        ;

        ldy #0
_charloop:
        lda (STRG),y    ; String character
        cmp #'|'
        beq _exit

        ; Convert ATASCII to internal mapping
        ;tax            ; Save for later
        and #$7F        ; 128 unique characters
        cmp #$20
        bcs _no40       ; bgt
        ora #$40        ; 00-1F become 40-5F
        bne _gotc
_no40:  cmp #$60
        bcs _gotc       ; bgt
        sbc #$1F        ; 20-5F become 00-3F
_gotc:
        ldx #0
        stx DSRC+1
        asl
        asl
        rol DSRC+1
        asl             ; 8 bytes per character
        rol DSRC+1

        adc FONT
        sta DSRC
        lda DSRC+1
        adc FONT+1
        sta DSRC+1

        ;txa ; Was it over 128 ?
        ;and #$80

;
; Draw the character
;
_draw_char:
        ldx #8
_dcloop:
        lda (DSRC),y    ; Character glyph bits

        inc DSRC
        bne _dc2
        inc DSRC+1
_dc2:   sta (DDST),y

        lda #40
        AddWord DDST
        dex
        bne _dcloop

;
; Next string character
;
        inc STRG        ; Increment source
        bne _dc3
        inc STRG+1
_dc3:   inc YOFF        ; Increment dest top
        bne _dc4
        inc YOFF+1
_dc4:   lda YOFF        ; Top of char for next loop
        sta DDST
        lda YOFF+1
        sta DDST+1

        bne _charloop

_exit:  rts

        .endproc
