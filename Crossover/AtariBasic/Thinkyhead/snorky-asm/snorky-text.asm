;
; Text drawing function for Snorky
; Use `make` to build with ca65 / ld65.
; The ld65 linker requires snorky.cfg.
;

        .include "snorky.inc"

; Use FR0 for scratch storage

        ; 9 bytes in front of FR0
        .ORG $CB
        .segment "BSS"
STRG:   .res 2 ; $CB
DSRC:   .res 2 ; $CD
DDST:   .res 2 ; $CF
YOFF:   .res 2 ; $D1
FONT:   .res 2 ; $D3
BMASK:  .res 1 ; $D5

        .ifdef WIDE
        .ifdef WIDEFLAG
FLWIDE: .res 1 ; $D6
        .endif
CBITS:  .res 2 ; $D7
        .endif

        .ORG $0600
        .segment "CODE"

        .proc draw_string

;
; DrawString (relocatable)
; Draw a string using the ROM font
; TODO: Draw a string with narrow color font
;
; X=USR(SAY, [wide], X, Y, COLOR, STRING)
;
DrawString:
        pla             ; ARGC from USR()

        .ifdef WIDEFLAG
        cmp #5          ; A 6th arg indicates wide
        bne _noflag
        pla
        pla
        sta FLWIDE
_noflag:
        .endif

        ; DDST = SAVMSC - Get Screen Address

        lda SAVMSC      ; Screen LO
        sta DDST
        lda SAVMSC+1    ; Screen HI
        sta DDST+1

        ; DDST += X / 4 - Apply X Position

        pla
        pla             ; Get X
        lsr             ; Divide by 4
        lsr
        AddWord DDST

        ; DDST = YOFF * 40 - Apply Y Position

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

        ; YOFF = DDST - Use YOFF to track top scanline
        sta YOFF+1
        lda DDST
        sta YOFF

        ; BMASK = (C | C<<2 | C<<4 | C<<6) - Get Color and clone low bits 3 times

        pla
        pla

        .ifdef WIDEFLAG

        sta BMASK ; provide mask (color * $55)

        .else

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

        .endif

        ; Y is now 0, assumed below...

        ; STRG - Get String Address
        pla
        sta STRG+1
        pla
        sta STRG

        ; FONT = CHBAS * 256 - Character Set address
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
        lda (STRG),y    ; A = *STRG - String character
        cmp #'|'
        beq _exit

        ; Convert ATASCII to internal mapping
        ; DSRC = FONT + 8 * internalize(A)

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
        lda (DSRC),y    ; A = DSRC[0] - Character glyph bits

        inc DSRC        ; DSRC++
        bne _dc2
        inc DSRC+1
_dc2:

        .ifdef WIDE

        .ifdef WIDEFLAG
        ldy FLWIDE
        bne _wide
        sta (DDST),y
        beq _nextline
        .endif

_wide:
        ldy #8          ; 8 bits wide
_loop8:
        asl             ; Shift out HI
        php             ; Save Carry
        rol CBITS+1     ; Shift into CBITS LO
        rol CBITS       ; and HI
        plp             ; Restore Carry
        rol CBITS+1     ; Shift into CBITS LO
        rol CBITS       ; and HI
        dey
        bne _loop8

        lda CBITS
        and BMASK
        sta (DDST),y    ; DDST = CBITS[0] & BMASK
        lda CBITS+1
        and BMASK
        iny
        sta (DDST),y    ; DDST + 1 = CBITS[1] & BMASK
        dey

        .else   ; !WIDE

        sta (DDST),y

        .endif

_nextline:
        lda #40
        AddWord DDST
        dex
        bne _dcloop

        .ifdef WIDEFLAG
        beq _nextchar   ; bra _nextchar
_charleap:
        bne _charloop   ; bra _charloop
_exit:  rts             ; closer _exit
        .endif

;
; Next string character
;
_nextchar:
        inc STRG        ; Increment source
        bne _dc3
        inc STRG+1
_dc3:
        .ifdef WIDE

        .ifdef WIDEFLAG

        ;clc
        lda FLWIDE
        adc #1          ; dest += 1 or 2

        .else

        lda #2          ; dest += 2

        .endif

        adc YOFF
        sta YOFF
        sta DDST
        lda YOFF+1
        adc #0
        sta YOFF+1

        .else

        inc YOFF        ; Increment dest top
        bne _dc4
        inc YOFF+1
_dc4:   lda YOFF        ; Top of char for next loop
        sta DDST
        lda YOFF+1

        .endif

        sta DDST+1

        .ifdef WIDEFLAG
        bne _charleap
        .else
        bne _charloop
_exit:  rts
        .endif

        .endproc
