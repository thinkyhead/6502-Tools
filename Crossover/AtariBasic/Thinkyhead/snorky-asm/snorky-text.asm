;
; Text drawing function for Snorky
; Use `make` to build with ca65 / ld65.
; The ld65 linker requires snorky.cfg.
;

        .include "snorky.inc"

RELOC = 1

        .ifdef WIDE
;WIDELOOKUP = 1
        .endif

        ; 9 bytes in front of FR0
        .ORG $CB
        .segment "BSS"
BMASK:  .res 1 ; $CB
STRG:   .res 2 ; $CC
DSRC:   .res 2 ; $CE
DDST:   .res 2 ; $D0
YOFF:   .res 2 ; $D2

        ; Use FR0 for scratch storage
FONT:   .res 2 ; $D4

        .ifdef WIDE

        .ifdef WIDEFLAG
FLWIDE: .res 1 ; $D6
        .endif

        .ifdef WIDELOOKUP
        .ifdef RELOC
WLOOK:  .res 2 ; $D7
        .endif
        .else  ; !WIDELOOKUP
CBITS:  .res 2 ; $D7
        .endif ; !WIDELOOKUP

        .endif ; WIDE


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
        cmp #5          ; A 5th arg indicates wide, or not
        bne _noflag
        pla
        pla
        sta FLWIDE
_noflag:
        .endif


        .ifdef WIDELOOKUP
        .ifdef RELOC
        ; WLOOK = widebits
        ;clc
        lda FR0
        adc #widebits-DrawString
        sta WLOOK
        lda FR0+1
        adc #0
        sta WLOOK+1
        .endif
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

        pla
        sta FONT        ; Set to 0 here to save two bytes below
        pla

        .ifdef WIDEFLAG

        sta BMASK ; get pre-made mask (color * $55)

        .else

        ; Get Color and clone low bits 3 times
        ; BMASK = (C | C<<2 | C<<4 | C<<6)

        and #%11
        sta BMASK
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
        ;lda #0
        ;sta FONT
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
; Draw the character, erasing previous contents
;
_draw_char:
        ldx #8
_dcloop:
        lda (DSRC),y    ; A = DSRC[0] - Character glyph bits
        IncWord DSRC    ; DSRC++

        .ifdef WIDE

        .ifdef WIDEFLAG
        ldy FLWIDE
        bne _wide
        sta (DDST),y
        beq _nextline   ; bra _nextline
        .endif

_wide:
        .ifdef WIDELOOKUP

        pha             ; Save char bits

        lsr
        lsr
        lsr
        lsr
        tay             ; Y = A >> 4
        .ifdef RELOC
        lda (WLOOK),y
        .else
        lda widebits,y
        .endif
        and BMASK
        ldy #0
        sta (DDST),y    ; DDST[0] = WLOOK[Y] & BMASK

        pla             ; Get char bits

        and #$0F
        tay             ; Y = A & $0F
        .ifdef RELOC
        lda (WLOOK),y
        .else
        lda widebits,y
        .endif
        and BMASK
        ldy #1
        sta (DDST),y    ; DDST[1] = WLOOK[Y] & BMASK

        .else  ; !WIDELOOKUP

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

        iny
        lda CBITS+1
        and BMASK
        sta (DDST),y    ; DDST[1] |= CBITS[1] & BMASK

        dey
        lda CBITS
        and BMASK
        sta (DDST),y    ; DDST[0] |= CBITS[0] & BMASK

        .endif  ; !WIDELOOKUP

        .else   ; !WIDE

        sta (DDST),y

        .endif  ; !WIDE

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
        IncWord STRG    ; Increment source

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

        IncWord YOFF    ; Increment dest top

        lda YOFF        ; Top of char for next loop
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


        .ifdef WIDELOOKUP
        ; Map 4 pixels to 8 bits
widebits:
        .byte $00,$03,$0c,$0f,$30,$33,$4c,$3f
        .byte $c0,$c3,$cc,$cf,$f0,$f3,$fc,$ff
        .endif

        .endproc ; draw_string
