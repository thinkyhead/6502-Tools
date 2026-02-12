;
; Helper functions for Snorky, to adapt into a proper EXE.
; Build this with ca65 / ld65 using the 'make' command.
; The ld65 linker requires snorky.cfg.
;

        .include "snorky.inc"

        ; >= 7 bytes are available at $CB
        .ORG ZERO7
        .segment "BSS"
PSRC:   .res 2
PDST:   .res 2
PTMP:   .res 1
OLDY:   .res 1

        .ORG $0600
        .segment "CODE"

        .proc snorky_guy

;
; SnorkyGuy (relocatable)
;
; X=USR(SNORKY, SNORKY, dir, frame, spriteAdr, x, y)
;
SnorkyGuy:
        pla             ; ARGC from USR()

        ; SNORKY (Address of this code)

        pla
        sta PSRC+1
        pla
        ; Point to our graphics data
        clc
        adc #(sprites-SnorkyGuy)
        sta PSRC
        bcc _noinc
        inc PSRC+1
_noinc:

        ; DIR 0=LEFT 1=RIGHT

        pla
        pla
        beq _noadd
        lda #(r_sprites-l_sprites)
        AddToPtr PSRC
_noadd:

        ; FRAME 0-2 ... multiply by 13, add to PSRC

        pla
        pla
        sta PTMP        ; x1 (save)
        asl             ; x2
        asl             ; x4
        tax             ; (hold x4)
        adc PTMP        ; x5
        sta PTMP        ; (save x5)
        txa             ; (draw x4)
        asl             ; x8
        adc PTMP        ; x13
        AddToPtrCC PSRC

        ; Get player top address

        pla
        sta PDST+1
        pla             ; Will always be 0 with single-line resolution P/M
        sta PDST        ; PDST = spriteData Low. This being on a page boundary simplifies things

        ; Get X and set position

        pla
        pla
        clc
        adc #48-4
        sta HPOSP0

        ; Erase the old sprite

        lda #0
        ldx #13
        ldy OLDY
_clear:
        sta (PDST),y
        iny
        dex
        bne _clear
_clrdone:

        ; Get Y and final destination P/M address

        pla
        pla
        clc
        adc #32-12
        sta PDST
        sta OLDY

        ; Draw the guy

        ldy #12
_copy:
        lda (PSRC),y
        sta (PDST),y
        dey
        bpl _copy

_exit:  rts

sprites:
l_sprites:
        .byte $1c,$3e,$7f,$d3,$93,$3f,$7f,$7e,$3c,$24,$24,$24,$6c
frame_size = *-l_sprites
        .byte $1e,$3f,$73,$d3,$bf,$7f,$7e,$3c,$24,$24,$24,$24,$6c
        .byte $1e,$3f,$73,$d3,$bf,$7f,$7e,$3c,$24,$24,$6c,$00,$00
r_sprites:
        .byte $38,$7c,$fe,$cb,$c9,$fc,$fe,$7e,$3c,$24,$24,$24,$36
        .byte $78,$fc,$ce,$cb,$fd,$fe,$7e,$3c,$24,$24,$24,$24,$36
        .byte $78,$fc,$ce,$cb,$fd,$fe,$7e,$3c,$24,$24,$36,$00,$00

        .endproc
