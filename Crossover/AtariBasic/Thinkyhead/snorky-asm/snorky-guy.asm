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

        ; Sprites Address

        pla
        sta PSRC+1
        pla
        sta PSRC

        ; DIR   0-1=UP   2-3=DOWN
        ;       0/2=LEFT 1/3=RIGHT
        ;

        pla
        pla
        and #1
        beq _noadd
        lda #(frame_size*3)
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

        .endproc

; Sprites stub
frame_size = 13
