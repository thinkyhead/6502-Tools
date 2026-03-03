;
; Helper functions for Snorky, to adapt into a proper EXE.
; Build this with ca65 / ld65 using the 'make' command.
; The ld65 linker requires snorky.cfg.
;

        .include "snorky.inc"

        ; 9 bytes in front of FR0
        .ORG UNUSED9
        .segment "BSS"
PSIZ:   .res 1
PSRC:   .res 2
PDST:   .res 2
PTOP:   .res 2

        .ORG $0600
        .segment "CODE"

        .proc move_player_y

;
; MovePlayerY (relocatable)
;
; X=USR(MOVE, PBASE, SIZE, X, OLDY, NEWY)
;
MovePlayerY:
        pla             ; argc from USR()
        cmp #5          ; Five 16-bit argument required
        bne _exit

        ; Get PBASE - Sprite's base address
        pla
        ;sta PTOP+1
        sta PSRC+1
        sta PDST+1
        pla             ; Always 0 with single-line resolution P/M
        ;sta PTOP

        ; Get SIZE (0-255)
        pla
        pla
        sta PSIZ
        tay

        ; Get X (0-255)
        pla
        pla
        sec
        sbc #4
        sta HPOSP1

        ; Get OLDY (0-255)
        pla
        pla
        ;clc
        ;adc PTOP
        sta PSRC        ; PSRC = &PTOP[oldY]

        ; Get NEWY (0-255)
        pla
        pla             ; newY
        ;clc
        ;adc PTOP
        sta PDST        ; PDST = &PTOP[newY]

        ldx #0
        cmp PSRC
        beq _exit       ; No movement? Exit!
        bcc _topward    ; if PDST < PSRC (towards the top) copy forward

_loopdown:
        dey
        bmi _exit
        lda (PSRC),y    ; Copy backwards when moving towards the bottom (up in RAM)
        sta (PDST),y
        txa
        sta (PSRC),y
        bcs _loopdown

_topward:
        ldy #0
_loopup:
        lda (PSRC),y    ; Copy forward when moving towards the top (down in RAM)
        sta (PDST),y
        txa
        sta (PSRC),y
        iny
        cpy PSIZ
        bne _loopup

_exit:  rts

        .endproc
