;
; Helper functions for Snorky, to adapt into a proper EXE.
; Build this with ca65 / ld65 using the 'make' command.
; The ld65 linker requires snorky.cfg.
;

        .include "snorky.inc"

        ; >= 7 bytes are available at $CB
        .ORG ZERO7
        .segment "BSS"
PSIZ:   .res 1
PTOP:   .res 2
PSRC:   .res 2
PDST:   .res 2

        .ORG $0600
        .segment "CODE"

        .proc move_player_y

;
; MovePlayerY (relocatable)
;
; X=USR(MOVE, spriteData, size, newX, oldY, newY)
;
MovePlayerY:
        pla             ; argc from USR()
        cmp #5          ; Four 16-bit arguments required
        bne _exit

        pla
        sta PTOP+1
        sta PSRC+1
        sta PDST+1
        pla             ; Will always be 0 with single-line resolution P/M
        sta PTOP        ; PTOP = spriteData Low. This being on a page boundary simplifies things

        pla
        pla             ; size
        sta PSIZ
        tay

        pla
        pla             ; newX
        sec
        sbc #4
        sta HPOSP1

        pla
        pla             ; oldY
        ;clc
        ;adc PTOP
        sta PSRC        ; PSRC = &PTOP[oldY]

        pla
        pla             ; newY
        ;clc
        ;adc PTOP
        sta PDST        ; PDST = &PTOP[newY]

        ldx #0
        cmp PSRC
        beq _exit       ; No movement? Exit!
        bcc _topward    ; if PDST < PSRC (towards the top) copy forward

        dey
_loopdown:
        lda (PSRC),y    ; Copy backwards when moving towards the bottom (up in RAM)
        sta (PDST),y
        txa
        sta (PSRC),y
        dey
        bpl _loopdown
        bmi _exit

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

;sprite: .byte 16,56,124,56,16
;sSprite = *-sprite

        .endproc
