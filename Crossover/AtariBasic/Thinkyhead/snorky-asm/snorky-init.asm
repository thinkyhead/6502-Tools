;
; Helper functions for Snorky, to adapt into a proper EXE.
; Build this with ca65 / ld65 using the 'make' command.
; The ld65 linker requires snorky.cfg.
;

        .include "snorky.inc"

        ; >= 7 bytes are available at $CB
        .ORG ZERO7
        .segment "BSS"
PADR:   .res 2
RES2:   .res 2
RES3:   .res 2
RES4:   .res 1

        .ORG $0600
        .segment "CODE"

;
; SnorkyInit (relocatable)
;
; X=USR(INIT, spriteData)
;
SnorkyInit:
        pla             ; argc from USR()
        ;cmp #1         ; One 16-bit argument required
        ;bne _exit

        pla             ; Sprite address to init
        sta PMBASE
        ;clc
        adc #4          ; First sprite 4 pages above PMBASE (what is the extra 1K for?)
        sta PADR+1
        pla             ; Always 0
        sta PADR

        ;lda #0
        tay
        ldx #4          ; Clear 4 pages
_loop:
        sta (PADR),y    ; Copy backwards when moving towards the bottom (up in RAM)
        iny
        bne _loop
        inc PADR+1
        dex
        bne _loop

        ; 10150 POK.559,58
        ; 10160 POK.53277,N3:POK.623,N1

        lda #$3A        ; Players, no missiles
        sta SDMCTL
        lda #1
        sta GPRIOR
        lda #3
        sta GRACTL

        ; Return a 16-bit unsigned value from USR
        ;SetWordVal FR0,65535

_exit:  rts
