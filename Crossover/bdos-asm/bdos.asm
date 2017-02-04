;
; Helper functions for BDOS, to adapt into a proper EXE
;

; 7 bytes at CB are available
        .ORG $CB
        .segment "BSS"
SCR0:   .res 1
SCR1:   .res 2
SCR2:   .res 2
SCR3:   .res 2

;
; InvertString (relocatable)
;
; X=USR(InvertString, stringAddr, length)
;
        .ORG $068A      ; 1674
        .segment "CODE"

        .proc inv_string

InvertString:
        pla
        cmp #2
        bne _fail
        pla
        sta SCR1+1
        pla
        sta SCR1
        pla
        pla             ; only 255 length
        beq _fail
        tay

_invloop:
        lda (SCR1),y
        eor #$80
        sta (SCR1),y
        dey
        bpl _invloop

_fail:
        rts

        .endproc
