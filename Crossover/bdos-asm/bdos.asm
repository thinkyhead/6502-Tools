;
; Helper functions for BDOS, to adapt into a proper EXE.
; Build this with ca65 / ld65 using the 'make' command. Output is 26 bytes.
; The ld65 linker requires bdos.lnk. For single files dasm might suffice.
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
        cmp #2          ; 2 arguments required
        bne _fail
        pla             ; stringAddr HI
        sta SCR1+1
        pla             ; stringAddr LO
        sta SCR1
        pla             ; length high ignored
        pla             ; only up to 255 length
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
