;
; Helper functions for Snorky, to adapt into a proper EXE.
; Build this with ca65 / ld65 using the 'make' command.
; The ld65 linker requires snorky.cfg.
;

        .include "snorky.inc"

; Use FR0 for scratch storage

CUBE    = FR0
DDST    = FR0+2
YOFF    = FR0+4
BTMP    = FR0+4
BMASK   = FR0+5

        ; >= 7 bytes are available at $CB
        .ORG ZERO7
        .segment "BSS"
RSV1:   .res 2
RSV2:   .res 2
RSV3:   .res 2
RSV4:   .res 1

        .ORG $0600
        .segment "CODE"

        .proc draw_cube

;
; DrawCube (relocatable)
; Cube is 2 bytes wide, aligned on 4 pixel boundary
;
; X=USR(CUBE, X, Y [,BMASK])
;
DrawCube:
        pla             ; ARGC from USR()
        tax

        ; Get Screen Address

        lda SAVMSC      ; Screen LO
        sta DDST
        lda SAVMSC+1    ; Screen HI
        sta DDST+1

        ; Apply X Position

        pla
        pla             ; Get X
        sec
        sbc #4          ; X is Centered
        lsr             ; Divide by 4
        lsr
        AddToPtr DDST

        ; Apply Y Position

        pla
        sta YOFF+1      ; 0
        pla             ; 0-191
        sec
        sbc #3          ; Y is Centered

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

        ; Get The Address of this USR
        ; Offset to point to our graphics data
        lda #(cube_body-DrawCube)
        AddToPtr CUBE

        ; With 2 arguments draw the cube body

        cpx #2          ; ARGC == 3 : Draw body
        beq _draw_body

        pla             ; ARGC == 3 : Get color BMASK
        pla
        sta BMASK

;
; Draw the top of the cube
;
_draw_top:
        lda #cube_top-cube_body
        AddToPtr CUBE

        ldx #size_top/2
@1:
        CubeLidLineDraw
        lda #2
        AddToPtrCC CUBE
        lda #40
        AddToPtrCC DDST
        dex
        bne @1
        rts

;
; Draw the whole cube body
;
_draw_body:
        ldx #size_body/2
@1:
        BodyLineDraw
        lda #2
        AddToPtr CUBE
        lda #40
        AddToPtrCC DDST
        dex
        bne @1
        rts

cube_body:
        .byte $00,$C0 ; ....3... ;
        .byte $03,$30 ; ...3.3.. ;
        .byte $0C,$0C ; ..3...3. ;
        .byte $30,$03 ; .3.....3 ;
        .byte $0C,$0C ; ..3...3. ;
        .byte $23,$33 ; .2.3.3.3 ;
        .byte $28,$CF ; .22.3.33 ;
        .byte $2A,$3F ; .222.333 ;
        .byte $2A,$BF ; .2222333 ;
        .byte $2A,$3F ; .222.333 ;
        .byte $0A,$BC ; ..22233. ;
        .byte $02,$30 ; ...2.3.. ;
        .byte $00,$80 ; ....2... ;

size_body = *-cube_body

cube_top:
        .byte $00,$00 ; ........ ;
        .byte $00,$C0 ; ....3... ;
        .byte $03,$F0 ; ...333.. ;
        .byte $0F,$FC ; ..33333. ;
        .byte $03,$F0 ; ...333.. ;
        .byte $00,$C0 ; ....3... ;

size_top = *-cube_top

        .endproc
