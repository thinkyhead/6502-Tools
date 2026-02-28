;
; Sprite data for Snorky
; Build this with 'make'.
;

        .ORG $0600
        .segment "CODE"

        ; Snorky left and right 6 x 13 = 78 bytes
        ; TODO: Make guy facing up-left and down-left, then mirror into a string buffer

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
