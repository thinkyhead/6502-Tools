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
        .byte %00011100
        .byte %00111110  ; $3e
        .byte %01111111  ; $7f
        .byte %11010011  ; $d3
        .byte %10010011  ; $93
        .byte %00111111  ; $3f
        .byte %01111111  ; $7f
        .byte %01111110  ; $7e
        .byte %00111100  ; $3c
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %01101100  ; $6c

frame_size = *-l_sprites ; 13 bytes

        .byte %00011110  ; $1e
        .byte %00111111  ; $3f
        .byte %01110011  ; $73
        .byte %11010011  ; $d3
        .byte %10111111  ; $bf
        .byte %01111111  ; $7f
        .byte %01111110  ; $7e
        .byte %00111100  ; $3c
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %01101100  ; $6c

        .byte %00011110  ; $1e
        .byte %00111111  ; $3f
        .byte %01110011  ; $73
        .byte %11010011  ; $d3
        .byte %10111111  ; $bf
        .byte %01111111  ; $7f
        .byte %01111110  ; $7e
        .byte %00111100  ; $3c
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %01101100  ; $6c
        .byte %00000000  ; $00
        .byte %00000000  ; $00
r_sprites:
        .byte %00111000  ; $38
        .byte %01111100  ; $7c
        .byte %11111110  ; $fe
        .byte %11001011  ; $cb
        .byte %11001001  ; $c9
        .byte %11111100  ; $fc
        .byte %11111110  ; $fe
        .byte %01111110  ; $7e
        .byte %00111100  ; $3c
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %00110110  ; $36

        .byte %01111000  ; $78
        .byte %11111100  ; $fc
        .byte %11001110  ; $ce
        .byte %11001011  ; $cb
        .byte %11111101  ; $fd
        .byte %11111110  ; $fe
        .byte %01111110  ; $7e
        .byte %00111100  ; $3c
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %00110110  ; $36

        .byte %01111000  ; $78
        .byte %11111100  ; $fc
        .byte %11001110  ; $ce
        .byte %11001011  ; $cb
        .byte %11111101  ; $fd
        .byte %11111110  ; $fe
        .byte %01111110  ; $7e
        .byte %00111100  ; $3c
        .byte %00100100  ; $24
        .byte %00100100  ; $24
        .byte %00110110  ; $36
        .byte %00000000  ; $00
        .byte %00000000  ; $00
