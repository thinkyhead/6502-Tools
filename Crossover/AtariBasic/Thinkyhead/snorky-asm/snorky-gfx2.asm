;
; Sprite data for Snorky
; Build this with 'make'.
;

        .ORG $0600
        .segment "CODE"

        ; Little dude frames 10 x 16 = 160 bytes

Frame0:
        .byte %01010100;$DC
        .byte %00111000;$3E
        .byte %01111100
        .byte %01010100
        .byte %01010100
        .byte %01111100
        .byte %01101100
        .byte %00111000
        .byte %00010000;$2A
        .byte %01111100;$4A
        .byte %10111010
        .byte %10111010
        .byte %00111000
        .byte %00101000
        .byte %00101000
        .byte %01101100
Frame1:
        .byte %01010100
        .byte %00111000
        .byte %01111100
        .byte %01010100
        .byte %01111100
        .byte %01000100
        .byte %01000100
        .byte %00111000
        .byte %10010010
        .byte %01111100
        .byte %00111000
        .byte %00111000
        .byte %00111000
        .byte %00101000
        .byte %00101000
        .byte %01101100
Frame2:
        .byte %01010100
        .byte %00111000
        .byte %01111100
        .byte %01111100
        .byte %01111100
        .byte %01010100
        .byte %01111100
        .byte %00111000
        .byte %00010000
        .byte %01111100
        .byte %10111010
        .byte %10111010
        .byte %00111000
        .byte %00101000
        .byte %00101000
        .byte %01101100
Frame3:
        .byte %00001010
        .byte %00011100
        .byte %00111100
        .byte %00111010
        .byte %00111010
        .byte %00111110
        .byte %00111110
        .byte %00011100
        .byte %00001000
        .byte %00011100
        .byte %00111110
        .byte %01011101
        .byte %00011100
        .byte %00010100
        .byte %00010100
        .byte %00011110
Frame4:
        .byte %00001010
        .byte %00011100
        .byte %00111100
        .byte %00111010
        .byte %00111010
        .byte %00111110
        .byte %00111110
        .byte %00011100
        .byte %00001000
        .byte %00011100
        .byte %00111110
        .byte %01011101
        .byte %00011100
        .byte %00011000
        .byte %00001000
        .byte %00001100
Frame5:
        .byte %00001010
        .byte %00011100
        .byte %00111100
        .byte %00111010
        .byte %00111010
        .byte %00111110
        .byte %00111110
        .byte %00011100
        .byte %00001000
        .byte %00011100
        .byte %00111110
        .byte %01011101
        .byte %00011100
        .byte %00010110
        .byte %00010000
        .byte %00011000
Frame6:
        .byte %00001010
        .byte %00011100
        .byte %00111100
        .byte %00111010
        .byte %00111010
        .byte %00111110
        .byte %00111110
        .byte %00011100
        .byte %00001000
        .byte %00011100
        .byte %00111110
        .byte %01011101
        .byte %00011100
        .byte %00110100
        .byte %00100100
        .byte %00110110
Frame7:
        .byte %00001010
        .byte %00011100
        .byte %00111100
        .byte %00111110
        .byte %00110100
        .byte %00110100
        .byte %00111110
        .byte %00011100
        .byte %00001000
        .byte %00011111
        .byte %00011100
        .byte %00011111
        .byte %00011100
        .byte %00011100
        .byte %00011100
        .byte %00011110
Frame8:
        .byte %00001010
        .byte %00011100
        .byte %00111100
        .byte %00111110
        .byte %00111110
        .byte %00110100
        .byte %00110110
        .byte %00011100
        .byte %00001000
        .byte %00011100
        .byte %00011101
        .byte %00011110
        .byte %00011100
        .byte %00011100
        .byte %00011100
        .byte %00011110
Frame9:
        .byte %01011010
        .byte %00111100
        .byte %01111110
        .byte %01011010
        .byte %01011010
        .byte %01111110
        .byte %01100110
        .byte %00111100
        .byte %00011000
        .byte %00111100
        .byte %01111110
        .byte %10111101
        .byte %00111100
        .byte %00100100
        .byte %00100100
        .byte %01100110
