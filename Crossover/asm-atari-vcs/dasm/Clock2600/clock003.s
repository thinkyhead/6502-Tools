;---------------------------------------------------------------------------

;   * Subject: [stella] 2600 Digital Clock (source code)
;   * From: crackers@hwcn.org
;   * Date: Sun, 5 Oct 1997 14:31:50 -0400 (EDT)

;---------------------------------------------------------------------------

; Here's the source code for that final digital clock programme.
; Feel free to employ and distribute this code however you may wish.
; Both the source code and the binary are public domain.

; -----------------------------------------------------------------------------
        processor 6502

VSYNC   =       $00
VBLANK  =       $01
WSYNC   =       $02
NUSIZ0  =       $04
NUSIZ1  =       $05
COLUPF  =       $08
COLUBK  =       $09
PF0     =       $0D
PF1     =       $0E
PF2     =       $0F
SWCHA   =       $280
INTIM   =       $284
TIM64T  =       $296
CTRLPF  =       $0A
COLUP0  =       $06
COLUP1  =       $07
GP0     =       $1B
GP1     =       $1C
HMOVE   =       $2a
RESP0   =       $10
RESP1   =       $11

;RAM

TEMP    =       $80     ;2 bytes for temporary data
SECS    =       $82     ;seconds counter
MINS    =       $83     ;minutes counter
HOURS   =       $84     ;hours counter
JOYDEL  =       $85     ;joystick delay variable
JOY1ST  =       $86     ;joystick first move variable
SPRITEA =       $87     ;8 bytes for the first sprite
SPRITEB =       $8F     ;8 bytes for the second sprite
RMINS   =       $97     ;real minutes
RHOURS  =       $98     ;real hours
FRAMES  =       $99     ;frames counter

        org  $F000

start   SEI
        CLD
        LDX  #$FF
        TXS
        LDA  #$00

zero    STA  $00,X      ;looks familiar, right?
        DEX             ;typical zeroing routine
        BNE  zero

        LDA  #$01       ;now we set up all our variables
        STA  CTRLPF
        LDA  #$0C       ;set our starting time at 12:00
        STA  HOURS      ;just like a VCR, eh? Except it doesn't blink
        LDA  #$3C       ;00 minutes
        STA  MINS
        LDA  #$ca       ;nice pretty green for our sprites
        STA  COLUP0
        STA  COLUP1
        LDA  #$07       ;make them good and fat
        STA  NUSIZ0
        STA  NUSIZ1
        LDA  #$3C       ;initialize the frame and seconds counters
        STA  FRAMES
        STA  SECS

main    JSR  vertb      ;main loop
        JSR  time
        JSR  draw
        JSR  clear
        JMP  main

vertb   LDX  #$00       ;vertical blank, We all know what this is about
        LDA  #$02
        STA  WSYNC
        STA  WSYNC
        STA  WSYNC
        STA  VSYNC
        STA  WSYNC
        STA  WSYNC
        LDA  #$2C
        STA  TIM64T
        LDA  #$00
        STA  WSYNC
        STA  VSYNC
        RTS

time    LDY  #06        ;just load Y ahead of time for #of sprite lines
        LDA  #$3C       ;60
        SEC
        SBC  MINS       ;subtract the clock minutes from 60 to get the
        STA  RMINS      ;real minutes since clock counts down
        CMP  #$00       ;see if it's 00 minutes
        BEQ  min0
        CMP  #$32       ;see if it's more than 50 minutes
        BPL  min5
        CMP  #$28       ;see if it's more than 40 minutes
        BPL  min4
        CMP  #$1E       ;see if it's more than 30 minutes
        BPL  min3
        CMP  #$14       ;see if it's more than 20 minutes
        BPL  min2
        CMP  #$0A       ;see if it's more than 10 minutes
        BPL  min1

min0    LDA  zeros,y    ;minutes must be less than 10 so load 00 sprite
        AND  #$F0       ;strip the first 4 bits
        STA  SPRITEA,y  ;store it to sprite A memory
        DEY
        BPL  min0       ;get next sprite line
        LDA  #$00       ;less than 10 minutes
        JMP  minload    ;go to where we load the first 4 bits of sprite

min5    LDA  fives,y    ;minutes must be 50+ so load 55 sprite
        AND  #$F0       ;strip 1st four bits
        STA  SPRITEA,y  ;store it to sprite A memory
        DEY
        BPL  min5       ;get next sprite line
        LDA  #$32       ;50+ minutes - you'll need this number later to
        JMP  minload    ;load the second half the sprite data

min4    LDA  fours,y    ;minutes must be 40+
        AND  #$F0
        STA  SPRITEA,y
        DEY
        BPL  min4
        LDA  #$28       ;40+ minutes
        JMP  minload

min3    LDA  threes,y   ;minutes must be 30+
        AND  #$F0
        STA  SPRITEA,y
        DEY
        BPL  min3
        LDA  #$1E       ;30+ minutes
        JMP  minload

min2    LDA  twos,y     ;minutes must be 20+
        AND  #$F0
        STA  SPRITEA,y
        DEY
        BPL  min2
        LDA  #$14
        JMP  minload    ;20+ minutes

min1    LDA  ones,y     ;minutes must be 10+
        AND  #$F0
        STA  SPRITEA,y
        DEY
        BPL  min1
        LDA  #$0A       ;10+ minutes

minload STA  TEMP       ;the accumulator had the 10s of minutes
        LDA  RMINS      ;now we subtract the 10s minutes from the real
        SEC             ;minutes to get the 1s minutes to act as a pointer
        SBC  TEMP       ;for the data tables for 2nd half of sprite
        ASL             ;double the number
        TAX
        LDA  numblk,x   ;load the first half of the sprite data address
        STA  TEMP
        LDA  numblk+1,x ;load the second half of the sprite table address
        STA  TEMP+1

        LDY  #$06       ;number of lines in the sprite (-1)
msload  LDA  (TEMP),y   ;get the sprite data
        AND  #$0F       ;strip off the last 4 bits
        ORA  SPRITEA,y  ;combine the 1st half with the 2nd half
        STA  SPRITEA,y  ;put it back in the sprite memory
        DEY
        BPL  msload     ;get the next line of data

        LDY  #$06       ;preload number of sprite lines (-1)
        LDA  #$18       ;24 hours
        SEC
        SBC  HOURS      ;subtract the counter hours to get
        STA  RHOURS     ;the real hours value
        CMP  #$00       ;see if it's 12:00 am
        BEQ  hour0
        CMP  #$14       ;see if it's 20+ hours
        BPL  hour2
        CMP  #$0A       ;see if it's 10+ hours
        BPL  hour1

hour0   LDA  zeros,y    ;load the zeros sprite data
        AND   #$F0      ;strip the 1st four bits
        STA  SPRITEB,y  ;store to the 2nd sprite memory
        DEY
        BPL  hour0
        LDA  #$00       ;same deal as with the minutes
        JMP  loadhrs    ;but now we load the second half of the hours data

hour1   LDA  ones,y
        AND  #$F0
        STA  SPRITEB,y
        DEY
        BPL  hour1
        LDA  #$0A
        JMP  loadhrs

hour2   LDA  twos,y
        AND  #$F0
        STA  SPRITEB,y
        DEY
        BPL  hour2
        LDA  #$14
        JMP  loadhrs

loadhrs STA  TEMP
        LDA  RHOURS
        SEC
        SBC  TEMP
        ASL
        TAX
        LDA  numblk,x
        STA  TEMP
        LDA  numblk+1,x
        STA  TEMP+1

        LDY  #$06
hsload  LDA  (TEMP),y
        AND  #$0F
        ORA  SPRITEB,y
        STA  SPRITEB,y
        DEY
        BPL  hsload
        RTS

numblk  .word  zeros    ;where all the sprites are at
        .word  ones
        .word  twos
        .word  threes
        .word  fours
        .word  fives
        .word  sixes
        .word  sevens
        .word  eights
        .word  nines

draw    LDA  INTIM      ;check to see if it's time to draw a frame
        BNE  draw
        STA  WSYNC
        STA  HMOVE
        STA  VBLANK     ;turn the screen on!

;insert  display kernel

        LDX  #$3F       ;okay, this display kernal sucks, but I'm not doing
blow1   STA  WSYNC      ;much here so I didn't go for anything fancy since
        DEX             ;this is just a demo. This wouldn't be the way you
        BPL  blow1      ;do things in a game, but it works for this.
        STA  WSYNC
        NOP             ;See... you'd never do something weenie like this
        NOP             ;in a real programme
        NOP             ;
        NOP             ;
        NOP             ;but when I was experimenting with this programme
        NOP             ;I just had a whole bunch of ";nop" lines here
        NOP             ;and I removed the ";" until I got the spacing more
        NOP             ;or less where I wanted it
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        STA  RESP0
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        NOP
        STA  RESP1

        LDY  #$06
sload   LDA  SPRITEB,y
        STA  GP0
        LDA  SPRITEA,y
        STA  GP1
        STA  WSYNC      ;you wouldn't do something weenie like this
        STA  WSYNC      ;either in a real programme, but it was an
        STA  WSYNC      ;easy way to make each sprite 8 lines high
        STA  WSYNC      ;and I was more concerned with making a working
        STA  WSYNC      ;and accurate clock than a nice display.
        STA  WSYNC
        STA  WSYNC
        STA  WSYNC
        DEY
        BPL  sload
        LDA  #$00
        STA  GP0
        STA  GP1

        LDX  #$48
blow2   STA  WSYNC      ;now we just blow the rest of the unused scanlines.
        DEX
        BPL  blow2
        RTS


clear   LDA  #$24       ;set timer for overscan
        STA  TIM64T
        LDA  #$02       ;clear the screen and turn off the video
        STA  WSYNC
        STA  VBLANK
        LDA  #$00
        STA  PF0
        STA  PF1
        STA  PF2
        STA  COLUPF
        STA  COLUBK

        LDA  #$3C       ;this is the clock routine itself. it counts
        DEC  FRAMES     ;down from 60 frames, and then decreases the
        BNE  joy        ;seconds, which count down the minutes and then
        LDA  #$3C       ;the hours.. etc. For whatever reason my 2600
        STA  FRAMES     ;wasn't running at exactly 60 frames a second
        DEC  SECS       ;so there were two lines inserted to correct
        BNE  joy        ;timing accuracy problems
        STA  SECS
        DEC  SECS       ;here's one. Kept me from losing a second every
        DEC  MINS       ;minute
        BNE  joy
        STA  MINS
        LDA  #$18
        INC  SECS       ;here's the other. It kept me from gaining a
        DEC  HOURS      ;second every hour.
        BNE  joy
        STA  HOURS
                        ;now my timing inaccuracies may have been caused
                        ;by either my V-blank, V-sync, Overscan, or
                        ;display being a few scanlines too long or short.
                        ;theoretically if all my lines were bang on,
                        ;I wouldn't have needed those two seconds counter
                        ;corrections. But with them inplace, it allows me
                        ;to be a little looser with my code which works for
                        ;me. It may still gain or lose a second every 60
                        ;hours, but I can live with that. And since I'll
                        ;be employing this clock in a virtual pet game and
                        ;not a swiss made olympic time piece, a little
                        ;inaccuracy won't matter.

joy     LDA  SWCHA      ;load joysticks
        ORA  #$0f       ;strip the data for player #2 joystick
        CMP  #$ef       ;up
        BEQ  up
        CMP  #$df       ;down
        BEQ  down
        CMP  #$bf       ;left
        BEQ  left
        CMP  #$7f       ;right
        BEQ  right
        LDA  #$00       ;no movement
        STA  JOYDEL     ;reset the joystick delay variable
        LDA  #$01       ;reset the first move variable
        STA  JOY1ST
        JMP  oscan      ;finish off the overscan

up      LDA  HOURS      ;check to see if we've run out our hours
        CMP  #$01
        BEQ  oscan      ;yep, then ignore the movement
        INC  JOYDEL     ;increase the joystick delay variable
        LDA  JOY1ST     ;check to see if this is the first move in this
        CMP  #$01       ;direction.
        BEQ  now1       ;if it is then change the variable now
        LDA  #$1E       ;nope then see if there's been enough of a delay
        CMP  JOYDEL     ;to change the variable yet.
        BNE  oscan
now1    LDA  #$00       ;reset the joystick delay and set the first move
        STA  JOY1ST     ;indicator to "no"
        STA  JOYDEL
        DEC  HOURS      ;decrease the hours counter
        JMP  oscan

down    LDA  HOURS
        CMP  #$18
        BEQ  oscan
        INC  JOYDEL
        LDA  JOY1ST
        CMP  #$01
        BEQ  now2
        LDA  JOYDEL
        CMP  #$1E
        BNE  oscan
now2    LDA  #$00
        STA  JOY1ST
        STA  JOYDEL
        INC  HOURS      ;increase the hours counter
        JMP  oscan

left    LDA  MINS
        CMP  #$01
        BEQ  oscan
        INC  JOYDEL
        LDA  JOY1ST
        CMP  #$01
        BEQ  now3
        LDA  #$1E
        CMP  JOYDEL
        BNE  oscan
now3    LDA  #$00
        STA  JOY1ST
        STA  JOYDEL
        DEC  MINS       ;decrease the minutes counter
        JMP  oscan

right   LDA  MINS
        CMP  #$3c
        BEQ  oscan
        INC  JOYDEL
        LDA  JOY1ST
        CMP  #$01
        BEQ  now4
        LDA  #$1E
        CMP  JOYDEL
        BNE  oscan
now4    LDA  #$00
        STA  JOY1ST
        STA  JOYDEL
        INC  MINS       ;increase the minutes counter

oscan   LDA  INTIM      ;see if the timer has run out
        BNE  oscan
        STA  WSYNC
        RTS

zeros   .byte %11100111 ;sprites are stored upsidedown, and there
        .byte %10100101 ;are two copies of each number in each sprite
        .byte %10100101 ;location. The unwanted number is stripped
        .byte %10100101 ;with the AND command (AND #$0F for the right
        .byte %10100101 ;number stripped, AND #F0 for the left)
        .byte %10100101 ;then any two numbers can be combined with an
        .byte %11100111 ;OR command. Neat huh?

ones    .byte %11100111
        .byte %01000010
        .byte %01000010
        .byte %01000010
        .byte %01000010
        .byte %11000110
        .byte %01000010

twos    .byte %11100111
        .byte %10000100
        .byte %10000100
        .byte %11100111
        .byte %00100001
        .byte %00100001
        .byte %11100111

threes  .byte %11100111
        .byte %00100001
        .byte %00100001
        .byte %11100111
        .byte %00100001
        .byte %00100001
        .byte %11100111

fours   .byte %00100001
        .byte %00100001
        .byte %00100001
        .byte %11100111
        .byte %10100101
        .byte %10100101
        .byte %10000100

fives   .byte %11100111
        .byte %00100001
        .byte %00100001
        .byte %11100111
        .byte %10000100
        .byte %10000100
        .byte %11100111

sixes   .byte %11100111
        .byte %10100101
        .byte %10100101
        .byte %11100111
        .byte %10000100
        .byte %10000100
        .byte %11000110

sevens  .byte %10000100
        .byte %10000100
        .byte %10000100
        .byte %01000010
        .byte %00100001
        .byte %00100001
        .byte %11100111

eights  .byte %11100111 ;This code is (c)1997 by Chris "Crackers" Cracknell
        .byte %10100101 ;and is placed in the Public Domain by the author.
        .byte %10100101 ;Anyone is free to employ and distribute this code
        .byte %11100111 ;as they see fit.
        .byte %10100101 ;
        .byte %10100101 ;
        .byte %11100111 ;
                        ;
nines   .byte %00100001 ;Well... if you're going to use this code in a
        .byte %00100001 ;"Doomsday Machine" to destroy the world, then
        .byte %00100001 ;I would rather you didn't. But otherwise, knock
        .byte %11100111 ;yourself out with it.
        .byte %10100101 ;
        .byte %10100101 ;Actually... if the "Doomsday Machine" is just in
        .byte %11100111 ;a game, then it's okay to use the code.
                        ;
        org $FFFC       ;Unless it's like the movie "War Games" where the
        .word start     ;computer running the game is hooked up to a real
        .word start     ;"Doomsday Machine" then it wouldn't be a good idea.



