 rem SYNTAX TEST "Packages/User/AtariTools/bAtariBASIC"

 rem bB syntax color test!
 rem ^comment.batari

 set kernel_options no_blank_lines background
 rem <-keyword.set.batari
 rem ^support.name.builtin.batari
 rem                ^constant.enum.batari
 rem                               ^constant.enum.batari

 set kernel_options no_such_option
 rem                ^invalid.error.syntax.batari

 dim _fpoint=a.b
 rem <-keyword.dim.batari
 rem  ^entity.name.alias.batari
 rem        ^keyword.operator.assignment.batari
 rem         ^variable.int.batari
 rem          ^keyword.operator.fixed-point.batari
 rem           ^variable.int.batari

     u=80
 rem ^variable.int.batari
 rem  ^keyword.operator.assignment.batari
 rem   ^constant.numeric.int.batari

     _fpoint=250.123
 rem ^variable.alias.batari
 rem        ^keyword.operator.assignment.batari
 rem         ^constant.numeric.fixed-point.batari

     COLUPF=2
 rem ^support.name.builtin.batari
 rem       ^keyword.operator.assignment.batari
 rem        ^constant.numeric.int.batari

premain
 rem ^entity.name.label.batari

     pfclear
 rem ^keyword.pfclear.batari

     if switchreset then goto main
 rem ^keyword.if.batari
 rem    ^support.name.builtin.batari
 rem                ^keyword.then.batari
 rem                     ^keyword.goto.batari
 rem                          ^entity.name.label.batari

     goto premain
 rem ^keyword.goto.batari
 rem      ^entity.name.label.batari

main

     COLUP0=28
 rem ^support.name.builtin.batari
 rem       ^keyword.operator.assignment.batari
 rem        ^constant.numeric.int.batari
     COLUP1=28
 rem ^support.name.builtin.batari
 rem       ^keyword.operator.assignment.batari
 rem        ^constant.numeric.int.batari

 rem The following can't be broken by comments,
 rem though an exception can be made for testing.
     player0:
 rem ^keyword.playerN.batari
     %00001111
 rem ^constant.numeric.binary.batari
     %00110001
     %01000001
     %10000001
     %11111111
     %10000001
     %01000001
     %00110001
     %00001111
end
 rem <-keyword.end.batari

     player1:
 rem ^keyword.playerN.batari
     %11100000
     %00011000
     %00000100
     %00000010
     %11111110
     %00000010
     %00000100
     %00011000
     %11100000
end

     lives:
 rem ^keyword.lives.batari
     %00000000
     %00111000
     %01111100
     %01111100
     %11111110
     %11111110
     %01111100
     %01111100
     %00111000
end

     goto background
 rem ^keyword.goto.batari
 rem      ^entity.name.label.batari

main2

 player0x=u
 player0y=v
 player1x=w
 player1y=v

 if joy0up then v=v-1
 if joy0down then v=v+1

 if joy0left then u=u-1: w=w-1
 if joy0right then u=u+1: w=w+1

 if w=153 then w=152
 if u=145 then u=144
 if u=0 then u=1
 if w=8 then w=9
 if v=86 then v=85
 if v=9 then v=10

 drawscreen

   asm
   ldx #47
   lda #0
playfieldclear
   sta playfield,x
   dex
   bne playfieldclear
end

  data _dataName
  1, 2, 78, 12.5
end

slow
 m=m+1:if m<10 then goto slow
 m=0

 goto main

background

 playfield:
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ...XX.................XX........
 XX.XX..X..............XX....X...
 XX.XX..XX.............XX...XX...
 XX.XX..XX.....XX.....XXX...XX...
 XXXXXXXXX....XXXX...XXXXXXXXXXXX
end
 goto main2
