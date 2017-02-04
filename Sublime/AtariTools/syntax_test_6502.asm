; SYNTAX TEST "Packages/User/AtariTools/6502"
; <- source.asm
; <- punctuation.definition.comment.line.start

    ; Whole line comment    
; ^-comment.asm
;                          ^-comment.asm
; ^-punctuation.definition.comment.line.start

 LIST OFF
; ^keyword.directive.list.asm
;      ^constant.list-option.asm

   processor 6502 ; comment
;   ^keyword.directive.processor.asm
;             ^constant.processor-option.asm
;                   ^comment.asm

TIA_BASE_READ_ADDRESS = $30  ; comment
; ^entity.name.constant.asm
;                     ^keyword.operator.assignment.asm
;                        ^constant.numeric.hex.asm
;                           ^-comment.asm
;                            ^punctuation.definition.comment.eol.start
;                               ^comment.asm

   include test.h
;   ^keyword.directive.include.asm
;           ^string.unquoted.path.asm

   LIST ON
;  ^keyword.directive.list.asm
;       ^constant.list-option.asm

   IF COMPILE_VERSION = NTSC        ; comment
;  ^keyword.directive.conditional.asm
;      ^entity.name.asm
;                     ^keyword.operator.compare.asm
;                        ^entity.name.asm
;                                ^-comment.asm
;                                    ^comment.asm

      jsr Waste12Cycles
;     ^support.opcode.double.asm
   ELSE                             ; comment
      nop                           ; waste 12 cycles
;     ^support.opcode.single.asm
;                                     ^comment.asm
   ENDIF                            ; comment

MAX_HOLD_CANDY          = 9 << 4    ; comment
; ^entity.name.constant.asm
;                       ^keyword.operator.assignment.asm
;                         ^constant.numeric.dec.asm
;                            ^keyword.operator.binary.asm
;                              ^constant.numeric.dec.asm
;                                     ^comment.asm
FOUR_DIAMOND_PITS_CANDY = %0001     ; comment
;                         ^constant.numeric.bin.asm
;                                     ^comment.asm
SHOW_HSW_INITIALS_VALUE = $69       ; comment
;                         ^constant.numeric.hex.asm
;                                     ^comment.asm
ET_DEAD                 = %10000000 ; comment
;                             ^constant.numeric.bin.asm
;                                     ^comment.asm
tempNumberFonts         = temp
;                         ^entity.name.asm
bankSwitchStrobe        = displayBank99+1
;                         ^entity.name.asm
;                                      ^keyword.operator.math.asm
;                                       ^constant.numeric.dec.asm

   *=$0600 + 1
;  ^support.builtin.origin.asm
   SEG Bank0                   ; comment
;  ^keyword.directive.seg.asm
;      ^entity.name.asm
;                               ^comment.asm
   BOUNDARY 0
;  ^keyword.directive.boundary.asm
;           ^constant.numeric.dec.asm
   .org BANK0TOP
;  ^keyword.directive.org.asm
;       ^entity.name.asm
   .rorg BANK0_REORG
;  ^keyword.directive.org.asm
;        ^entity.name.asm

HorizPositionObjects
; ^entity.name.label.asm
   lda #<(MOVE_UP >> 4)        ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
;         ^entity.name.asm
   lda #5                      ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
   adc #-$F5                   ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
   cmp #'X'-1                  ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
   ldx #PLAY_SOUND_CHANNEL0 | $1C           ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
;       ^entity.name.asm
   ldy #NECK_EXTENDED | NECK_DECENDING | 3  ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
;       ^entity.name.asm
   cpx #H_MOTHERSHIP * 2       ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
;       ^entity.name.asm
   cpy #<RESBL - RESP0         ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
;         ^entity.name.asm
;              ^keyword.operator.math.asm
   lda #<-1                    ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
   lda HMOVETable,y            ; comment
;  ^support.opcode.double.asm
;   ^-support.opcode.single
;   ^-support.opcode.added
;   ^-support.opcode.mos65C02
;       ^entity.name.asm
;                               ^comment.asm
   lda $B000 + 5              comment comment
;  ^support.opcode.double.asm
;                               ^comment.asm
   and #<(~SET_STARTING_SCREEN)
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
;         ^keyword.operator.binary.asm
   and #$0F                         ; comment
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
;                                     ^comment.asm

.moveObjectLoop
; ^entity.name.label.asm
   sta WSYNC                        ; comment
;  ^support.opcode.double.asm
;                                     ^comment.asm
   lda currentObjectHorizPos,x      ; comment
;  ^support.opcode.double.asm
;                               ^-comment.asm
;                                     ^comment.asm
   adc   #(1 * 16)                  ; comment
;  ^support.opcode.double.asm
;        ^keyword.operator.immediate.asm
;          ^constant.numeric.dec.asm
;              ^constant.numeric.dec.asm
;                              ^-support.opcode
;                              ^-comment.asm
;                                     ^comment.asm
   stp                              ; comment
;  ^support.opcode.65C02.asm
;                              ^-support.opcode
;                              ^-comment.asm
;                                     ^comment.asm

   cmp #H_MOTHERSHIP / 2
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm
;                    ^keyword.operator.math.asm
;                      ^constant.numeric.dec.asm

   cmp #ID_TITLE_SCREEN
;  ^support.opcode.double.asm
;      ^keyword.operator.immediate.asm

   bpl .coarseMoveObject
;  ^support.opcode.double.asm

VerticalSync SUBROUTINE
; ^entity.name.label.asm
;            ^keyword.directive.misc.asm

COARSE_MOTION SET 0
; ^entity.name.constant.asm
;              ^keyword.operator.assignment.asm
;                 ^constant.numeric.dec.asm

   REPEAT 8
;  ^keyword.directive.misc.asm
;         ^constant.numeric.dec.asm

COARSE_MOTION SET COARSE_MOTION + 1 ; comment again
;^entity.name.constant.asm
;              ^keyword.operator.assignment.asm
;                               ^keyword.operator.math.asm
;                                 ^constant.numeric.dec.asm

   .byte HMOVE_L7 | COARSE_MOTION
;  ^keyword.data.asm
;        ^entity.name.asm

   REPEND  ; comment
;  ^keyword.directive.misc.asm
;            ^comment.asm

Hexdigdata  .byte "01234567\"9ABCDEF"
;^entity.name.label.asm
;            ^keyword.data.asm
;                 ^punctuation.definition.string.begin.asm
;                  ^string.quoted.asm
;                           ^constant.character.escape.asm
;                               ^string.quoted.asm

   .byte 19,$32,%1001 ; comment
;  ^keyword.data.asm
;        ^constant.numeric.dec.asm
;            ^constant.numeric.hex.asm
;                ^constant.numeric.bin.asm
;                      ^comment.asm

   .byte $00          ; |........|
;  ^keyword.data.asm
;         ^constant.numeric.hex.asm
;                     ^comment.asm

   .byte <Telephone,<Telephone - 3,<Telephone - 5,<Telephone - 9
;  ^keyword.data.asm
;        ^keyword.operator.binary.asm
;         ^entity.name.asm
;                  ^punctuation.separator.data.asm

   .byte ID_WARP_UP_ZONE << 4 | ID_RETURN_HOME_ZONE
;  ^keyword.data.asm

   .word StartPos, EndPos
;  ^keyword.data.asm
;        ^entity.name.asm
;                ^punctuation.separator.data.asm


   ora #[~(MOVE_RIGHT) >> 4] & 15  don't allow E.T. to move horiz
;  ^support.opcode.double.asm
;       ^punctuation.definition.brace.open
;        ^keyword.operator.binary.asm
;         ^punctuation.definition.parentheses.open
;          ^entity.name.asm
;                    ^punctuation.definition.parentheses.close
;                      ^keyword.operator.binary.asm
;                         ^constant.numeric.dec.asm
;                          ^punctuation.definition.brace.close
;                            ^keyword.operator.binary.asm
;                               ^constant.numeric.dec.asm
;                                  ^character.auto-comment.asm

;
; Apple ][ old-school 6502 tests
;

**********************
* Whole line comment *
**********************
; ^comment.asm

// comment allowed
; ^comment.asm


   HEX F5F6F7
;  ^keyword.data.hex.asm
;       ^constant.numeric.hex.asm

*
* Disassembly Output
*

$1234 56 7F   STAR LDA #5   Comment
; ^constant.disassembly.asm
;              ^entity.name.label.asm
;                    ^support.opcode.double.asm
;                      ^keyword.operator.immediate.asm
;                       ^constant.numeric.dec.asm
;                             ^comment.asm

$1234 567F94        STA PLACE   *** Comment ***
; ^constant.disassembly.asm
;                   ^support.opcode.double.asm
;                        ^entity.name.asm
;                                ^punctuation.definition.comment.eol.start
;                                  ^comment.asm

