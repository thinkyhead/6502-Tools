#!/usr/bin/env bash
#
# bAtariBASIC-build.sh
#

# Get 'helper' path and user environment
SELF="$0" ; ORIG=$(readlink "$SELF")
[ "$ORIG" == "" ] || SELF=$ORIG
HERE=$(dirname "$SELF")
source "$HERE/AtariTools.sh"

PATH="$bB/source:$PATH"
which preprocess || { echo "Building bAtariBasic..." ;  cd $bB/source ; make ; echo ; }

echo -n "Starting build of " ; echo -n $( basename "$1" ) ...

# Process and Compile bAtariBasic to 6502 Assembly Language
preprocess < "$1" | 2600basic -i "$bB" > bB.asm

# Exit on fail
[ "$?" -ne "0" ] && { echo "Compilation failed."; exit; }

# Post-process and optionally Optimize the ASM source code
if [ "$2" = "-O" ]
  then
   postprocess -i "$bB" | optimize > "$1.asm"
  else
   postprocess -i "$bB" > "$1.asm"
fi

# Assemble the 6502 code into a binary
dasm "$1.asm" -I"$bB/includes" -f3 -o"$1.bin" | sed '/Label mismatch/d' \
| sed '/shakescreen/d;/rand16/d;/debugscore/d;/pfscore/d;/noscore/d;/vblank_bB_code/d;/PFcolorandheight/d;/pfrowheight/d;/pfres/d;/PFmaskvalue/d;/overscan_time/d;/vblank_time/d;/no_blank_lines/d;/superchip/d;/ROM2k/d;/NO_ILLEGAL_OPCODES/d;/minikernel/d;/debugcycles/d;/mincycles/d;/legacy/d;/PFcolors/d;/playercolors/d;/player1colors/d;/backgroundchange/d;/readpaddle/d;/multisprite/d;/PFheights/d;/bankswitch/d;/Unresolved Symbols/d' \
| sed '2,/-->/!{ /-->/,/-->/d; }' \
| sed 's/--> 0./Possible duplicate label: /'

# Exit on fail
[ "$?" -ne "0" ] && { echo "Assembly failed."; exit; }

echo "Build complete."

# Run the binary in one or more emulators
[ -d "$z26" ] && "$z26/Contents/MacOS/z26" "$1.bin"
[ -d "$Stella" ] && "$Stella/Contents/MacOS/Stella" "$1.bin"

exit
