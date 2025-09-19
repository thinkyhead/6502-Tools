#!/usr/bin/env bash
#
# Atari800-build.sh
#
# Uses ca65 + ld65 to build the active source file
# On success runs the resulting executable in the emulator
# Expects a .lnk or .cfg file with the same base name as the source file
#

# Get 'helper' path and user environment
SELF="$0" ; ORIG=$(readlink "$SELF")
[ "$ORIG" == "" ] || SELF=$ORIG
HERE=$(dirname "$SELF")
source "$HERE/AtariTools.sh"

echo -n "Starting build of " ; echo -n $( basename "$1" ) ...

# Output filename for atari800 executable
INSRC="$1"
INBASE="${INSRC/.s/}"
INBASE="${INBASE/.asm/}"
INBASE="${INBASE/.inc/}"
INBASE="${INBASE/.a65/}"
INBASE="${INBASE/.6502/}"

IN_LNK="$INBASE.lnk"
[[ -f "$IN_LNK" ]] || IN_LNK="$INBASE.cfg"
[[ -f "$IN_LNK" ]] || { echo "Required file '$INBASE.lnk'/'$INBASE.cfg' not found."; exit; }

OUT_LISTING="$INBASE.txt"
OUT_O="$INBASE.o"
OUT_BIN="$INBASE.bin"

# Build the project
ca65 "$INSRC" -l "$OUT_LISTING" -o "$OUT_O" \
  && ld65 -o "$OUT_BIN" -C "$IN_LNK" "$OUT_O"

# Exit on fail
[ "$?" -ne "0" ] && { echo "Assembly failed."; exit; }

# Find emulator if not already set
[[ -f $ATARI800 ]] || ATARI800=$( which atari800 )

# On success, run the binary in an emulator
if [[ -f $ATARI800 ]]; then
  echo -n "Starting emulator..."
  "$ATARI800" -atari -nobasic -run "$OUTFILE"
  echo
else
  echo "No Atari 800 emulator found!"
fi

exit
