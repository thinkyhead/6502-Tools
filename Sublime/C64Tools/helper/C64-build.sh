#!/usr/bin/env bash
#
# C64-build.sh
#
# Uses ca65 + ld65 to build the active source file
# On success runs the resulting executable in the emulator
# Expects a .lnk or .cfg file with the same base name as the source file
#

# Get 'helper' path and user environment
SELF="$0" ; ORIG=$(readlink "$SELF")
[ "$ORIG" == "" ] || SELF=$ORIG
HERE=${SELF%/*}
source "$HERE/C64Tools-settings.sh"

echo -n "Starting build of ${1##*/} ..."

# Output filename for C64 executable
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
[[ -f $VICEHOME ]] || VICEHOME=$( which x64 )

# On success, run the binary in an emulator
# Assume that it has a BASIC loader
if [[ -f $VICEHOME ]]; then
  echo -n "Starting VICE emulator..."
  "$VICEHOME/x64 " -- "$OUTFILE"
  echo
else
  echo "No Commodore 64 emulator found!"
fi
