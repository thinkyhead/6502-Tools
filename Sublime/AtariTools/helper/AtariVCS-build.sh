#!/usr/bin/env bash
#
# bAtariVCS-build.sh
# Assemble the given source file with DASM
#

# Get 'helper' path and user environment
SELF="$0" ; ORIG=$(readlink "$SELF")
[ "$ORIG" == "" ] || SELF=$ORIG
HERE=$(dirname "$SELF")
source "$HERE/AtariTools.sh"

echo -n "Starting build of " ; echo $( basename "$1" ) ...

# Clean up the output filename
INSRC="$1"
INBASE="${INSRC/.s/}"
INBASE="${INBASE/.asm/}"
INBASE="${INBASE/.inc/}"
INBASE="${INBASE/.6502/}"
OUTFILE="$INBASE.bin"

# Build the project
dasm "$INSRC" -f3 -o"$OUTFILE" -I"$VCS_DIR" -E2 | sed '/(.+\\..+):(\\d+): ()(error: .*)|()()()--- (\\d+ Unresolved Symbols?)/d'

# Exit on fail
[ "$?" -ne "0" ] && { echo "Assembly failed."; exit; }

# On success, run the binary in an emulator
if [[ -d "$z26" || -d "$Stella" ]]; then
  echo -n "Starting emulator..."
  [ -d "$z26" ] && "$z26/Contents/MacOS/z26" "$OUTFILE"
  [ -d "$Stella" ] && "$Stella/Contents/MacOS/Stella" "$OUTFILE"
  echo
else
  echo "No Atari 2600 emulator found!"
fi

exit
