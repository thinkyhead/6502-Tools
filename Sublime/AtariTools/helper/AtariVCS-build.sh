#!/usr/bin/env bash
#
# bAtariVCS-build.sh
#

# Get 'helper' path and user environment
SELF="$0" ; ORIG=$(readlink "$SELF")
[ "$ORIG" == "" ] || SELF=$ORIG
HERE=$(dirname "$SELF")
source "$HERE/AtariTools.sh"

echo -n "Starting build of " ; echo -n $( basename "$1" ) ...

# Clean up the output filename
OUTFILE="$1.bin"
OUTFILE="${OUTFILE/.asm.bin/.bin}"
OUTFILE="${OUTFILE/.s.bin/.bin}"

# Build the project
dasm "$1" -f3 -o"$OUTFILE" -I"$VCS_DIR" -E2 | sed '/(.+\\..+):(\\d+): ()(error: .*)|()()()--- (\\d+ Unresolved Symbols?)/d'

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
