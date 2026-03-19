#!/usr/bin/env bash
#
# AtariVCS-build.sh
# Assemble the given source file with DASM
#

# Get 'helper' path and user environment
SELF="$0" ; ORIG=$(readlink "$SELF")
[ "$ORIG" == "" ] || SELF=$ORIG
HERE=${SELF%/*}
source "$HERE/AtariTools-settings.sh"

case "$#" in
  1 ) INSRC=$1 ;;
  2 ) EXTRA=$1 ; INSRC=$2 ;;
  * ) echo "Usage: ${0##*/} [run] filename" 1>&2 ; exit 1 ;;
esac

# Validate the input file type
[[ $INSRC =~ \.(s|asm|dasm|inc|6502)$ ]] || { echo "Not Valid source file" ;  exit 1 ; }

# The output file has the base name with .bin
OUTFILE="${INSRC%.*}.bin"

# Build the project
echo -n "Starting build of ${INSRC##*/} ..."
dasm "$INSRC" -f3 -o"$OUTFILE" -I"$VCS_DIR" -E2 | sed '/(.+\\..+):(\\d+): ()(error: .*)|()()()--- (\\d+ Unresolved Symbols?)/d'

# Exit on fail
[ "$?" -ne "0" ] && { echo "Assembly failed." ; exit ; }

# Exit if not running
if [[ $EXTRA == "run" ]]; then

  # On success, run the binary in an emulator
  echo -n "Starting emulator..."
  if [ -d "$z26" ]; then
    "$z26/Contents/MacOS/z26" "$OUTFILE"
  elif [ -d "$STELLA" ]; then
    "$STELLA/Contents/MacOS/Stella" "$OUTFILE"
  else
    open -a Stella "$OUTFILE" --args -tv.phosblend 20 -tv.filter 1 -tv.scanlines 40 || \
      echo "No Atari 2600 emulator found!"
  fi

fi

echo ; echo ; exit
