#!/usr/bin/env bash
#
# CommodoreBASIC-run.sh
#
# Usage: CommodoreBASIC-run.sh [--run|--save|--list|--edit] [--warp] [--joy] filename
#
# Convert L64 to PRG and optionally run the given file.
#
# Unlike the Atari we cannot send over encoded text converted literally to PETSCII.
# x64 has a -keybuf parameter taking the actual keystrokes to type out a program so
# so it can run directly. This is not the fastest or prettiest way to run the program.
#
# Instead use 'petcat' to do conversion from a file input.
#
#   petunia.py --cat helloworld.L64 > helloworld.P64
#   petcat -w2 -o helloworld.PRG -- helloworld.P64
#   x64 -autostartprgmode 1 +mouse -- helloworld.PRG
#
# The best place to store the compiled PRG is in the same folder with the L64.
# (Once the Atari Tools can make BAS files it will apply the same behavior.)
#

# Get 'helper' path and user environment
SELF="$0" ; ORIG=$(readlink "$SELF")
[ "$ORIG" == "" ] || SELF=$ORIG
HERE=${SELF%/*}
source "$HERE/C64Tools-settings.sh"

perror() { echo -e "$0: \033[0;31m$1 -- $2\033[0m" ; }

#echo "ARGS = $@"
#echo "ARGS is $@" ; exit 1

while getopts 'hlsvw-:' OFLAG; do
  case "${OFLAG}" in
    e) EDIT_UNI=1 ;;
    h) EXIT_USAGE=1 ;;
    j) JOYPAD=1 ;;
    l) LIST_PRG=1 ;;
    s) SAVE_PRG=1 ;;
    v) DEBUG=1 ;;
    w) VWARP=1 ;;
    -) ONAM="${OPTARG%%=*}" ; OVAL="${OPTARG#*=}"
       case "$ONAM" in
         edit) EDIT_UNI=1 ;;
         help) [[ -z "$OVAL" ]] || perror "option can't take value $OVAL" $ONAM ; EXIT_USAGE=1 ;;
          joy) JOYPAD=1 ;;
         list) LIST_PRG=1 ;;
         save) SAVE_PRG=1 ;;
debug|verbose) DEBUG=1 ;;
         warp) VWARP=1 ;;
       esac
       ;;
    *) EXIT_USAGE=2 ;;
  esac
done
shift $((OPTIND - 1))

((EXIT_USAGE)) && { echo "Usage: ${0##*/} [--run|--save|--list|--edit] [--warp] filename" 1>&2 ; exit 1 ; }

set -e
shopt -s nocasematch

INFILE=$1
if [[ $INFILE =~ ".P64" ]]; then
  P64FILE=$INFILE
elif [[ $INFILE =~ ".L64" || $INFILE =~ ".PRG" ]]; then
  L64FILE=$INFILE
  P64FILE="${INFILE%.*}.P64"
fi

#
# --edit : Convert PRG or P64 to Unicode / ASCII L64
#
if [[ -n $EDIT_UNI ]]; then

  if [[ $INFILE =~ ".PRG" ]]; then
    echo "Creating intermediate file ${P64FILE##*/} ..."
    echo "petcat -nh -nc \"$PRGFILE\" > \"$P64FILE\""
    "$VICEHOME/petcat" -nh -nc "$PRGFILE" > "$P64FILE"
  elif [[ ! $INFILE =~ ".P64" ]]; then
    echo "${0##*/} : Not a PRG or or P64 file??"
    sleep 30
    exit 1
  fi

  # Convert VICE listing to Unicode + ASCII
  L64FILE="${P64FILE%.*}.L64"
  echo "petunia.py -u -a -s \"$P64FILE\" >\"$L64FILE\""
  "$HERE/petunia.py" -u -a -s "$P64FILE" >"$L64FILE"
  open -a "Sublime Text" "$L64FILE"

  exit
fi

# Make a keymap file that petcat can tokenize
if [[ $INFILE =~ ".L64" ]]; then
  echo "Creating intermediate file ${P64FILE##*/} ..."
  # Convert the UTF-8 file to PETSCII suitable for petcat (and -keybuf?)
  echo "petunia.py -p -k \"$L64FILE\" >\"$P64FILE\""
  "$HERE/petunia.py" -p -k "$L64FILE" >"$P64FILE"
elif [[ ! $INFILE =~ ".P64" ]]; then
  echo "${0##*/} : Not an L64 or P64 file??"
  sleep 30
  exit 1
fi

# Final output of petcat is a PRG file VICE will like
PRGFILE="${P64FILE%.*}.PRG"

# Assuming no errors, convert to PRG with petcat
echo "Converting \"${P64FILE##*/}\" to \"${PRGFILE##*/}\""
echo "petcat -w2 -o \"$PRGFILE\" -- \"$P64FILE\""
"$VICEHOME/petcat" -w2 -o "$PRGFILE" -- "$P64FILE"
echo "Conversion done."

# List PRG and load back into Sublime for verification
((LIST_PRG)) && { "$VICEHOME/petcat" -nh "$PRGFILE" > /tmp/LIST.P64 && open -a "Sublime Text" /tmp/LIST.P64 ; }

# If not running then exit now
((SAVE_PRG || LIST_PRG)) && exit 0

# Warp mode enabled by default?
((VWARP)) && VICEOPTS="-warp $VICEOPTS"
((JOYPAD)) && VICEOPTS="-joydev1 2 $VICEOPTS"
((LIST_PRG)) && VICEOPTS="-keybuf list\\n $VICEOPTS"

#
# VICE
# @ https://sourceforge.net/projects/vice-emu
# The vice package can be installed from MacPorts, Homebrew, etc.
#
X64=x64sc
echo
echo "VICE: Creating a new '$X64' instance."
echo "      Running \"${PRGFILE##*/}\""
echo "      Press Command-W to toggle Warp Mode."
echo

# Run the program in VICE using Direct Injection
echo "$X64 -autostartprgmode 1 +mouse $VICEOPTS -- \"$PRGFILE\""
"$VICEHOME/$X64" -autostartprgmode 1 +mouse $VICEOPTS -- "$PRGFILE" >/dev/null
