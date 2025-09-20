#!/usr/bin/env bash
#
# AtariBASIC-run.sh
#
# Usage: AtariBASIC-run.sh [macx|turbo|save] filename
#
# macx - Use Atari800MacX to run the program. Otherwise atari800 is used.
# turbo - If using atari800, start it in Turbo mode.
# save - Tell the emulator to save FILENAME.BAS - i.e., Just tokenize the program.
#

# Get 'helper' path and user environment
SELF="$0" ; ORIG=$(readlink "$SELF")
[ "$ORIG" == "" ] || SELF=$ORIG
HERE=${SELF%/*}
source "$HERE/AtariTools-settings.sh"

# echo "ARGS is $@" ; exit 1

set -e
shopt -s nocasematch

case "$#" in
  1 ) LSTFILE=$1 ;;
  2 ) EXTRA=$1 ; LSTFILE=$2 ;;
  * ) echo "Usage: ${0##*/} [macx|turbo|save] filename" 1>&2 ; exit 1 ;;
esac

# Make the HD folder if it doesn't exist
HDD=${HDD:-"/tmp/HardDrive1"}
mkdir -p "$HDD"

# Opening a LST file with this script will overwrite the old file on H:
# but a safe filename can be configured with SUBLFILE.
LSTBASE=${LSTFILE##*/}
RUNFILE=${HDD}/${SUBLFILE:-$LSTBASE}
RUNFILE=${RUNFILE/.ATARIBAS*/.LST}

if [[ ! $RUNFILE =~ ".LST" ]]; then
  echo "${0##*/} : Not a LST file??"
  sleep 30
  exit 1
fi

# Convert the UTF-8 file to ATASCII
"$HERE/../basic/atascii.py" -s -u "$LSTFILE" >"$RUNFILE"

# SAVE or RUN?
if [[ $EXTRA == "save" ]]; then
  OUTBAS=$LSTBASE           # name part only (no directories)
  OUTBAS=${OUTBAS%.*}.BAS   # replace extension
  ACTION='S."H:$OUTBAS"'
else
  ACTION=RUN
fi

#
# Start Atari800MacX with BASIC, have it load and run the LST file.
# It may be important that Atari800MacX stay in front until export starts.
#
# Prefer to run in Atari800MacX?
if [[ $EXTRA == "macx" ]]; then

  # If possible run directly for much faster load
  if [[ -n $ATARI800MACX ]]; then

    echo "Atari800MacX: Creating a new 'Atari800MacX' instance."
    echo "              Running ${LSTFILE##*/} (as $RUNFILE)"
    echo "              Press F7 to toggle Limit Speed."
    echo "              Press F5 for Warmstart."

    ATARIEXE="${ATARI800MACX}/Contents/MacOS/Atari800MacX"

    #echo "${ATARIEXE} \"$RUNFILE\" \"$FLOPPY1\" &"
    ${ATARIEXE} "$RUNFILE" "$FLOPPY1" &

    sleep 1

    # Command Atari800MacX to reset and import
    /usr/bin/osascript -e 'tell application "Atari800MacX" to activate'

  else

    echo
    echo "Please wait for Atari800MacX import to start..."
    echo "This can take a while. Use F7 to toggle Limit Speed."
    echo

    # Set up the clipboard (loses previous contents, sorry!)
    pbcopy <<COPY
NEW
?CHR\$(125);"Importing...":?"Use F7 to toggle Limit Speed.":E."H:${RUNFILE##*/}"
COPY

    # Print "Import Done" and either save to BAS or run the code
    #KEYWAIT='?"Press any key...":O.#1,4,0,"K":GET#1,K:CL.#1:'
    printf '\233?"Import Complete.":?"F7 to toggle Limit Speed.":%s%s\233' $KEYWAIT $ACTION >>"$RUNFILE"

    # Temporary file for the script
    ASCRIPT=$( mktemp ).applescript

    # AppleScript tells Atari800MacX to run full speed, reset, and import the file
    cat <<ASCRIPT >$ASCRIPT
tell application "Atari800MacX"
  activate
  tell application "System Events"
    tell process "Atari800MacX"
      set MB to menu bar 1
      set MC to menu bar item "Control" of MB
      set MC1 to menu 1 of MC
      set M to menu item "Limit To Normal Speed" of MC1
      set X to value of attribute "AXMenuItemMarkChar" of M
      ignoring application responses
        if not (X = missing value) then
          tell MC to click M
        end if
        tell MC to click menu item "Warm Reset" of MC1
        tell menu bar item "Edit" of MB to click menu item "Paste" of menu 1
      end ignoring
      delay 10
      tell MC to click M
      "ok"
    end tell
  end tell
end tell
ASCRIPT

    # Command Atari800MacX to reset and import
    /usr/bin/osascript "$ASCRIPT"
    rm "$ASCRIPT"

  fi

else

  #
  # atari800 by David Firth and Atari800 Development Team
  # @ http://atari800.atari.org https://atari800.github.io
  # The atari800 package can be installed from MacPorts, Homebrew, etc.
  #
  echo "atari800: Creating a new 'atari800' instance."

  # [ $JOY ] || ATARIOPTS="-nojoystick $ATARIOPTS"
  [[ $EXTRA == "turbo" || $EXTRA == "save" ]] && { ATARIOPTS="-turbo $ATARIOPTS" ; echo "          (Running TURBO SPEED)" ; }

  echo "          Press F12 to toggle Turbo."

  if [[ $EXTRA == "save" ]]; then
    echo "          Tokenizing ${LSTFILE##*/} to H:$OUTBAS"
  else
    echo "          Running ${LSTFILE##*/} (as $RUNFILE)"
  fi

  # Run the LST file from Sublime that was converted to ATASCII

  echo

  # Add "Import Complete" code to save to BAS or to RUN the program
  printf '\233?"Import Complete.":' >>"$RUNFILE"
  if [[ $EXTRA == "save" ]]; then
    printf '?"Saving to H:${RUNFILE}.":%s\233' $ACTION >>"$RUNFILE"
  else
    printf '?"F12 to toggle Turbo.":%s\233' $ACTION >>"$RUNFILE"
  fi

  # If atari800 points at Atari800MacX use a simpler command
  if [[ ${ATARI800##*/} == "Atari800MacX" ]]; then
    #echo "${ATARI800} \"$RUNFILE\" \"$FLOPPY1\" &"
    ${ATARI800} "$RUNFILE" "$FLOPPY1" &
  else
    #echo "${ATARI800} $ATARIOPTS -basic -run \"$RUNFILE\" \"$FLOPPY1\" \"$FLOPPY2\""
    ${ATARI800} $ATARIOPTS -basic -run "$RUNFILE" "$FLOPPY1" "$FLOPPY2"
  fi

fi
