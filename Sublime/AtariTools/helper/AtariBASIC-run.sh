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
HERE=$(dirname "$SELF")
source "$HERE/AtariTools.sh"

# echo "ARGS is $@" ; exit 1

case "$#" in
  1 ) LSTFILE=$1 ;;
  2 ) EXTRA=$1 ; LSTFILE=$2 ;;
  * ) echo "Usage: `basename $0` [macx|turbo|save] filename" 1>&2 ; exit 1 ;;
esac

set -e

# Make the HD folder if it doesn't exist
mkdir -p "$HDD"

# Convert the UTF-8 file to ATASCII
"$HERE/atascii" -s -u "$LSTFILE" >"$HDD/$SUBLFILE"

# SAVE or RUN?
if [ "$EXTRA" == "save" ]; then
  OUTBAS="${LSTFILE##*/}"        # name part only (no directories)
  OUTBAS="${OUTBAS%.*}.BAS"      # replace extension
  ACTION="S.\"H:$OUTBAS\""
else
  ACTION=RUN
fi

# Print "Import Done" and auto-run the code
printf '\233?"Import Complete.":?"F12 to toggle Turbo.":%s\233' $ACTION >>"$HDD/$SUBLFILE"

#
# Start Atari800MacX with BASIC, have it load and run the LST file.
# It may be important that Atari800MacX stay in front until export starts.
#
# Prefer to run in Atari800MacX?
if [ "$EXTRA" == "macx" ]; then

  echo
  echo "Please wait for import to start..."
  echo "This can take a while. Leave Atari800MacX in front. Use F7 to toggle Turbo."
  echo

  # Temporary file for the script
  ASCRIPT=$( mktemp ).applescript

  # AppleScript tells Atari800MacX to run full speed, reset, and import the file
  cat <<ASCRIPT >$ASCRIPT
tell application "Atari800MacX"
  activate
  tell application "System Events"
    tell process "Atari800MacX"
      set MC to menu bar item "Control" of menu bar 1
      set M to menu item "Limit To Normal Speed" of menu 1 of MC
      tell MC to click menu item "Warm Reset" of menu 1
      set X to value of attribute "AXMenuItemMarkChar" of M
      if not (X = missing value) then
        tell MC to click M
      end if
      tell menu bar item "Edit" of menu bar 1 to click menu item "Paste" of menu 1
      "ok"
    end tell
  end tell
end tell
ASCRIPT

  # Set up the clipboard (loses previous contents, sorry!)
  pbcopy <<COPY
NEW
?CHR\$(125);"Importing... Leave this window front. Use F7 to toggle Turbo.";:E."H:$SUBLFILE"
COPY

  # Command Atari800MacX to reset and import
  /usr/bin/osascript "$ASCRIPT"
  rm "$ASCRIPT"

else

  #
  # atari800 by David Firth and Atari800 Development Team
  # @ http://atari800.atari.org https://atari800.github.io
  # The atari800 package can be installed from MacPorts, Homebrew, etc.
  #
  echo "atari800: Creating a new 'atari800' instance."
  echo "          Running $HDD/$SUBLFILE (`basename "$LSTFILE"`)"
  echo "          Press F12 to toggle Turbo."

  # [ $JOY ] || ATARIOPTS="-nojoystick $ATARIOPTS"
  [ "$EXTRA" == "turbo" ] && { ATARIOPTS="-turbo $ATARIOPTS" ; echo "          (Running TURBO SPEED)" ; }

  # A basic helper file is needed to run a LST file
  echo "$ATARI800 $ATARIOPTS -basic -run \"$HDD/$SUBLFILE\" \"$FLOPPY1\" \"$FLOPPY2\""
  $ATARI800 $ATARIOPTS -basic -run "$HDD/$SUBLFILE" "$FLOPPY1" "$FLOPPY2" &

fi
