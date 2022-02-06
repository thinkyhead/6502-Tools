#!/usr/bin/env bash
#
# AtariBASIC-run.sh
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
  * ) echo "Usage: `basename $0` [macx|run|joy|turbo] filename" 1>&2 ; exit 1 ;;
esac

set -e

# Make the HD folder if it doesn't exist
mkdir -p "$HDD"

# Convert the UTF-8 file to ATASCII
"$HERE/atascii" -u "$LSTFILE" >"$HDD/$SUBLFILE"

# Print "Import Done" and auto-run the code
printf '\233?"Import Complete.":?"F12 to toggle Turbo.":RUN\233' >>"$HDD/$SUBLFILE"

#
# Start Atari800MacX with BASIC, have it load and run the LST file.
# It may be important that Atari800MacX stay in front until export starts.
#
# Prefer to run in Atari800MacX?
if [ "$EXTRA" = "macx" ]; then

  echo
  echo "Please wait for import to start..."
  echo "This can take a while. Leave Atari800MacX in front."
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
  set X to value of attribute "AXMenuItemMarkChar" of M
  tell MC to click menu item "Warm Reset" of menu 1
  tell menu bar item "Edit" of menu bar 1 to click menu item "Paste" of menu 1
  if not (X = missing value) then
    tell MC to click M
  end if
end tell
end tell
end tell
ASCRIPT

  # Set up the clipboard (loses previous contents, sorry!)
  pbcopy <<COPY
NEW
?CHR\$(125);"Importing... Leave this window front.";:E."H:$SUBLFILE"
COPY

  # Command Atari800MacX to reset and import
  /usr/bin/osascript "$ASCRIPT" >/dev/null 2>&1

  rm "$ASCRIPT"

else

  #
  # The advantage of atari800 is that sound works!
  #
  echo "atari800: Creating a new 'atari800' instance."
  echo "          Running $HDD/$SUBLFILE (`basename "$LSTFILE"`)"
  echo "          Press F12 to toggle Turbo."

  # [ $JOY ] || ATARIOPTS="-nojoystick $ATARIOPTS"
  [ "$EXTRA" == "turbo" ] && { ATARIOPTS="-turbo $ATARIOPTS" ; echo "          (Running TURBO SPEED)" ; }

  # A basic helper file is needed to run a LST file
  $ATARI800 $ATARIOPTS -basic -run "$HDD/$SUBLFILE" "$FLOPPY1" "$FLOPPY2"

fi
