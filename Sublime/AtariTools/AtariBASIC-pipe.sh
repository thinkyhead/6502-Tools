#!/usr/bin/env bash
#
# AtariBASIC-pipe.sh
#
# Send the file to Atari800MacX by the virtual HD interface
# Ensure that Atari800MacX stays in front until export starts.
#

case "$#" in
  1 ) LSTFILE=$1 ;;
  * ) echo "Usage: `basename $0` filename" 1>&2 ; exit 1 ;;
esac

set -e

HDD=/Applications/Atari800MacX/HardDrive1
OUTFILE=SUBLIME.LST

# Copy the source file to H1: with ATASCII translation
LANG=ASCII tr '\012' '\233' <"$LSTFILE" >"$HDD/$OUTFILE"
# Add a "Done" message
printf '\233?"Done."\233RUN\233' >>"$HDD/$OUTFILE"

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
?CHR\$(125);"Importing...";:E."H:$OUTFILE"
COPY

# Command Atari800MacX to reset and import
/usr/bin/osascript "$ASCRIPT" >/dev/null 2>&1

rm "$ASCRIPT"
