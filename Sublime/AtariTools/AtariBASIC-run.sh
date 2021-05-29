#!/usr/bin/env bash
#
# AtariBASIC-run.sh
#
# Send the file to atari800 by the virtual HD interface.
# The advantage of this emulator is that sound works.
# Disadvantage is loading DOS takes extra steps.
#

case "$#" in
  1 ) LSTFILE=$1 ;;
  * ) echo "Usage: `basename $0` filename" 1>&2 ; exit 1 ;;
esac

set -e

HERE=$(dirname "$0")

# Your installation of atari800
ATARI800=/opt/local/bin/atari800

# A folder to act as virtual H1 drive.
ATARI_HOME=/Applications/Atari800MacX
HDD=$ATARI_HOME/HardDrive1

# Optional floppy drives to mount
FLOPPY1= # $ATARI_HOME/Disks/floppy1.atr
FLOPPY2= # $ATARI_HOME/Disks/floppy2.atr

# The exported code
OUTFILE=SUBLIME.LST

# This may be automated:
ATARI_800_VERSION=420

# Make the HD folder if it doesn't exist
mkdir -p "$HDD"

# Copy the source file to H1: with ATASCII translation
LANG=ASCII tr '\012' '\233' <"$LSTFILE" >"$HDD/$OUTFILE"

# Add a "Done" message and auto-run the code
printf '\233?"Done."\233RUN\233' >>"$HDD/$OUTFILE"

# Start Atari800 with BASIC, have it load and run the LST file
echo "atari800: Creating a new 'atari800' instance."
echo "          Running $HDD/$OUTFILE"
echo "          Press F12 (Throttle) to import faster."

# A basic helper file is needed to run a LST file
$ATARI800 -basic -run "$HDD/$OUTFILE" $FLOPPY1 $FLOPPY2
