#!/usr/bin/env bash
#
# atari2subl.sh - A utility to pipe printed output from atari800 to Sublime Text, converting
#                 special characters to UTF-8 for use with the Atari Unicode font.
#

# Determine this script's real location
SELF="$0"
[[ $(readlink "$SELF") == "" ]] || SELF=$( readlink "$SELF" )
HERE=$(dirname "$SELF")

# Get user tools environment
source "$HERE/AtariTools.sh"

# Printed output goes to...
UTF="$HDD/$PRINTFILE"
BINF="$HDD/$PRINTBIN"

# Make the HD folder if it doesn't exist
mkdir -p $(dirname "$UTF")

# Convert the "printed" output to UTF-8
"$HERE/atascii.py" -p "$1" >"$UTF"

# Save raw binary output
cp "$1" "$BINF"

# Open the UTF-8 file in Sublime Text
subl "$UTF"
