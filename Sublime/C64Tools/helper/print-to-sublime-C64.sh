#!/usr/bin/env bash
#
# print-to-sublime-C64.sh
#   - Read from a file argument or from STDIN
#   - Save the raw data to $BINF (binary copy)
#   - Convert that data with petunia.py -> UTF‑8 file
#   - Open the resulting UTF‑8 file in Sublime Text
#

set -euo pipefail

# Locate this script (so we can source the env and call petunia.py)
SELF="$0"
[[ -L "$SELF" ]] && SELF=$(readlink "$SELF")   # follow symlink if needed
HERE=${SELF%/*}

# Load user tools environment (DRIVE9, PRINTFILE etc.)
source "$HERE/C64Tools-settings.sh"

# Destination filenames
UTF="$DRIVE9/$PRINTFILE"
BINF="$DRIVE9/$PRINTBIN"

# Ensure the destination directory exists
mkdir -p "${UTF%/*}"

# Grab input – either from a file argument or STDIN
if [[ $# -eq 0 ]]; then
  # No argument -> read from pipe / stdin
  INPUT_DATA=$(cat)                 # capture all piped input
else
  # Argument supplied -> read from that file
  INPUT_DATA=$(<"$1")
fi

# Write raw binary copy (needed for the “print‑bin” file)
echo "Writing $BINF"
printf "%s" "$INPUT_DATA" >"$BINF"

# Convert to UTF‑8 using petunia.py and write out
echo "Converting to $UTF"
"$HERE/petunia.py" "$BINF" >"$UTF"

# Open the UTF‑8 file in Sublime Text
subl "$UTF"
