#
# AtariTools.sh - Atari Tools user environment
#

# Emulation apps folder, if any
EmuDir=/Applications

#
# Atari 800
#

# Installed 'atari800' binary
ATARI800=/usr/local/bin/atari800
#ATARI800=/opt/local/bin/atari800

# Runtime Atari 800 preferences
ATARIOPTS="-win-width 1200 -win-height 900 -colors-preset deep-black"

# Common Atari 800 files location
ATARI_HOME=$EmuDir/Atari800

# A folder to act as virtual H1 drive
HDD=$ATARI_HOME/HardDrive1

# Optional floppy drives to mount
#FLOPPY1="$ATARI_HOME/Disks/floppy1.atr"
#FLOPPY2="$ATARI_HOME/Disks/floppy2.atr"

# Build output from Sublime:
SUBLFILE="SUBLIME3.LST"

# Printed output from atari800:
PRINTFILE="ATARIOUT.LST"

#
# Atari 2600
#

# VCS includes (from dasm package)
VCS_DIR=/usr/local/include/vcs

# Atari 2600 Emulator app. Choose only one.
# z26="$EmuDir/z26.app"
Stella="$EmuDir/Stella.app"

#
# Get the shell environment
#
[ -f ~/.profile ] && source ~/.profile
