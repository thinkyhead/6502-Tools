#!/usr/bin/env python3

"""
atascii.py - Convert to ATASCII or UTF-8, send to stdout.

Usage: atascii [-a|-i|-n|-u] [-p] [-s] filename >outname
    By default, ATASCII to UTF-8 E000-E0FF, E100-E1FF
 -a ATASCII to plain ASCII
 -i International set UTF-8 mapping
 -n ATASCII to naive UTF-8
 -p Printed with 'LIST "P:"'
 -s (with -u) Strip host comments
 -u UTF-8 to ATASCII

Atari Font Mapping:

  $E000 for the normal set;
  $E080 for the inverse set;
  $E100 for the international set; and
  $E180 for the inverse international set."

  3-byte UTF-8 : 1110 aaaa  10bb bbc.  10.. ....
        Normal :      1110  1000 000.  10.. ....   E00
        Invert :      1110  1000 001.  10.. ....   E08
  Int'l Normal :      1110  1000 010.  10.. ....   E10
  Int'l Invert :      1110  1000 011.  10.. ....   E18

Called by atari2subl.sh when used as a pipe -- e.g., for Atari800MacX.

Key changes made during conversion:
 1. Replaced C++ constants with Python variables
 2. Converted the character mapping arrays to a list of lists in Python
 3. Changed file operations from C-style to Python's `with open()` context manager
 4. Replaced `fgetc()` with reading bytes directly from the file object
 5. Used `sys.stdout.buffer.write()` for binary output instead of `putchar()`
 6. Simplified the state machine logic using Python's while loops and if/else statements
 7. Removed the need for explicit EOF handling by checking for empty reads
 8. Used Python's exception handling for file operations

The functionality remains the same as the original C++ code, just implemented in Python idioms.
"""

import sys
import getopt

DEBUG = 0

# Constants
LF = 0x0A    # ASCII LF
CR = 0x0D    # ASCII CR
ATESC = 0x1B # ESCAPE
ATURW = 0x1C # UP-ARROW
ATDRW = 0x1D # DOWN-ARROW
ATLRW = 0x1E # LEFT-ARROW
ATRRW = 0x1F # RIGHT-ARROW
ATCLR = 0x7D # CLEAR SCREEN CHARACTER
ATRUB = 0x7E # BACK SPACE (RUBOUT)
ATTAB = 0x7F # TAB
ATEOL = 0x9B # END-OF-LINE
ATDELL = 0x9C # delete line
ATINSL = 0x9D # insert line
ATCTAB = 0x9E # clear TAB
ATSTAB = 0x9F # set TAB
ATBEL = 0xFD  # CONSOLE BELL
ATDEL = 0xFE  # delete char.
ATINS = 0xFF  # insert char.

REG_NORMAL = 0xE000
REG_INVERT = 0xE080
INT_NORMAL = 0xE100
INT_INVERT = 0xE180

ASCII = 0
UTF_1 = 1
UTF_2 = 2
UTF_3 = 3

# Character mappings (same as in C++ code)
naive = [
    [0xE2, 0x99, 0xA5],        [0xE2, 0x94, 0x9C],        [0x20],
    [0xE2, 0x94, 0x98],        [0xE2, 0x94, 0xA4],        [0xE2, 0x94, 0x90],
    [0xE2, 0x95, 0xB1],        [0xE2, 0x95, 0xB2],        [0xE2, 0x97, 0xA2],
    [0xE2, 0x96, 0x97],        [0xE2, 0x97, 0xA3],        [0xE2, 0x96, 0x9D],
    [0xE2, 0x96, 0x98],        [0xE2, 0x96, 0x86],        [0xE2, 0x96, 0x82],
    [0xE2, 0x96, 0x96],        [0xE2, 0x99, 0xA3],        [0xE2, 0x94, 0x8C],
    [0xE2, 0x94, 0x80],        [0xE2, 0x94, 0xBC],        [0xE2, 0x97, 0x8F],
    [0xE2, 0x96, 0x84],        [0xE2, 0x96, 0x8E],        [0xE2, 0x94, 0xAC],
    [0xE2, 0x94, 0xB4],        [0xE2, 0x96, 0x8C],        [0xE2, 0x94, 0x94],
    [0xE2, 0x90, 0x9B],        [0xE2, 0x86, 0x91],        [0xE2, 0x86, 0x93],
    [0xE2, 0x86, 0x90],        [0xE2, 0x86, 0x92],        None,
    None,                      None,                      None,
    None,                      None,                      None,
    None,                      None,                      None,
    None,                      None,                      None,
    None,                      None,                      None,
    [0xE2, 0x99, 0xA6],        None,                      None,
    None,                      None,                      None,
    None,                      None,                      None,
    None,                      None,                      None,
    None,                      None,                      None,
    [0xE2, 0x99, 0xA5],        [0xE2, 0x94, 0xA3],        [0xE2, 0x96, 0x8A],
    [0xE2, 0x94, 0x9B],        [0xE2, 0x94, 0xAB],        [0xE2, 0x94, 0x93],
    [0xE2, 0x95, 0xB1],        [0xE2, 0x95, 0xB2],        [0xE2, 0x97, 0xA4],
    [0xE2, 0x96, 0x9B],        [0xE2, 0x97, 0xA5],        [0xE2, 0x96, 0x99],
    [0xE2, 0x96, 0x9F],        [0xE2, 0x96, 0x86],        [0x3F],
    [0xE2, 0x96, 0x9C],        [0xE2, 0x99, 0xA3],        [0xE2, 0x94, 0x8F],
    [0xE2, 0x94, 0x81],        [0xE2, 0x95, 0x8B],        [0xE2, 0x97, 0x8F],
    [0xE2, 0x96, 0x80],        [0x3F],                    [0xE2, 0x94, 0xB3],
    [0xE2, 0x94, 0xBB],        [0xE2, 0x96, 0x90],        [0xE2, 0x94, 0x97],
    [0xE2, 0x90, 0x9B],        [0xE2, 0x87, 0x91],        [0xE2, 0x87, 0x93],
    [0xE2, 0x87, 0x90],        [0xE2, 0x87, 0x92],        [0xE2, 0x96, 0x91],
    [0x21],                    [0x22],                    [0x23],
    [0x24],                    [0x25],                    [0x26],
    [0x27],                    [0x28],                    [0x29],
    [0x2A],                    [0x2B],                    [0x2C],
    [0x2D],                    [0x2E],                    [0x2F],
    [0xF0, 0x9D, 0x9F, 0xAC],  [0xF0, 0x9D, 0x9F, 0xAD],  [0xF0, 0x9D, 0x9F, 0xAE],
    [0xF0, 0x9D, 0x9F, 0xAF],  [0xF0, 0x9D, 0x9F, 0xB0],  [0xF0, 0x9D, 0x9F, 0xB1],
    [0xF0, 0x9D, 0x9F, 0xB2],  [0xF0, 0x9D, 0x9F, 0xB3],  [0xF0, 0x9D, 0x9F, 0xB4],
    [0xF0, 0x9D, 0x9F, 0xB5],  [0x3A],                    [0x3B],
    [0x3C],                    [0x3D],                    [0x3E],
    [0x3F],                    [0x40],                    [0xF0, 0x9D, 0x97, 0x94],
    [0xF0, 0x9D, 0x97, 0x95],  [0xF0, 0x9D, 0x97, 0x96],  [0xF0, 0x9D, 0x97, 0x97],
    [0xF0, 0x9D, 0x97, 0x98],  [0xF0, 0x9D, 0x97, 0x99],  [0xF0, 0x9D, 0x97, 0x9A],
    [0xF0, 0x9D, 0x97, 0x9B],  [0xF0, 0x9D, 0x97, 0x9C],  [0xF0, 0x9D, 0x97, 0x9D],
    [0xF0, 0x9D, 0x97, 0x9E],  [0xF0, 0x9D, 0x97, 0x9F],  [0xF0, 0x9D, 0x97, 0xA0],
    [0xF0, 0x9D, 0x97, 0xA1],  [0xF0, 0x9D, 0x97, 0xA2],  [0xF0, 0x9D, 0x97, 0xA3],
    [0xF0, 0x9D, 0x97, 0xA4],  [0xF0, 0x9D, 0x97, 0xA5],  [0xF0, 0x9D, 0x97, 0xA6],
    [0xF0, 0x9D, 0x97, 0xA7],  [0xF0, 0x9D, 0x97, 0xA8],  [0xF0, 0x9D, 0x97, 0xA9],
    [0xF0, 0x9D, 0x97, 0xAA],  [0xF0, 0x9D, 0x97, 0xAB],  [0xF0, 0x9D, 0x97, 0xAC],
    [0x5B],                    [0x5C],                    [0x5D],
    [0x5E],                    [0x5F],                    [0xE2, 0x99, 0xA6],
    [0xF0, 0x9D, 0x97, 0xAE],  [0xF0, 0x9D, 0x97, 0xAF],  [0xF0, 0x9D, 0x97, 0xB0],
    [0xF0, 0x9D, 0x97, 0xB1],  [0xF0, 0x9D, 0x97, 0xB2],  [0xF0, 0x9D, 0x97, 0xB3],
    [0xF0, 0x9D, 0x97, 0xB4],  [0xF0, 0x9D, 0x97, 0xB5],  [0xF0, 0x9D, 0x97, 0xB6],
    [0xF0, 0x9D, 0x97, 0xB7],  [0xF0, 0x9D, 0x97, 0xB8],  [0xF0, 0x9D, 0x97, 0xB9],
    [0xF0, 0x9D, 0x97, 0xBA],  [0xF0, 0x9D, 0x97, 0xBB],  [0xF0, 0x9D, 0x97, 0xBC],
    [0xF0, 0x9D, 0x97, 0xBD],  [0xF0, 0x9D, 0x97, 0xBE],  [0xF0, 0x9D, 0x97, 0xBF],
    [0xF0, 0x9D, 0x98, 0x80],  [0xF0, 0x9D, 0x98, 0x81],  [0xF0, 0x9D, 0x98, 0x82],
    [0xF0, 0x9D, 0x98, 0x83],  [0xF0, 0x9D, 0x98, 0x84],  [0xF0, 0x9D, 0x98, 0x85],
    [0xF0, 0x9D, 0x98, 0x86],  [0xF0, 0x9D, 0x98, 0x87],  [0xE2, 0x99, 0xA0],
    [0x7C],                    [0xE2, 0x87, 0xB1],        [0xE2, 0x97, 0x80],
    [0xE2, 0x96, 0xB6]
]

def usage():
    print("Usage: atascii [-a|-i|-n|-u] [-p] [-s] filename >outname")
    print(" -a ATASCII to plain ASCII")
    print(" -i International set UTF-8 output")
    print(" -n ATASCII to naive UTF-8")
    print(" -p Printed with 'LIST \"P:\"'")
    print(" -s Strip host comments")
    print(" -u UTF-8 to ATASCII")

def put_utf_trio(c, intl=False):
    if c & 0x80:
        base = INT_INVERT if intl else REG_INVERT
    else:
        base = INT_NORMAL if intl else REG_NORMAL

    chr_val = base + (c & 0x7F)

    c0 = 0xE0 | (chr_val >> 12)
    c1 = 0x80 | ((chr_val >> 6) & 0x3F)
    c2 = 0x80 | (chr_val & 0x3F)

    sys.stdout.buffer.write(bytes([c0, c1, c2]))
    return None

def putchar_a(c):
    if c == LF:
        sys.stdout.buffer.write(bytes([ATEOL]))
    else:
        sys.stdout.buffer.write(bytes([c]))

def putchar_u(c):
    if c == ATEOL:
        sys.stdout.buffer.write(bytes([LF]))
    else:
        sys.stdout.buffer.write(bytes([c]))

# Convert from UTF-8 to ATASCII, writing to stdout
def unicode_to_atascii(filename, sflag):
    with open(filename, 'rb') as fp:
        state = ASCII
        scheck = sflag
        gotchar = not sflag
        skip = False
        while True:
            ch = fp.read(1)
            if not ch: break
            c = ord(ch)

            if DEBUG > 1 and c == CR: print("<CR>", end='')

            # For 's' check the beginning of the line for a comment
            if scheck:
                if DEBUG > 1: print(f"<{c}>", end='')
                if ch in { b' ', b'\t' }: continue
                scheck = False
                skip = ch in { b';', b'#' }  # A comment to skip?
                if DEBUG and skip: print("<SKIP>", end='')

            # Reset states at the end of a line
            if c == LF:
                scheck = sflag                      # Check for a host comment with 'S' flag
                if skip:                            # When skipping a line skip this LF too
                    skip = False
                    if DEBUG: print("</SKIP>")
                    continue
                if DEBUG > 1: print("<LF>", end='')

            # Skipping this input character
            if skip:
                if DEBUG > 1: print("|", end='')
                continue

            # With 'S' the first output character must be non-whitespace
            if not gotchar:
                if ch == b' ' or ch == b'\t' or c == LF or c == CR: continue
                gotchar = True

            if DEBUG: print(f"{chr(c)}", end='')

            if state == ASCII:
                if c == 0xEE:
                    state = UTF_1
                else:
                    putchar_a(c)
            elif state == UTF_1:
                if (c & 0xC0) == 0x80:
                    c1 = c
                    state = UTF_2
                else:
                    sys.stdout.buffer.write(bytes([0xEE]))
                    putchar_a(c)
                    state = ASCII
            elif state == UTF_2:
                c = (c1 & 3) << 6 | (c & 0x3F)
                sys.stdout.buffer.write(bytes([c]))
                state = ASCII

# Convert from ATASCII to UTF-8, writing to stdout
def atascii_to_unicode(filename, pflag, iflag, aflag, nflag):
    with open(filename, 'rb') as fp:
        got_lf = False
        while True:
            c = fp.read(1)
            if not c: break
            c = ord(c)

            if pflag:
                # The LIST "P:" command in atari800 replaces 0x9B with 0x0A (LF)
                # which makes it (nearly) impossible to tell whether LF was
                # actually in the original file. As a workaround for most cases,
                # only keep the LF if the next character is a numeric digit.
                #
                # A more robust heuristic would look for one or more digits
                # followed by a space.
                if got_lf:
                    if c >= ord('0') and c <= ord('9'):
                        sys.stdout.buffer.write(bytes([LF]))  # LF followed by a digit? Trust the LF.
                    else:
                        put_utf_trio(0x0A, iflag)           # LF in the middle of a line, substitute
                # Wait till the next character to decide what to do with LF
                if c == LF:
                    got_lf = True
                    continue

                got_lf = False

            if aflag:
                # Do a clean ASCII-only conversion
                if c < 0x20:
                    sys.stdout.buffer.write(bytes([c + ord('a') - 1]))
                elif c >= 0xC0:
                    sys.stdout.buffer.write(bytes([c - 0xC0 + ord('A') - 1]))
                else:
                    putchar_u(c)
            elif nflag:
                # Convert to common UTF-8 for Unicode fonts
                if c == ATEOL:
                    sys.stdout.buffer.write(bytes([LF]))
                else:
                    utf_bytes = naive[c]
                    if utf_bytes is None:
                        sys.stdout.buffer.write(bytes([c]))
                    elif utf_bytes[0] < 0x80:
                        sys.stdout.buffer.write(bytes([utf_bytes[0]]))
                    else:
                        for b in utf_bytes:
                            sys.stdout.buffer.write(bytes([b]))
            else:
                # Convert to user-space UTF-8 for Atari Font
                if c == ATEOL:
                    sys.stdout.buffer.write(bytes([LF]))
                else:
                    b = c & 0x7F
                    if b <= 0x1F or b == 0x60 or c >= 0x7B:
                        put_utf_trio(c, iflag)
                    else:
                        putchar_u(c)

        # If the last character in the file was LF...
        if got_lf:
            sys.stdout.buffer.write(bytes([LF]))

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "ainpsu")
    except getopt.GetoptError as err:
        print(str(err))
        usage()
        sys.exit(2)

    aflag = False
    iflag = False
    nflag = False
    pflag = False
    sflag = False
    uflag = False
    for opt, arg in opts:
        if opt == '-a': aflag = True
        elif opt == '-i': iflag = True
        elif opt == '-n': nflag = True
        elif opt == '-p': pflag = True
        elif opt == '-s': sflag = True
        elif opt == '-u': uflag = True

    # Check for incompatible options
    if sum([aflag, iflag, nflag, uflag]) > 1:
        print("Only one of -a -i -n -u may be used at a time.")
        sys.exit(2)
    if pflag and uflag:
        print("-p ignored with -u")
    if sflag and not uflag:
        print("-s ignored without -u")

    # A filename parameter is required
    if len(args) < 1:
        usage()
        sys.exit(2)

    filename = args[0]

    try:
        if uflag:
            unicode_to_atascii(filename, sflag)
        else:
            atascii_to_unicode(filename, pflag, iflag, aflag, nflag)

    except IOError as e:
        print(f"File error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
