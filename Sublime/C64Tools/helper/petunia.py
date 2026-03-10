#!/usr/bin/env python3
"""
  petunia.py - Convert between PETSCII and UTF-8.

  Usage: petunia.py [-p|-u] [-a|-k|-s] filename >outname

  Default: PETSCII to UTF-8 using Private Use Area mapping.

    -p  Convert Unicode input to PETSCII
    -a  Convert PETSCII to ASCII where possible
    -k  Encode output in keybuf format for VICE
    -u  Convert PETSCII keybuf input to Unicode
    -s  Output shifted set for PETSCII-to-Unicode

  Shift State Notes:
  - $D1 = Shift ON  (lowercase/symbols active, $40-$5A maps to a-z)
  - $D2 = Shift OFF (uppercase/numbers active, $40-$5A maps to A-Z)
  - Default state is SHIFT OFF (uppercase mode)

  == Screen Code Font Mappings ==

  This is the actual Unicode mapping of PETSCII on modern computers.
  By convention it uses the "screen mapping" order, which makes sense for an 8-bit font.
  On PET the char  ($5C) appears as the backslash, same as ASCII.

    C64 Full Set, unshifted
  $00  $1F
  $20  $3F
  $40  $5F
  $60  $7F
  $80  $9F
  $A0  $BF
  $C0  $DF
  $E0  $FF

    C64 Full Set, shifted
  $00  $1F
  $20  $3F
  $40  $5F
  $60  $7F
  $80  $9F
  $A0  $BF
  $C0  $DF
  $E0  $FF

  Actual PETSCII encoding is different from the screen order and
  only maps the non-inverted characters $20-$BF. The range $C0-$FF
  simply repeats the character ranges $60-$7F and $A0-$BF.
  I'm not sure of the utility of this mapping for our purposes.
  It is presented here to reflect the orderly mapping of PETSCII to ASCII.

    C64 Full Set, unshifted
  SCR PET                                  SCR PET
  $20 $20  $3F $3F
  $00 $40  $1F $5F
  $40 $60  $5F $7F
  $60 $A0  $7F $BF
  $40 $C0  $5F $DF
  $60 $E0  $7F $FF

    C64 Full Set, shifted
  SCR PET                                  SCR PET
  $20 $20  $3F $3F
  $00 $40  $1F $5F
  $40 $60  $5F $7F
  $60 $A0  $7F $BF
  $40 $C0  $5F $DF
  $60 $E0  $7F $FF


  == Which Set to Use ==

  The -s flag determines which codepoint is used for PETSCII-to-Unicode conversion.

  C64 Character Modes:

   Unshifted:
   - Uppercase ASCII appears as uppercase PETSCII.
   - Lowercase characters are supplanted by 26 special chars.
   - Also includes    

   Shifted:
   - Uppercase ASCII (HELLO) appears as lowercase PETSCII (hello).
   - Lowercase ASCII (hello) appears as uppercase PETSCII (HELLO).
   - Also includes    

  Our mapping for Sublime editing mixes in plain ASCII instead of mapping
  everything to the PUA. So you can still see the code in any font!

  Mixed Unicode + ASCII mapping of PETSCII screen codes.
  C64 Full Set, unshifted:

  $00 @ABCDEFGHIJKLMNOPQRSTUVWXYZ[]
  $20  !"#$%&'()*+,-./0123456789:;<=>?
  $40 
  $60  
  $80 
  $A0 
  $C0 
  $E0 

  Mixed Unicode + ASCII mapping of PETSCII screen codes.
  C64 Full Set, shifted:

  $00 @abcdefghijklmnopqrstuvwxyz[]
  $20  !"#$%&'()*+,-./0123456789:;<=>?
  $40 ABCDEFGHIJKLMNOPQRSTUVWXYZ
  $60  
  $80 
  $A0 
  $C0 
  $E0 
"""

import sys, getopt, re

DEBUG = 0

# PETSCII newline conventions
CR = 0x0D
LF = 0x0A

# Unicode Private Use mapping bases
REG_NORMAL = 0xE000     # First half of the "Unshifted" set
REG_INVERT = 0xE080     # Unshifted characters again but inverted
SHF_NORMAL = 0xE100     # First half of the "Shifted" set
SHF_INVERT = 0xE180     # Shifted characters again but inverted

ASCII, UTF_1, UTF_2, CHR_READY = 0, 1, 2, 3

#
# Unicode to PETSCII
#

def screen_to_petscii(c, upr=False):
    """
    Convert a Unicode PETSCII screen mapped char to its PETSCII keymap char.
    The input char index is always derived from screen mapped Unicode.
    The output is the PETSCII keymap char in binary form.
    """
    # Inverted chars remap to normal
    c = c & 0x7F
    #print('{$' + format(c, '02x') + '}', end='', file=sys.stderr)
    if 0x00 <= c <= 0x1F:   # Lowercase letters in PETSCII (unshifted) map to ASCII uppercase area
        c += 0x40
    elif 0x40 <= c <= 0x5F: # Special chars $40-5F -> $60-7F
        c += 0x20
    elif 0x60 <= c <= 0x7F: # Special chars $60-7F -> $A0-BF
        c += 0x40

    # In PETSCII unshifted lowercase appears as upper
    if upr:
        if 0x41 <= c <= 0x5A:       # PETSCII unshifted A-Z = shifted a-z
            c += 0x20
        elif 0x61 <= c <= 0x7A:     # PETSCII unshifted a-z = shifted A-Z
            c -= 0x20

    #print('->{$' + format(c, '02x') + '}', file=sys.stderr)

    return c


# A class with an iterator to go through the specified bytes
# and returning the PETSCII equivalent for each one
class PUA_PETSCII_Parser:
    def __init__(self, mixdata: bytes, caseCheck=False):
        self.data = mixdata
        self.pos = 0
        self.caseCheck = caseCheck
        self.inquote, self.inline, self.incomment = False, False, False

    def __iter__(self):
        return self

    def __next__(self):
        state = ASCII # State machine for Unicode bytes
        while True:   # Get bytes until a char is decoded
            if self.pos >= len(self.data):
                raise StopIteration

            b = self.data[self.pos]
            self.pos += 1

            try:
                if state == ASCII:
                    if b == 0xEE:  # start of E0xx PUA UTF-8
                        state = UTF_1
                    else:
                        # ASCII characters 0x20-0x7F map directly to PETSCII
                        # However, the cases are swapped and are not in screen order.
                        c = b
                        state = CHR_READY

                elif state == UTF_1:
                    if (b & 0xC0) == 0x80:
                        c1 = b
                        state = UTF_2
                    else:
                        # No two-byte characters are expected
                        c = ord("?")
                        state = CHR_READY

                elif state == UTF_2:
                    c = ((c1 & 3) << 6) | (b & 0x3F)
                    # In the shifted set lowercase stays the same
                    c = screen_to_petscii(c, c1 & 0x04 != 0x00)
                    state = CHR_READY

                if state == CHR_READY:

                    if not self.inline:
                        if c == 0x3B or c == 0x23:
                            self.incomment = True
                        elif c != 0x20 and c != 0x09:
                            self.inline = True

                    if self.caseCheck:
                        if c == 0x22:
                           self.inquote = not self.inquote
                        if not self.inquote:
                           c = ord(chr(c).lower())

                    if self.incomment:
                        state = ASCII

                    if c in (0x0A, 0x0D):
                       self.inquote, self.inline, self.incomment = False, False, False

                if state == CHR_READY:
                    return c

            except UnicodeDecodeError as e:
                raise StopIteration from e

def mixed_bytes_to_petscii(mixdata: bytes, caseCheck=False):
    """
    Convert ASCII mixed with Unicode (PUA PETSCII in screen order)
    to simplified PETSCII bytes based on visual appearance.
    If special chars from the unshifted set are mixed with uppercase,
    we could lowercase all letters. For now just extract PETSCII
    verbatim. When viewed in ASCII the cases will be reversed.
    """
    out = bytearray(c for c in PUA_PETSCII_Parser(mixdata, caseCheck))
    return out


def petscii_bytes_to_petcat(petdata:bytes):
    """
    Convert PETSCII bytes to literal or encoded keymap PETSCII
    that will reproduce the intent.
    """

    def petscii_to_petcat(c, allCaps=False):
        """
        Encode a single character.

        $40-$5F    @a…z[\\]^_
        $20-$3F     !"#$%&'()*+,-./0123456789:;<=>?
        $60-$7F    {$60}ABCDEFGHIJKLMNOPQRSTUVWXYZ{$7b}…{$7f}
        $A0-$BF    {$a0}…{$bf}

        """
        #if 0x41 <= c <= 0x5A: return chr(c + 0x20) # $41-$5A = lower
        #if 0x61 <= c <= 0x7A: return chr(c - 0x20) # $61-$7A = UPPER
        if 0x00 <= c <= 0x40: return chr(c) # $00-$3F = literal
        if 0x41 <= c <= 0x5F: return chr(c) # $41-$5A = lower
        if 0x61 <= c <= 0x7A:
            if allCaps:
                return chr(c-0x20)          # $61-$7A = lower
            else:
                return chr(c)               # $61-$7A = UPPER
        return '{$' + format(c, 'x') + '}'  #  OTHER  = "{$hh}"

    out = bytearray()
    i = 0
    while i < len(petdata):
        c = petdata[i]
        i += 1
        cdat = petscii_to_petcat(c)
        for d in cdat: out.append(ord(d))

    return out


def mixed_file_to_petscii(filename, kflag=False):
    """
    - Read a file containing mixed Unicode-mapped PETSCII characters
      and ASCII.
    - Reduce the characters down to PETSCII bytes
    - With -k further convert to keyboard inputs
    """
    with open(filename, 'rb') as f:
        mixdata = f.read()

        # Convert mixed Unicode+ASCII to PETSCII
        out = mixed_bytes_to_petscii(mixdata, True)

        # Convert to keymapped characters
        if kflag: out = petscii_bytes_to_petcat(out)

        sys.stdout.buffer.write(out)


#
# PETSCII to Unicode
#

def petscii_to_screen(c):
    """
    Convert a PETSCII keymap char to its screen mapped index.
      $20 -> $20  Symbols
      $40 -> $00  Lowercase
      $60 -> $40  Special 1
      $A0 -> $60  Special 2
      $C0 -> $40  Special 1
      $E0 -> $60  Special 2
    """
    if 0x40 <= c <= 0x5F:   # Lowercase letters $40-5F -> $00-1F
        c -= 0x40
    elif 0x60 <= c <= 0x7F: # Special chars $60-7F -> $40-5F
        c -= 0x20
    elif 0xA0 <= c <= 0xBF: # Special chars $A0-BF -> $60-7F
        c -= 0x40
    elif 0xC0 <= c <= 0xDF: # Special chars $C0-DF -> $40-5F
        c -= 0xA0
    elif 0xE0 <= c <= 0xFF: # Special chars $E0-FF -> $60-7F
        c -= 0x80

    return c

def vice_str_to_unicode(vicestr: str, aflag=False, sflag=False):
    """
    Convert ASCII encoded PETSCII key codes to Unicode bytes.
    Unicode conversion for use in Sublime.
    Printable tokens in PETSCII are $20-$7F and $A0-$BF.

      Unshifted 0xE000 + …
      Shifted   0xE100 + …

      Unicode     PETSCII VICE
      $E000-$E01F $40-$5F @a…z[\\]^_
      $E020-$E03F $20-$3F  !"#$%&'()*+,-./0123456789:;<=>?
      $E040-$E05F $60-$7F {$60}ABCDEFGHIJKLMNOPQRSTUVWXYZ{$7b}…{$7f}
      $E060-$E07F $A0-$BF {$a0}…{$bf}

      {$a0}{CBM-K}{CBM-I}{CBM-T}{CBM-@}{CBM-G}{CBM-+}{CBM-M}{CBM-POUND}{SHIFT-POUND}{CBM-N}{CBM-Q}{CBM-D}{CBM-Z}{CBM-S}{CBM-P}{CBM-A}{CBM-E}{CBM-R}{CBM-W}{CBM-H}{CBM-J}{CBM-L}{CBM-Y}{CBM-U}{CBM-O}{SHIFT-@}{CBM-F}{CBM-C}{CBM-X}{CBM-V}{CBM-B}

    Handles shift state changes via $D1/$D2 control codes.
     - aflag : Map to plain ASCII where possible.
     - sflag : Use shifted mapping for Unicode output.
    """

    # Map aliases to PETSCII bytes ($A1-$BF)
    alias_map = {
      '{CBM-K}': 0xA1, '{CBM-I}': 0xA2, '{CBM-T}': 0xA3, '{CBM-@}': 0xA4,
      '{CBM-G}': 0xA5, '{CBM-+}': 0xA6, '{CBM-M}': 0xA7,
      '{CBM-POUND}': 0xA8, '{SHIFT-POUND}': 0xA9,
      '{CBM-N}': 0xAA, '{CBM-Q}': 0xAB, '{CBM-D}': 0xAC, '{CBM-Z}': 0xAD,
      '{CBM-S}': 0xAE, '{CBM-P}': 0xAF, '{CBM-A}': 0xB0, '{CBM-E}': 0xB1,
      '{CBM-R}': 0xB2, '{CBM-W}': 0xB3, '{CBM-H}': 0xB4, '{CBM-J}': 0xB5,
      '{CBM-L}': 0xB6, '{CBM-Y}': 0xB7, '{CBM-U}': 0xB8, '{CBM-O}': 0xB9,
      '{SHIFT-@}': 0xBA,
      '{CBM-F}': 0xBB, '{CBM-C}': 0xBC, '{CBM-X}': 0xBD, '{CBM-V}': 0xBE,
      '{CBM-B}': 0xBF
    }

    # Replace aliases with $HEX notation
    for alias, petcode in alias_map.items():
        vicestr = vicestr.replace(alias, f'${{petcode:x}}')

    # Process {$HEX} patterns to replace with actual bytes
    def convert_hex_match(match):
        hex_val = match.group(1)
        byte_val = int(hex_val, 16)
        return chr(byte_val)

    vicestr = re.sub(r'\{\$([0-9a-fA-F]+)\}', convert_hex_match, vicestr)

    # Process {DEC} patterns to replace with actual bytes
    def convert_dec_match(match):
        dec_val = int(match.group(1))
        return chr(dec_val)

    vicestr = re.sub(r'\{(\d+)\}', convert_dec_match, vicestr)

    # Convert PETSCII bytes to UTF-8 PUA bytes

    def utf_trio_bytes(base, c):
        """Encode PETSCII byte to UTF-8 PUA."""
        codepoint = base + c # e.g., 0xE0A9
        c0 = 0xE0 | (codepoint >> 12)
        c1 = 0x80 | ((codepoint >> 6) & 0x3F)
        c2 = 0x80 | (codepoint & 0x3F)

        #print("[" + format(c0, '02X') + ":" + format(c1, '02X') + ":" + format(c2, '02X') + "]", file=sys.stderr)

        return bytes([c0, c1, c2])

    out = bytearray()
    base = SHF_NORMAL if sflag else REG_NORMAL

    for c in vicestr:
        b = ord(c)

        # Lower chars are trusted
        if 0x00 <= b <= 0x20:
            out.append(b)
            continue

        # In PETSCII unshifted lowercase appears as upper
        if not sflag:
            if 0x41 <= b <= 0x5A:       # PETSCII unshifted A-Z = shifted a-z
                b += 0x20
            elif 0x61 <= b <= 0x7A:     # PETSCII unshifted a-z = shifted A-Z
                b -= 0x20

        # Literal ASCII where possible
        if aflag and (0x20 <= b <= 0x5F or 0x61 <= b <= 0x7A):
            out.append(b)
            continue

        # Map PETSCII to Unicode PUA codepoints
        #if c & 0x80:
        #    base = SHF_INVERT if sflag else REG_INVERT
        #else:
        #    base = SHF_NORMAL if sflag else REG_NORMAL

        # Encode as UTF-8 trio bytes with screen mapping
        c = petscii_to_screen(b)
        u = utf_trio_bytes(base, c)
        out.extend(u)

    return out.decode("utf-8", errors="replace")


def vice_file_to_unicode(filename, aflag=False, sflag=False):
    """
    Convert an ASCII file to PETSCII binary
    - Convert {123} and {$7b} to literal PETSCII
    - Map lowercase letters to $41-$5A
    - Map uppercase letters to $61-$7A
    - Keep other chars as ASCII
    - If there is no uppercase, all letters could be uppercased
      with mapping to Unicode.
    """
    with open(filename, 'r', encoding='utf-8') as f:
        vicestr = f.read()
        unistr = vice_str_to_unicode(vicestr, aflag, sflag)
        print(unistr)


def usage():
    print("Usage: petscii [-a|-k|-p|-s|-u] filename\n")
    print("Options:")
    print("  -a  Convert PETSCII to ASCII where possible")
    print("  -k  Encode output in keybuf format for VICE")
    print("  -p  Convert Unicode input to PETSCII")
    print("  -u  Convert PETSCII keybuf input to Unicode")
    print("  -s  Output shifted set for PETSCII-to-Unicode")

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "akpsu")
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(2)

    aflag = False   # Output in plain modern ASCII
    kflag = False   # Output compatible with -keybuf
    pflag = False   # Convert Unicode input to PETSCII output
    uflag = False   # Convert PETSCII input to Unicode output
    sflag = False   # For Unicode output use the Shifted set

    for opt, arg in opts:
        if   opt == '-a': aflag = True
        elif opt == '-k': kflag = True
        elif opt == '-p': pflag = True
        elif opt == '-u': uflag = True
        elif opt == '-s': sflag = True

    # Assume Unicode input to PETSCII output if nothing else was specified
    if not uflag: pflag = True

    if len(args) < 1 or (pflag and uflag) or (pflag and sflag):
        usage()
        sys.exit(2)

    filename = args[0]

    if pflag:
        mixed_file_to_petscii(filename, kflag)
    else:
        vice_file_to_unicode(filename, aflag, sflag)


if __name__ == "__main__":
    main()
