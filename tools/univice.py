#!/usr/bin/env python3
"""
Misc functions to test PETSCII Unicode conversion
"""

# PETSCII Shift Control Codes
SHIFT_ON = 0xD1   # Switch to lowercase/symbols ($40-$5A maps to a-z)
SHIFT_OFF = 0xD2  # Switch to uppercase/numbers ($40-$5A maps to A-Z)
INVERT = 0xDF     # Invert (display attribute, not character mapping)

def uninvert_str(utext):
    """
    Move to the non-inverted code point, but still PUA.
    Both Unshifted and Shifted sets are handled.
    TODO: Replace with logic and maths for efficiency.
    """
    invt = ""
    norm = ""
    norm_map = {ord(a): ord(b) for a, b in zip(invt, norm)}
    return utext.translate(norm_map)

def reduce_ascii_str(utext):
    """
    Move PUA to ASCII where possible. Still suitable for the editor.
    This gives the same visual representation, so uppercase map to
    special characters except in the narrow case of all-uppercase code.
    Both Unshifted and Shifted sets are handled.
    TODO: Replace with logic and maths for efficiency.
    """
    unic = ""
    redu = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_ !\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_ !\"#$%&'()*+,-./0123456789:;<=>?ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    redu_map = {ord(a): ord(b) for a, b in zip(unic, redu)}
    return utext.translate(redu_map)

def petscii_str(utext):
    """
    Taking Unicode input
    Replace all chars with uppercase letters or PETSCII codes.

      Unicode -> PETSCII -> Encoded
      $00-$1F    $40-$5F    @a…z[\\]^_
      $20-$3F    $20-$3F     !"#$%&'()*+,-./0123456789:;<=>?
      $40-$5F    $60-$7F    {$60}ABCDEFGHIJKLMNOPQRSTUVWXYZ{$7b}…{$7f}
      $60-$7F    $A0-$BF    {$a0}…{$bf}

    It may also be permissible to just pass the binary data through without hex-encoding.

    TODO: Replace with logic and maths for efficiency.
    """
    unic = ""
    redu = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_ !\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_ !\"#$%&'()*+,-./0123456789:;<=>?ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    redu_map = {ord(a): ord(b) for a, b in zip(unic, redu)}
    return utext.translate(redu_map)

def vice_ascii_str(utext):
    """
    Remap Unicode chars to ASCII mapped letters or PETSCII codes.
    These keymapped codes are suitable for loading (.P64) with VICE.

      Unicode -> PETSCII -> VICE Encoded
      $00-$1F    $40-$5F    @a…z[\\]^_
      $20-$3F    $20-$3F     !"#$%&'()*+,-./0123456789:;<=>?
      $40-$5F    $60-$7F    {$60}ABCDEFGHIJKLMNOPQRSTUVWXYZ{$7b}…{$7f}
      $60-$7F    $A0-$BF    {$a0}…{$bf}

    It may also be permissible to just pass the binary data through without hex-encoding.

    TODO: Replace with logic and maths for efficiency.
    """
    unic = ""
    redu = "@abcdefghijklmnopqrstuvwxyz[\\]^_ !\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_ !\"#$%&'()*+,-./0123456789:;<=>?ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    redu_map = {ord(a): ord(b) for a, b in zip(unic, redu)}
    return utext.translate(redu_map)

def petscii_to_unicode(petdata: bytes, aflag=False, sflag=False):
    """
    Convert PETSCII byte stream to Unicode string.
    Needed to view and edit real PETSCII in Sublime.
    Printable tokens in PETSCII are $20-$7F and $A0-$BF.
    Other tokens may have special meaning.

    Handles shift state changes via $D1/$D2 control codes.
     - aflag: Map to plain ASCII instead of PUA Unicode.
     - sflag: Use shifted mapping for Unicode output.
    """
    out = bytearray()
    shift_state = True  # False = SHIFT OFF (uppercase), True = SHIFT ON (lowercase)

    for c in petdata:

        if aflag:
            # Map to plain ASCII when requested
            if 0x41 <= c <= 0x5A:       # PETSCII uppercase A-Z ($41-$5A)
                out.append(c - 0x20)    # Convert to ASCII (65-90)
            elif 0x61 <= c <= 0x7A:     # Already lowercase
                out.append(c)
            elif c == CR:
                out.append(LF)
            elif c == SHIFT_ON:
                shift_state = True
            elif c == SHIFT_OFF:
                shift_state = False
            else:
                # For other characters, try to map sensible ASCII equivalents
                if 0x20 <= c <= 0x7E:
                    out.append(c)
                else:
                    out.append(ord('?'))

        else:
            # Handle PETSCII shift control codes first
            if c == SHIFT_ON:
                shift_state = True
                continue
            elif c == SHIFT_OFF:
                shift_state = False
                continue
            elif c == INVERT:
                # Invert is display attribute, not character mapping
                continue

            # Handle CR/LF conversion
            if c == CR:
                out.append(LF)
            else:
                # Map based on current shift state or sflag
                if 0x41 <= c <= 0x5A:  # A-Z range
                    if sflag or shift_state:
                        # Shifted mode: map to lowercase Unicode (a-z)
                        out.extend(utf_trio_bytes(c - 0x20, True))
                    else:
                        # Unshifted mode: map to uppercase Unicode (A-Z)
                        out.append(ord(chr(ord('A') + (c - 0x41))))
                elif c == 0x5C:  # Backslash character
                    out.append(c)
                else:
                    out.extend(utf_trio_bytes(c, sflag or shift_state))

    return bytes(out).decode("utf-8", errors="replace")

def petscii_file_to_unicode(filename, aflag, sflag):
    with open(filename, 'r') as f:
        petdata = f.read()
        unidata = petscii_to_unicode(petdata, aflag, sflag)
        sys.stdout.buffer.write(unidata.decode('utf-8'))

def unicode_str_to_petscii(unidata: bytes, caseCheck=False):
    pass
