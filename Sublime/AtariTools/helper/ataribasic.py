"""
ataribasic.py - AtariBASIC language data and utilities
"""
import re, math
#from typing import Callable, Dict, Iterable
from ataridefs import *

DEBUG_CODE = False

# --------------------------------------------------------------------------- #
# Lookup and conversion
# --------------------------------------------------------------------------- #

def getint(inbytes: bytearray) -> int:
    return int.from_bytes(inbytes, byteorder='little')

def decode_bcd(bcd_bytes: bytearray) -> float:
    """Decode a 6‑byte BCD floating‑point value."""
    if len(bcd_bytes) != 6:
        raise ValueError("BCD float must be exactly 6 bytes")

    # Exponent
    exp = bcd_bytes[0]
    sign = 1
    if exp > 127:              # sign bit set -> negative
        sign, exp = -1, 128

    # Integer part
    num = 0
    for b in bcd_bytes[1:]:     # Bytes 1‑5 are the BCD digits
        num = num * 100 + (b - 6 * (b >> 4))

    # Exponential Scaling
    exp -= 68
    if exp > 0:
        num *= 100 ** exp
    elif exp < 0:
        num /= 100 ** (-exp)

    return re.sub(r'\.0+$', '', str(sign * num))  # Trim trailing zeros

def string_for_command_token(token, abbrev=False):
    """Return the full or abbreviated command string for the given token."""
    if token >= len(commands_info): return "<?>"
    return commands_info[token]['abbrev' if abbrev else 'name']

def string_for_function_token(token):
    """Return the string corresponding to the given function token byte."""
    pass

def string_for_operator_token(token):
    """Return the string corresponding to the given operator token byte."""
    pass

# --------------------------------------------------------------------------- #
# BASIC listing
# --------------------------------------------------------------------------- #

def op_func_string(atok):
    """Return the string for the given operator or function token"""
    if atok < len(ops_and_funcs): return ops_and_funcs[atok]
    return f"<{atok:02X}>"

def get_number(inbuf:bytes, index:int, lowval:float=0, hival:float=0):
    """
    Extract a number from the line input buffer 'inbuf' at 'index'.
    If a range is given reject values outside the range.

    The number may be an integer (e.g. "123") or a floating point
    number (e.g., "234.5"). The number ends at the first character
    that is not a digit or a decimal point.

    A solitary “.” – or a “.” that is not followed by at least one
    digit – is **not** considered a number.
    """
    start = index
    got_neg = False
    got_dot = False
    got_digit = False
    got_exp = False
    got_exp_digit = False

    while index < len(inbuf):
        c = chr(inbuf[index])
        is_blk = c == ' '
        is_neg = c == '-'
        is_num = c.isdigit()
        is_dot = c == '.'
        is_exp = c == 'e' or c == 'E'
        if not (is_blk or is_neg or is_num or is_dot or is_exp): break

        # A blank is okay before the number, including " - 123"
        if is_blk and (got_digit or got_dot or got_exp): break

        if is_neg:
            if got_neg or got_digit or got_dot or got_exp: break
            got_neg = True

        if is_exp:
            if not got_digit: break
            if got_exp: break
            got_exp = True

        if is_dot:
            if got_dot: break           # second dot
            if got_exp: break           # dot after exp
            got_dot = True

        if is_num:
            if got_exp: got_exp_digit = True
            else: got_digit = True

        index += 1

    # At least one digit is needed
    if not got_digit: return None, start

    # If it ends with 'e' back up one char
    if got_exp and not got_exp_digit:
        index -= 1

    # Decode the numeric bytes into a float.
    # Return None if conversion fails (e.g. empty slice).
    number = None
    if index > start:
        try:
            # ``inbuf[start:index]`` is a bytes slice – it can be passed
            # directly to ``float``.
            number = float(inbuf[start:index])
            print(f"Interpreting {inbuf[start:index]} as {number}")
        except ValueError:
            pass

    if lowval != hival and not (lowval <= number <= hival):
        return None, start

    return number, index

def pack_word(val):
    """Pack a value into a little‑endian two‑byte array"""
    return bytearray([val & 0xFF, (val >> 8) & 0xFF])

def get_line_number(inbuf:bytes, index:int):
    """
    Extract a line number from the line input buffer 'inbuf' at 'index'.
    After extraction, 'index' points to the following character.
    The floor of the value is converted into a two‑byte little‑endian bytearray.

    Parameters
    ----------
    inbuf : bytearray
        The source text to scan.
    index : int
        Current position in 'inbuf'.

    Returns
    -------
    tuple[float, int]
        The floored number and the updated index.
    """

    # Get the line number, if any. Fall back to "direct line" 32768.
    lineno, index = get_number(inbuf, index, 0, 32767)
    if lineno == None: lineno = 32768
    lineno = math.floor(lineno)

    return lineno, index

# $A462 SEARCH - Based on a general search of name tables
def search_statement_name_table(lbuff:bytes, index:int):
    """
    Search commands_info 'name' fields for the string at lbuff + index.
    Return the command ID and the following index.
    On fail return kILET to try var assignment.

    Parameters
    ----------
    lbuff : bytes
        The complete input buffer (bytes).
    index : int
        Current position in lbuff from which to start the search.

    Returns
    -------
    tuple[int, int]
        (command_id, new_index)
        * command_id – index in commands_info or kILET
        * new_index  – position after the matched command name
    """

    # Go through the statement table and find the first match.
    got = False
    for t, c in enumerate(commands_info):
        cmdname = bytes(c['name'], "ascii")
        cmdlen = len(cmdname)
        ipos = index
        cpos = 0
        while ipos < len(lbuff):
            ch = lbuff[ipos:ipos+1]       # one byte from the input
            if ch == b'.':                # Reached the dot without mismatch?
                got = True                # Then eat the dot and be done
            elif ch.upper() != cmdname[cpos:cpos+1]: # Mismatch?
                break                     # Go to next command info
            ipos += 1                     # Next input index
            cpos += 1                     # Next compare index
            if cpos == cmdlen:            # Got the whole command?
                got = True

            if got: return t, ipos            # Return the command token and next index

    return kILET, index

def search_operator_name_table(lbuff:bytes, index:int):
    """
    Search ops_and_funcs for the string at lbuff + index.
    Return the operator ID and the following index.
    On fail return None.

    Parameters
    ----------
    lbuff : bytes
        The complete input buffer (bytes).
    index : int
        Current position in lbuff from which to start the search.

    Returns
    -------
    tuple[int, int]
        (operator_token, new_index)
        * operator_token – index in ops_and_funcs or None
        * new_index  – position after the matched command name
    """

    # Go through the statement table and find the first match.
    got = False
    for t, c in enumerate(ops_and_funcs[cSROP:]):
        opname = c.strip()
        oplen = len(opname)
        ipos = index
        cpos = 0
        while ipos < len(lbuff):
            ch = lbuff[ipos:ipos+1]       # one byte from the input
            if ch == ' ': continue        # ignore leading spaces
            if ch.upper() != opname[cpos:cpos+1]: # Mismatch?
                break                     # Go to next command info
            ipos += 1                     # Next input index
            cpos += 1                     # Next compare index
            if cpos == oplen:             # Got the whole command?
                got = True

            if got: return t, ipos        # Return the operator token and next index

    return None, index
