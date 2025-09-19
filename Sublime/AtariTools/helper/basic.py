#!/usr/bin/env python3
"""
basic.py - AtariBASIC language tokenizer and interpreter

Usage: basic.py [-o|--output <outfile>] [infile]

This Python Script maintains a single Atari BASIC program in tokenized form and provides an interface
for the user to modify and run it. It is meant to run in the Terminal, but when we get to the point of
using graphics we can switch to a PyGame window with an Atari color scheme, flesh out a debugger,
and so on.

For validation compare the tokenization output against Atari's original tokenizer.
Set a breakpoint at SETCODE ($A2C8) to observe tokenization in action and copy its behavior.

# Modes of operation
- In standard mode behave like any language interpreter (e.g., Python) and take text input interactively.
- For a LST input file tokenize all the lines of the file then go to interactive mode.
- For a BAS input file load the BAS file into our data structures then go to interactive mode.

# Tokenizer
The tokenizer closely mirrors the implementation from the original BASIC cartridge (fixing a bug or two),
in order to produce an exact BAS file from an input LST file.

# Program Executor
The Atari BASIC Source Book covers a lot of this. We'll just lay out the basics:
- The "Program" refers to the entire Statement Table (ST), a packed binary buffer containing line
  numbers, chunk sizes, and tokens: [ [line16][nextLine][ [nextStatement][command][(etc)][eos|eol] ]... ]...
- Memory buffers contain the state of the interpreter itself:
  - Current Offset in the Program
- Variables contain the broad state of the interpreted program
  - The Variable Name Table (VNT) is used in listing
  - The Variable Value Table is modified by the interpreter in response to the Program

# Development Plan
- Phase 1: Program Loader so we can LOAD and LIST a tokenized BAS file: load_file_BAS, list_program
- Phase 2: Interactive Mode and Program Tokenizer. Take input, tokenize, SAVE to BAS and LST files.
- Phase 3: Integrate into the Sublime plugin to validate and tokenize AtariBASIC direcly.
- Phase 4: Program Runner to actually execute statements and complete programs.

"""
import re, os, sys, argparse
from termcolor import colored
from pathlib import Path
from typing import Callable, Dict, Iterable

from atascii import atascii_to_unicode_str, unicode_to_atascii_str

DEBUG_CODE = False
args_abbrev = False

# ANSI color formatting
# black, grey, red, green, yellow, blue, magenta, cyan, light_..., white
DARK_MODE = False
def color_print(text, color):
    if DARK_MODE:
       if color == 'black': color = 'white'
       elif not (color.startswith('light_') or color == 'white'):
           color = 'light_' + color
    elif color == 'white':
        color = 'black'

    print(colored(text, color))

# Symbols used by the original tokenizer:

# LOMEM (80, 81) points to this 256 byte buffer (at the end of OS RAM):
tokenized_line = []

# VNTP (82, 83) points to the Variable Name Table start
# VNTD (84, 85) points to the Variable Name Table end (or a dummy "\0" byte)
# Entries can be indexed by the variable ID & 0x7F
variable_name_table = []

# VVTP (86, 87) points to the Variable Value Table start
# Each entry is 8 bytes long and depends on the type:
# Scalar: | 00    | Var# ||       6 byte BCD        |
# Array:  | 40/41 | Var# || valoffs || dim1 || dim2 |
# String: | 80/81 | Var# || valoffs || len  || dim  |
variable_value_table = []

# STMTAB (88, 89) points the Statement Table containing the Tokenized Program and Tokenized Immediate Line.
statement_table = []
immediate_line = None

# STMCUR points to the current statement during Program Execution.
# In Interactive Mode it points to the start of the Immediate Line.
current_statement = immediate_line

# STARP (8C, 8D) points to the string/array buffer
strings_and_arrays = []

# RUNSTK (8E, 8F) points to the BASIC runtime stack used for GOSUB/RETURN and FOR/NEXT. (And POP)
# A GOSUB entry is 4 bytes: | 0 || lineno || stoffs |
# A FOR entry is 16 bytes:
#   0: limit BCD, 6: step BCD, 12: var ID, 13: lineno, 14: stoffs
program_stack = []

# MEMTOP (90, 91) points to the end of the BASIC program.
# Atari BASIC programs will often use the RAM between this address and HIMEM (2E5, 2E6) on real machines.
#program_end = None

# --------------------------------------------------------------------------- #
# Lookup Tables
# --------------------------------------------------------------------------- #

# Commands indexed by their Token ID
basic_commands = [
    "R.EM", "D.ATA", "I.NPUT", "C.OLOR", "L.IST", "E.NTER", "LE.T", "IF", "F.OR", "N.EXT",
    "G.OTO", "GO TO", "GOS.UB", "T.RAP", "B.YE", "CONT", "COM", "CL.OSE", "CLR", "DEG",
    "DIM", "END", "NEW", "O.PEN", "LO.AD", "S.AVE", "ST.ATUS", "NO.TE", "P.OINT", "XIO",
    "ON", "POK.E", "PR.INT", "RAD", "REA.D", "RES.TORE", "RET.URN", "RU.N", "STO.P", "POP",
    "?", "GET", "PUT", "GR.APHICS", "PL.OT", "POS.ITION", "DOS", "DR.AWTO", "SE.TCOLOR", "LOC.ATE",
    "SO.UND", "LP.RINT", "CS.AVE", "CL.OAD", "(let)" if DEBUG_CODE else "", "ERROR"
]

_REM    = 0 ; _GOTO   = 10 ; _DIM    = 20 ; _ON     = 30 ; _QUESTION=40 ; _SOUND  = 50
_DATA   = 1 ; _GO_TO  = 11 ; _END    = 21 ; _POKE   = 31 ; _GET    = 41 ; _LPRINT = 51
_INPUT  = 2 ; _GOSUB  = 12 ; _NEW    = 22 ; _PRINT  = 32 ; _PUT    = 42 ; _CSAVE  = 52
_COLOR  = 3 ; _TRAP   = 13 ; _OPEN   = 23 ; _RAD    = 33 ; _GRAPHIC= 43 ; _CLOAD  = 53
_LIST   = 4 ; _BYE    = 14 ; _LOAD   = 24 ; _READ   = 34 ; _PLOT   = 44 ; _LET_   = 54
_ENTER  = 5 ; _CONT   = 15 ; _SAVE   = 25 ; _RESTORE= 35 ; _POSITION=45 ; _ERROR  = 55
_LET    = 6 ; _COM    = 16 ; _STATUS = 26 ; _RETURN = 36 ; _DOS    = 46
_IF     = 7 ; _CLOSE  = 17 ; _NOTE   = 27 ; _RUN    = 37 ; _DRAWTO = 47
_FOR    = 8 ; _CLR    = 18 ; _POINT  = 28 ; _STOP   = 38 ; _SETCOLOR=48
_NEXT   = 9 ; _DEG    = 19 ; _XIO    = 29 ; _POP    = 39 ; _LOCATE = 49

commands_info = []
c = {}

ops_and_funcs = [
    "", "", "", "", "", "", "",
    "", "", "", "", "", "", "",
    # 14-60: Operators
    "",       # 14 BCD Literal, Next 6 bytes
    "",       # 15 String Literal, Next byte is length
    "",       # 16 Double Quote (UNUSED)
    "",       # 17 Operator Stack Marker (SOE)
    ",",      # 18 , in PRINT statement
    "$",      # 19 Dollar Sign
    ":",      # 20 End of expr / statement
    ";",      # 21 ; in PRINT statement
    "",       # 22 End of expr / line
    " GOTO",  # 23 ON...GOTO
    " GOSUB", # 24 ON...GOSUB
    " TO",    # 25 FOR ... TO
    " STEP",  # 26 FOR ... TO ... STEP
    " THEN",  # 27 IF...THEN
    "#",      # 28 OPEN #
    "<=",     # 29 IF A<=B
    "<>",     # 30 IF A<>B
    ">=",     # 31 IF A<=B
    "<",      # 32 IF A<B
    ">",      # 33 IF A>B
    "=",      # 34 IF A=B
    "^",      # 35 A=B^C
    "*",      # 36 A=B*C
    "+",      # 37 A=B+C
    "-",      # 38 A=B-C
    "/",      # 39 A=B/C
    "NOT",    # 40 ... Boolean logic
    " OR",    # 41
    " AND",   # 42
    "(",      # 43 ... ( math
    ")",      # 44 ... close for all parens
    "=",      # 45 ... S1=S2
    "=",      # 46 ... N$=A$
    "<=",     # 47 ... cmp A$<=B$
    "<>",     # 48 ... cmp A$<>B$
    ">=",     # 49 ... cmp A$>=B$
    "<",      # 50 ... cmp A$<B$
    ">",      # 51 ... cmp A$<B$
    "=",      # 52 ... cmp A$=B$
    "+",      # 53 ... POSITIVE
    "-",      # 54 ... NEGATIVE
    "(",      # 55 ... A$(
    "(",      # 56 ... SC(
    "(",      # 57 ... DIM SC(
    "(",      # 58 ... CHR$(, PEEK(
    "(",      # 59 ... DIM S$(
    ",",      # 60 ... comma

    # 61-84: Functions
    "STR$", "CHR$", "USR", "ASC", "VAL", "LEN", "ADR", "ATN", "COS", "PEEK",
    "SIN", "RND", "FRE", "EXP", "LOG", "CLOG", "SQR", "SGN", "ABS", "INT",
    "PADDLE", "STICK", "PTRIG", "STRIG"
]

_ops_and_funcs = [ f"<{x:02X}={ops_and_funcs[x]}>" for x in range(60) ]

# --------------------------------------------------------------------------- #
# Handlers
# --------------------------------------------------------------------------- #

def handle_REM() -> None:
    """Handle the REM command."""
    print("OK")

def handle_DATA() -> None:
    """Handle the DATA command."""
    print("OK")

def handle_NEW() -> None:
    """Handle the NEW command."""
    global variable_name_table
    global variable_value_table
    global statement_table
    variable_name_table = []
    variable_value_table = []
    statement_table = []
    strings_and_arrays = []
    program_stack = []

def handle_LOAD() -> None:
    """Handle the LOAD command."""
    print("OK")

def handle_LIST() -> None:
    """Handle the LIST command."""
    print()
    list_program()

def handle_SAVE() -> None:
    """Handle the SAVE command."""
    print("OK")

def handle_DOS() -> None:
    """Return to the shell."""
    exit(0)

def handle_BYE() -> None:
    """Return to the shell."""
    exit(0)


# --------------------------------------------------------------------------- #
# Command dispatch table
# --------------------------------------------------------------------------- #

# Placeholders. Real handlers will be looked up by token.
COMMAND_HANDLERS: Dict[str, Callable[[], None]] = {
    "NEW":   handle_NEW,
    "LOAD":  handle_LOAD,
    "LIST":  handle_LIST,
    "SAVE":  handle_SAVE,
    "DOS":   handle_DOS,
    "BYE":   handle_BYE,
}

# --------------------------------------------------------------------------- #
# Lookup and conversion
# --------------------------------------------------------------------------- #

def getint(inbytes):
    return int.from_bytes(inbytes, byteorder='little')

def decode_bcd(bcd_bytes: bytearray) -> float:
    """Decode a 6‑byte BCD floating‑point value."""
    if len(bcd_bytes) != 6:
        raise ValueError("BCD float must be exactly 6 bytes")

    # Exponent
    exp = bcd_bytes[0]
    sign = 1
    if exp > 127:              # sign bit set → negative
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

def init_lookups():
    """Init more convenient lookup data based on the predefined arrays."""
    for i in range(len(basic_commands)):
        cmd = basic_commands[i]
        pcs = cmd.split('.')
        name = ''.join(pcs)
        v = {
            'name': name,
            'abbrev': pcs[0] + '.' if len(pcs) > 1 else name,
            'minlen': len(pcs[0])
        }
        commands_info.append(v)
        c[name] = i

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

def variable_name(token):
    """Return the variable name for the given variable token."""
    i = token & 0x7F
    if i < len(variable_name_table): return re.sub(r'[\(]', '', variable_name_table[i])
    return f"<var#{i}>"

# --------------------------------------------------------------------------- #
# BASIC listing
# --------------------------------------------------------------------------- #

def emit_arg_rest_of_line(start, end):
    """The command argument is the rest of the line"""
    print(atascii_to_unicode_str(statement_table[start:end]), end='')
    pass

def op_func_string(atok):
    """Return the string for the given operator or function token"""
    if atok < len(ops_and_funcs): return ops_and_funcs[atok]
    return f"<{atok:02X}>"

def print_variable_tables():
    print("Variables:")
    """Print all the variables with their types and sizes"""
    for i,v in enumerate(variable_value_table):
        vname = variable_name(i)
        disp = v.get('disp', -1)
        dstr = f"[{v['disp']:04X}]" if disp >= 0 else "      "
        print(f"${(i|0x80):02X} {dstr} {vname}", end='')
        t = v['type']
        if t == 0x40:
            print(f"({v['dim1']}, {v['dim2']})")
        elif t == 0x80:
            print(f"({v['maxl']}) = \"{'.'*v['curl']}\"")
        else:
            print(f" = {v['value']}")
    print()

# For structured output use an indent
args_structured = False
indent = ""

def emit_token_at_index(i):
    """Emit the token at thie given index in the statement_table"""
    atok = statement_table[i]
    if DEBUG_CODE: print(f"<{atok:02X}>", end='')

    # VARIABLE ID
    if atok & 0x80:
        print(variable_name(atok), end='')
        return 1

    # 20 End of Statement
    #if atok == 0x14:
    #    print(ops_and_funcs[atok], end='')
    #    return 1

    # 22 End of Last Statement
    #if atok == 0x16:
    #    print(ops_and_funcs[atok], end='')
    #    return 1

    # Advance past token
    i += 1

    # 14 BCD Literal, Next 6 bytes
    if atok == 0x0E:
        bcd_bytes = statement_table[i:i+6]
        bcd_value = decode_bcd(bcd_bytes)
        print(bcd_value, end='')
        return 7

    # 15 String Literal, Next byte is length
    if atok == 0x0F:
        strlen = statement_table[i]
        str_bytes = statement_table[i+1:i+1+strlen]
        print('"' + atascii_to_unicode_str(str_bytes) + '"', end='')
        return strlen + 2

    # Other operators and functions
    print(op_func_string(atok), end='')
    return 1

def list_program_abbrev(start=0, end=32767):
    list_program(start, end, True)

def list_program(start=0, end=32767, abbrev=args_abbrev):
    """
    Implement the LIST command to list the program in human-readable form.
    For 'abbrev' output abbreviated commands (e.g., "D." rather than "DATA")
    """

    stend = len(statement_table)
    if stend == 0: return

    qsp = "" if abbrev else " "

    # Loop through the statement table and interpret the data of each line.

    # Start at the first byte of the program
    i = 0

    # Loop through the program's lines
    while True:
        # The first two bytes are the little-endian line number
        lineno = getint(statement_table[i:i+2])
        if lineno > end: break

        # The next byte is the offset to the next line
        line_len = statement_table[i+2]

        # Reached the first listing line yet?
        if lineno < start: i += line_len ; continue

        # Remember the index of the start of the line
        thisline = i
        nextline = i + line_len

        # Print out the line number and detokenize the rest of the line
        #print(lineno, end=' ')

        if DEBUG_CODE: print(f"{{{line_len}}}", end='')
        print(f"{lineno}", end=' ')

        # Skip to the first statement
        i += 3

        # Detokenize statements
        while i < nextline:
            # Get the statement offset from start of line
            st_off = statement_table[i]
            if DEBUG_CODE: print(f"{{{st_off}}}", end='')
            next_st = thisline + st_off

            # Get the command token and print the command
            cmd_tok = statement_table[i+1]
            if DEBUG_CODE: print(f"<{cmd_tok:02X}>", end='')
            print(string_for_command_token(cmd_tok, abbrev), end=qsp)

            i += 2

            # REM, DATA, ERROR
            if cmd_tok in (_REM, _DATA, _ERROR):
                emit_arg_rest_of_line(i, nextline-1)
                i = nextline                    # Go right to the next line
                break

            # Emit tokens until the end of the statement
            # NOTE: Output for DIM statement needs to insert commas between items
            while i < next_st:
                # Emit the token, receiving the size of the emitted token
                tlen = emit_token_at_index(i)
                if DEBUG_CODE: print(f"[{tlen}]", end='')
                i += tlen

                #if i >= nextline:                   # At the next line?
                #    if DEBUG_CODE: print("~~~", end='')
                #    break

        #print(f" ({i})", end='')

        # Go to the next line. (Needed until tokenizer is complete.)
        i = nextline

        #print(f" ({i})")

        print()
        if i >= len(statement_table): break

# --------------------------------------------------------------------------- #
# BASIC tokenization
# --------------------------------------------------------------------------- #

def add_variable(var_name):
    """
    Variables are added as soon as they are parsed in a statement, so
    the statement only needs to contain the variable ID.
    """
    pass

def identify_keyword(statement_txt):
    """
    Scan the input string until a keyword is identified.
    Recognize abbreviations of keywords (ending with '.') over a minimum abbrev. length.
    If the input matches no keywords assume Implied LET ($36).
    If the input can't be a keyword, return ERROR ($37).
    """
    pass

def tokenize_line(line_txt):
    """
    Tokenize a complete input line of AtariBASIC.
    Return the tokenized representation ready for whatever use.
    This may return a line tokenized with "ERROR" ($37) if there was a problem.
    """
    pass

def tokenize_and_apply_line(line_txt):
    """
    Tokenize a complete input line of AtariBASIC and apply it.
    - Lines with no number are executed right away (as if they were the last line in the program).
    - Lines with only a number cause the line with matching number to be deleted from the program.
    - Lines with a number and one or more statements are added to the program.
    """
    pass

def consolidate_tokenized_program():
    """
    Gather all the program data into a single buffer matching a real Atari program buffer,
    suitable for saving to a BAS file.
    Return the buffer.
    """
    pass

def save_program(outpath, asListing=False, abbrev=args_abbrev):
    """Save the stored program to a tokenized .BAS file or to a .LST file."""
    try:
        f = open(outpath, 'w')
    except IOError as e:
        print(f"Error {e} trying to open {outpath}")
        sys.exit(1)

    if asListing:
        oldout = sys.stdout
        sys.stdout = f
        if abbrev:
            list_program_abbrev()
        else:
            list_program()
        sys.stdout = oldout
    else:
        buffer = consolidate_tokenized_program()
        try:
            f.write(buffer)
            f.close()
            print(f"Saved to {outpath}")
        except IOError as e:
            print(f"Error {e} writing to {outpath}")
            sys.exit(1)

# --------------------------------------------------------------------------- #
# Program Runner
# --------------------------------------------------------------------------- #

def execute_tokenized_statement(st_bytes):
    """
    Execute a single tokenized statement.
    """
    pass

def execute_tokenized_line(line_bytes):
    """
    Execute a whole tokenized line.
    """
    pass

# --------------------------------------------------------------------------- #
# Interactive Mode
# --------------------------------------------------------------------------- #

def interactive_mode():
    """
    Do a loop of input and feedback (interactive mode):
    - Accept a line of input, parse it as AtariBASIC.
      - If there is an error in the line, report the error.
      - If the line has no number, run the tokenized line.
        - (Until the Program Runner is written we'll just handle LIST, RUN, SAVE...)
      - Otherwise, insert the tokenized line into to the program.
    - Write the output from commands.
    - If the program is entered (with RUN, GOTO, GOSUB, etc.), go to the program executor until it exits or hits an error.
    """
    nowReady = True
    while True:
        if nowReady: print("\nREADY") ; nowReady = False

        try:
            raw = input()
        except (KeyboardInterrupt, EOFError):
            print("\n\nBye!\n")
            break

        cmd = raw.strip().upper()
        if cmd in ("QUIT", "EXIT", "Q"):
            print("\nBye!\n")
            break

        cmd_name = cmd.split(' ')[0]
        handler = COMMAND_HANDLERS.get(cmd_name)
        if handler:
            handler()
            nowReady = True
        elif cmd != "":
            print(f"Unknown command: {cmd_name!r}.  Valid commands are: {', '.join(COMMAND_HANDLERS)}")

# --------------------------------------------------------------------------- #
# LOAD from BAS file
# --------------------------------------------------------------------------- #

def ingest_variable_name_table(vnt_data):
    """Extract variable names from the VNT buffer."""
    vnt = []
    name_bytes = bytearray()            # Temporary holder

    for b in vnt_data:
        if b & 0x80:                    # High bit set → end of name
            name_bytes.append(b & 0x7F) # Clear high bit
            vnt.append(name_bytes.decode('ascii'))
            name_bytes.clear()          # Start a new name
            #print(f"Variable {len(vnt)-1}: '{varname}'")
        else:
            name_bytes.append(b)        # Accumulate the name bytes

    # For now ignore any trailing incomplete name
    return vnt

def ingest_variable_value_table(vvt_data):
    """Extract variable dimensions and values from the VVT buffer."""
    vvt = []
    val_bytes = bytearray()                         # Temporary holder

    for i in range(0, len(vvt_data), 8):
        chunk = vvt_data[i:i+8]

        if len(chunk) != 8:
            raise ValueError(f"Incomplete 8‑byte entry at offset {vvt_offset + i}")

        var_type = chunk[0] & 0xC0                  # High 2 bits: type
        var_id = chunk[1] & 0x7F                    # Low 7 bits: variable number
        item = { 'type': var_type, 'id': var_id }
        if var_type == 0x40:
            # Array: disp, dim1, dim2
            item['disp'] = getint(chunk[2:4])    # Displacement in string/array memory
            item['dim1'] = getint(chunk[4:6])    # Array dimension 1
            item['dim2'] = getint(chunk[6:8])    # Array dimension 2 (or 1)
            #print(f"Array {variable_name(var_id)}({item['dim1']}, {item['dim2']}) [{item['disp']}]")
        elif var_type == 0x80:
            # String: disp, curl, maxl
            item['disp'] = getint(chunk[2:4])    # Displacement in string/array memory
            item['curl'] = getint(chunk[4:6])    # Current length
            item['maxl'] = getint(chunk[6:8])    # Max length as indicated by DIM
            #print(f"String {variable_name(var_id)}({item['maxl']}) [{item['disp']}] = \"{'.'*item['curl']}\"")
        else:
            item['value'] = decode_bcd(chunk[2:])
            #print(f"Scalar {variable_name(var_id)} = {item['value']}")

        vvt.append(item)

    # If the file is malformed and we ended mid‑name, we could handle it here.
    # For now we ignore any trailing incomplete name.
    return vvt

def ingest_statement_table(st_data):
    return st_data

def load_file_BAS(inpath):
    """
    Load the complete program from a BAS file into our data structures.
    Return True if successful.
    """
    try:
        with open(inpath, "rb") as f:
            buffer = bytearray(f.read())
    except FileNotFoundError:
        print(f"Error: File not found – {inpath}")
        return False
    except IOError as e:
        print(f"Error reading {inpath}: {e}")
        return False

    # ----- BASIC header validation -----
    HEADER_SIZE = 14

    # First two bytes should be little‑endian 0 (0x0000)
    # Next two bytes should be little‑endian 256 (0x0100)
    if len(buffer) < 4 or buffer[0:2] != b'\x00\x00' or buffer[2:4] != b'\x00\x01':
        print("Error: Not a valid Atari BASIC file.") ; return False

    # Bytes 4-5 are UNUSED

    # Bytes 6-7 are the VVT offset, plus 0x100
    vvt_offset = HEADER_SIZE + getint(buffer[6:8]) - 0x100

    # Bytes 8-9 are the Statement Table offset, plus 0x100
    st_offset = HEADER_SIZE + getint(buffer[8:10]) - 0x100

    # Bytes 10-11 are UNUSED

    # Bytes 12-13 are the offset to end of file from the beginning of the data, plus 0x100
    end_offset = HEADER_SIZE + getint(buffer[12:14]) - 0x100

    # Load the raw Variable Name Table data into the variable_name_table array
    global variable_name_table                  # global required for reaassignment
    vnt_data = buffer[HEADER_SIZE:vvt_offset]
    variable_name_table = ingest_variable_name_table(vnt_data)

    # Load the raw Variable Value Table data into the variable_value_table array
    global variable_value_table
    vvt_data = buffer[vvt_offset:st_offset]
    variable_value_table = ingest_variable_value_table(vvt_data)

    # Load the raw Statement Table data into the statement_table array
    global statement_table
    st_data = buffer[st_offset:end_offset]
    statement_table = ingest_statement_table(st_data)

    #print(f"Offsets: VNT={HEADER_SIZE}, VVT={vvt_offset}, ST={st_offset}, END={end_offset}")
    #print(f"Lengths: VNT={len(vnt_data)}, VVT={len(vvt_data)}, ST={len(st_data)}")

    return True

# --------------------------------------------------------------------------- #
# LOAD from LST file
# --------------------------------------------------------------------------- #

def load_file_LST(inpath):
    """
    Load a complete input file of AtariBASIC in LST format, tokenize_and_apply_line each line.
    This function expects the input in ATASCII native format,
    but later we can use atascii.py (i.e., import atascii) to
    convert Atari-specific UTF-8 characters to ATASCII.
    """
    pass

def main():
    """
    Parse input arguments and decide where to route the program next.
    - If there is an 'infile' argument tokenize all the file's lines.
    - For an 'outfile' argument:
      - The argument is invalid without an 'infile' argument.
      - Save the tokenized BASIC to the given filename.
      - Exit.
    - For no 'outfile' argument:
      - Enter Interactive Mode.
      - (Once the Program Runner is written we will run the program instead.)
    """
    parser = argparse.ArgumentParser(
        description="AtariBASIC tokenizer & interpreter",
        usage="basic.py [-l|--list] [-o|--output <outfile>] [infile]"
    )
    parser.add_argument('-l', '--list', action="store_true", help='Just list the program and exit.')
    parser.add_argument('-a', '--abbrev', action="store_true", help='Produce an abbreviated listing.')
    parser.add_argument('-s', '--struct', action="store_true", help='Display structured output.')
    parser.add_argument('-t', '--tvars', action="store_true", help='Print the VNT and VVT tables.')
    parser.add_argument('-o', '--output', type=Path, help='Path to write the resulting tokenized program (BAS file).')
    parser.add_argument('infile', nargs='?', type=Path, help='Optional input file (LST or BAS).')
    args = parser.parse_args()

    # ----- Argument validation -----
    if not args.infile:
        if args.output: parser.error("The '-o/--output' option requires an input file.")
        if args.list: parser.error("The '-l/--list' option requires an input file.")
        if args.tvars: parser.error("The '-t/--tvars' option requires an input file.")

    # Display listings with abbreviated keywords, minimal whitespace
    global args_abbrev
    if args.abbrev: args_abbrev = True

    # Display listings in a structured format
    global args_struct
    if args.struct: args_struct = True

    # ----- Init BASIC data and state -----

    init_lookups()

    # ----- Handle the three modes -----
    if args.infile:
        # Load the input file (currently just raw bytes)
        result = load_file_BAS(args.infile)
        if result is False: sys.exit(1)

        # If an output file was supplied, write the buffer there.
        if args.output != None:
            save_program(args.output, args.list)
            exit(0)
        elif args.list:
            print()
            if args.tvars: print_variable_tables()
            if args_abbrev:
                list_program_abbrev()
            else:
                list_program()
            print()
            exit(0)
        elif args.tvars:
            print_variable_tables()
            exit(0)

    interactive_mode()

if __name__ == "__main__":
    main()
