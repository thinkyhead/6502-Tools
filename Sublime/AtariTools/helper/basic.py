#!/usr/bin/env python
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
import re, os, sys, math, argparse
from termcolor import colored
from pathlib import Path
from typing import Callable, Dict, Iterable

from atascii import atascii_to_unicode_str, unicode_to_atascii_str, ATEOL

DEBUG_CODE = False
args_abbrev = False
args_colorify = False
list_colorify = False

# ANSI color formatting
# black, grey, red, green, yellow, blue, magenta, cyan, light_..., white
DARK_MODE = False
def color_print(text, color, end=None):
    if DARK_MODE:
       if color == 'black': color = 'white'
       elif not (color.startswith('light_') or color == 'white'):
           color = 'light_' + color
    #elif color == 'white':
    #    color = 'black'

    print(colored(text, color), end=end)

# Colorize only if the flag is set
def colorize(text, color=None, end=None):
    if list_colorify and color:
        color_print(text, color, end=end)
    else:
        print(text, end=end)

# --------------------------------------------------------------------------- #
# Static lookup Tables
# --------------------------------------------------------------------------- #

# Commands indexed by their Token ID
statement_name_table = (
    ".REM", "D.ATA", "I.NPUT", "C.OLOR", "L.IST", "E.NTER", "LE.T", "IF", "F.OR", "N.EXT",
    "G.OTO", "GO TO", "GOS.UB", "T.RAP", "B.YE", "CONT", "COM", "CL.OSE", "CLR", "DEG",
    "DIM", "END", "NEW", "O.PEN", "LO.AD", "S.AVE", "ST.ATUS", "NO.TE", "P.OINT", "XIO",
    "ON", "POK.E", "PR.INT", "RAD", "REA.D", "RES.TORE", "RET.URN", "RU.N", "STO.P", "POP",
    "?", "GET", "PUT", "GR.APHICS", "PL.OT", "POS.ITION", "DOS", "DR.AWTO", "SE.TCOLOR", "LOC.ATE",
    "SO.UND", "LP.RINT", "CS.AVE", "CL.OAD", "(let)" if DEBUG_CODE else "", "ERROR  -"
)

kREM    = 0 ; kGOTO   = 10 ; cDIM    = 20 ; kON     = 30 ; kQUESTION=40 ; kSOUND  = 50
kDATA   = 1 ; kGO_TO  = 11 ; cEND    = 21 ; kPOKE   = 31 ; kGET    = 41 ; kLPRINT = 51
kINPUT  = 2 ; kGOSUB  = 12 ; cNEW    = 22 ; kPRINT  = 32 ; kPUT    = 42 ; kCSAVE  = 52
kCOLOR  = 3 ; kTRAP   = 13 ; cOPEN   = 23 ; kRAD    = 33 ; kGRAPHIC= 43 ; kCLOAD  = 53
kLIST   = 4 ; kBYE    = 14 ; cLOAD   = 24 ; kREAD   = 34 ; kPLOT   = 44 ; kILET   = 54
kENTER  = 5 ; kCONT   = 15 ; cSAVE   = 25 ; kRESTORE= 35 ; kPOSITION=45 ; kERROR  = 55
kLET    = 6 ; kCOM    = 16 ; cSTATUS = 26 ; kRETURN = 36 ; kDOS    = 46
kIF     = 7 ; kCLOSE  = 17 ; cNOTE   = 27 ; kRUN    = 37 ; kDRAWTO = 47
kFOR    = 8 ; kCLR    = 18 ; cPOINT  = 28 ; kSTOP   = 38 ; kSETCOLOR=48
kNEXT   = 9 ; kDEG    = 19 ; cXIO    = 29 ; kPOP    = 39 ; kLOCATE = 49

commands_info = []
c = {}

ops_and_funcs = (
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
    ")",      # 44 ... close for all parens. Solve before moving on.
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
)

# Syntax Rules Tokens

kANTV   = 0x00  # Absolute Non-Terminal Vector (ANTV) to sub-call another rule
kESRT   = 0x01  # External Subroutine Call (ESRT) to call a handler for more complex rules
kOR     = 0x02  # ABML or
kRTN    = 0x03  # (aka <END>) Return, marks the end of an ABML rule. Return pass or fail.
kVEXP   = 0x0E  # (aka <EXP>) Expression Non-Terminal Vector. Shorthand for ANTV AD(EXP)
kCHNG   = 0x0F  # Change Last Token to X. e.g., to rectify '=' as assign or compare.

# Program Tokens

cDQ     = 0x10  # "      Double-Quote (UNUSED?)
cSOE    = 0x11  #        Expression Stack Marker
cCOM    = 0x12  # ,      in PRINT statement
cDOL    = 0x13  # $      Dollar Sign
cEOS    = 0x14  # :      End of expr / statement
cSC     = 0x15  # ;      in PRINT statement
cCR     = 0x16  #        End of expr / line
cGTO    = 0x17  # GOTO   ON...GOTO
cGS     = 0x18  # GOSUB  ON...GOSUB
cTO     = 0x19  # TO     FOR ... TO
cSTEP   = 0x1A  # STEP   FOR ... TO ... STEP
cTHEN   = 0x1B  # THEN   IF...THEN
cPND    = 0x1C  # #      OPEN #

# "Real" Operators
cSROP   = 0x1D  # First "real" operator

cLE     = 0x1D  # <=     IF A<=B
cNE     = 0x1E  # <>     IF A<>B
cGE     = 0x1F  # >=     IF A<=B
cGT     = 0x20  # <      IF A<B
cLT     = 0x21  # >      IF A>B
cEO     = 0x22  # =      IF A=B
cEXP    = 0x23  # ^      A=B^C   (Up Arrow $5E+$80)
cMUL    = 0x24  # *      A=B*C
cPLUS   = 0x25  # +      A=B+C
cMINUS  = 0x26  # -      A=B-C
cDIV    = 0x27  # /      A=B/C
cNOT    = 0x28  # NOT    Boolean logic
cOR     = 0x29  # OR
cAND    = 0x2A  # AND
cLPRN   = 0x2B  # (      ( math
cRPRN   = 0x2C  # )      close for all parens
cAASN   = 0x2D  # =      S1=S2
cSASN   = 0x2E  # =      N$=A$
cSLE    = 0x2F  # <=     cmp A$<=B$
cSNE    = 0x30  # <>     cmp A$<>B$
cSGE    = 0x31  # >=     cmp A$>=B$
cSLT    = 0x32  # <      cmp A$<B$
cSGT    = 0x33  # >      cmp A$<B$
cSEQ    = 0x34  # =      cmp A$=B$
cUPLUS  = 0x35  # +      POSITIVE
cUMINUS = 0x36  # -      NEGATIVE
cSLPRN  = 0x37  # (      A$(
cALPRN  = 0x38  # (      SC(
cDLPRN  = 0x39  # (      DIM SC(
cFLPRN  = 0x3A  # (      CHR$(, PEEK(
cDSLPR  = 0x3B  # (      DIM S$(
cACOM   = 0x3C  # ,      Array Subscript Separator

# Function Tokens
cFFUN   = 0x3D  # FIRST FUNCTION CODE

cSTR    = 0x3D  # STR$()
cCHR    = 0x3E  # CHR$()
cUSR    = 0x3F  # USR()
cASC    = 0x40  # ASC()
cVAL    = 0x41  # VAL()
cLEN    = 0x42  # LEN()
cADR    = 0x43  # ADR()
cNFNP   = 0x44  # Numeric Functions
cATN    = 0x44  # ATN()
cCOS    = 0x45  # COS()
cPEEK   = 0x46  # PEEK()
cSIN    = 0x47  # SIN()
cRND    = 0x48  # RND()
cFRE    = 0x49  # FRE()
cEXP    = 0x4A  # EXP()
cLOG    = 0x4B  # LOG()
cCLOG   = 0x4C  # CLOG()
cSQR    = 0x4D  # SQR()
cSGN    = 0x4E  # SGN()

class Program:
    """
    Encapsulates an Atari BASIC program: the VNT, VVT, ST,
    runtime stack, and all helper methods for loading, listing,
    tokenising, etc.
    """
    def __init__(self) -> None:
        # --- Core program data ---------------------------------------------

        # VNTP (82, 83) points to the Variable Name Table start
        # VNTD (84, 85) points to the Variable Name Table end (or a dummy "\0" byte)
        # Entries can be indexed by the variable ID & 0x7F
        self.variable_name_table: List[str] = []

        # VVTP (86, 87) points to the Variable Value Table start
        # Each entry is 8 bytes long and depends on the type:
        # Scalar: | 00    | Var# ||       6 byte BCD        |
        # Array:  | 40/41 | Var# || valoffs || dim1 || dim2 |
        # String: | 80/81 | Var# || valoffs || len  || dim  |
        self.variable_value_table: List[Dict] = []   # entries are dicts

        # STMTAB (88, 89) points the Statement Table containing the Tokenized Program and Tokenized Immediate Line.
        # Each statement is variable length, with the following format:
        # 00-01 : Line Number in 16 bit little endian format
        #    02 : Offset to the next line from the start of this line
        # 03-?? : Statements with their own offsets, end markers, etc.
        self.statement_table: bytearray = bytearray()

        # Statements could be split up by line for easier insertion of new statements
        # whereas the original tokenizer expands and contracts each buffer as needed
        self.program_statements: Dict = {}

        # STARP (8C, 8D) points to the string/array buffer
        self.strings_and_arrays: bytearray = bytearray()

        # --- Runtime state --------------------------------------------------

        # RUNSTK (8E, 8F) points to the BASIC runtime stack used for GOSUB/RETURN and FOR/NEXT. (And POP)
        # A GOSUB entry is 4 bytes: | 0 || lineno || stoffs |
        # A FOR entry is 16 bytes:
        #   0: limit BCD, 6: step BCD, 12: var ID, 13: lineno, 14: stoffs
        self.program_stack: List[int] = []          # GOSUB/RETURN, FOR/NEXT

        # MEMTOP (90, 91) points to the end of the BASIC program.
        # Atari BASIC programs will often use the RAM between this address and HIMEM (2E5, 2E6) on real machines.
        #self.program_end = None

        # STMCUR points to the current statement during Program Execution.
        # In Interactive Mode it points to the start of the Immediate Line.
        self.current_statement: int | None = None   # offset in statement_table

        # --- UI / listing flags ---------------------------------------------
        self.abbrev: bool = False
        self.colorify: bool = False

    def insert_statement(self, statement) -> None:
        """
        Insert a statement - an array of bytes - into the existing
        statement_table, expanding the bytearray to put this
        statement in sequence by line number.
        """
        # Get the line number from the first two bytes of 'statement'
        lineno = getint(statement[0:2])
        # Scan the statement_table to find the start of a
        # line with a higher number than 'lineno'.
        # ...
        pass

    def handle_NEW(self) -> None:
        """Handle the NEW command."""
        self.variable_name_table = []
        self.variable_value_table = []
        self.statement_table = []
        self.strings_and_arrays = []
        self.program_stack = []

prog = Program()

# Symbols used by the original tokenizer:

# LOMEM (80, 81) points to this 256 byte buffer (at the end of OS RAM):
tokenized_line: bytearray = bytearray()

# VNTP (82, 83) points to the Variable Name Table start
# VNTD (84, 85) points to the Variable Name Table end (or a dummy "\0" byte)
# Entries can be indexed by the variable ID & 0x7F
variable_name_table: bytearray = bytearray()

# VVTP (86, 87) points to the Variable Value Table start
# Each entry is 8 bytes long and depends on the type:
# Scalar: | 00    | Var# ||       6 byte BCD        |
# Array:  | 40/41 | Var# || valoffs || dim1 || dim2 |
# String: | 80/81 | Var# || valoffs || len  || dim  |
variable_value_table: bytearray = bytearray()

# STMTAB (88, 89) points the Statement Table containing the Tokenized Program and Tokenized Immediate Line.
statement_table: bytearray = bytearray()
immediate_line = None

# STMCUR points to the current statement during Program Execution.
# In Interactive Mode it points to the start of the Immediate Line.
current_statement = immediate_line

# STARP (8C, 8D) points to the string/array buffer
strings_and_arrays: bytearray = bytearray()

# RUNSTK (8E, 8F) points to the BASIC runtime stack used for GOSUB/RETURN and FOR/NEXT. (And POP)
# A GOSUB entry is 4 bytes: | 0 || lineno || stoffs |
# A FOR entry is 16 bytes:
#   0: limit BCD, 6: step BCD, 12: var ID, 13: lineno, 14: stoffs
program_stack: bytearray = bytearray()

# MEMTOP (90, 91) points to the end of the BASIC program.
# Atari BASIC programs will often use the RAM between this address and HIMEM (2E5, 2E6) on real machines.
#program_end = None

# --------------------------------------------------------------------------- #
# Operator and Function Execution
# --------------------------------------------------------------------------- #

def op_LE()     -> None: pass
def op_NE()     -> None: pass
def op_GE()     -> None: pass
def op_LT()     -> None: pass
def op_GT()     -> None: pass
def op_EQ()     -> None: pass
def op_POWER()  -> None: pass
def op_MUL()    -> None: pass
def op_PLUS()   -> None: pass
def op_MINUS()  -> None: pass
def op_DIV()    -> None: pass
def op_NOT()    -> None: pass
def op_OR()     -> None: pass
def op_AND()    -> None: pass
def op_LPRN()   -> None: pass
def op_RPRN()   -> None: pass
def op_AASN()   -> None: pass
def xs_AASN()   -> None: pass
def op_SLE()    -> None: pass
def op_SNE()    -> None: pass
def op_SGE()    -> None: pass
def op_SLT()    -> None: pass
def op_SGT()    -> None: pass
def op_SEQ()    -> None: pass
def op_UPLUS()  -> None: pass
def op_UMINUS() -> None: pass
def op_SLPRN()  -> None: pass
def op_ALPRN()  -> None: pass
def op_DLPRN()  -> None: pass
def op_FLPRN()  -> None: pass
def op_DSLPR()  -> None: pass
def op_ACOM()   -> None: pass
def op_STR()    -> None: pass
def op_CHR()    -> None: pass
def op_USR()    -> None: pass
def op_ASC()    -> None: pass
def op_VAL()    -> None: pass
def op_LEN()    -> None: pass
def op_ADR()    -> None: pass
def op_ATN()    -> None: pass
def op_COS()    -> None: pass
def op_PEEK()   -> None: pass
def op_SIN()    -> None: pass
def op_RND()    -> None: pass
def op_FRE()    -> None: pass
def op_EXP()    -> None: pass
def op_LOG()    -> None: pass
def op_L10()    -> None: pass
def op_SQR()    -> None: pass
def op_SGN()    -> None: pass
def op_ABS()    -> None: pass
def op_INT()    -> None: pass
def op_PADDLE() -> None: pass
def op_STICK()  -> None: pass
def op_PTRIG()  -> None: pass
def op_STRIG()  -> None: pass

# OPETAB - Operator Execution Table
# - Contains operator handler function refs
# - Same order as Operator Name Table
misc_ops_table = (
    op_LE,      op_NE,      op_GE,      op_LT,      op_GT,
    op_EQ,      op_POWER,   op_MUL,     op_PLUS,    op_MINUS,
    op_DIV,     op_NOT,     op_OR,      op_AND,     op_LPRN,
    op_RPRN,    op_AASN,    xs_AASN,    op_SLE,     op_SNE,
    op_SGE,     op_SLT,     op_SGT,     op_SEQ,     op_UPLUS,
    op_UMINUS,  op_SLPRN,   op_ALPRN,   op_DLPRN,   op_FLPRN,
    op_DSLPR,   op_ACOM,
    op_STR,     op_CHR,     op_USR,     op_ASC,     op_VAL,
    op_LEN,     op_ADR,     op_ATN,     op_COS,     op_PEEK,
    op_SIN,     op_RND,     op_FRE,     op_EXP,     op_LOG,
    op_L10,     op_SQR,     op_SGN,     op_ABS,     op_INT,
    op_PADDLE,  op_STICK,   op_PTRIG,   op_STRIG,
)

#_ops_and_funcs = [ f"<{x:02X}={ops_and_funcs[x]}>" for x in range(60) ]

# --------------------------------------------------------------------------- #
# Immediate Command Handlers
# Interpret our own basic.py commands
# --------------------------------------------------------------------------- #

def h_REM()  -> None: print("OK")
def h_DATA() -> None: print("OK")
def h_LOAD() -> None: print("OK")
def h_SAVE() -> None: print("OK")

def h_DOS() -> None: x_DOS()
def h_BYE() -> None: x_BYE()
def h_NEW() -> None: x_NEW()

def h_LIST() -> None:
    print()
    list_program(abbrev=args_abbrev)

def h_CLIST() -> None:
    print()
    list_program(abbrev=args_abbrev, colorify=True)

# --------------------------------------------------------------------------- #
# Command dispatch table
# --------------------------------------------------------------------------- #

# Placeholders. Real handlers will be looked up by token.
COMMAND_HANDLERS: Dict[str, Callable[[], None]] = {
    "NEW":   h_NEW,
    "LOAD":  h_LOAD,
    "LIST":  h_LIST,
    "CLIST": h_CLIST,
    "SAVE":  h_SAVE,
    "DOS":   h_DOS,
    "BYE":   h_BYE,
}

# --------------------------------------------------------------------------- #
# Statement Execution
# --------------------------------------------------------------------------- #

def x_REM()      -> None: pass
def x_DATA()     -> None: pass
def x_INPUT()    -> None: pass
def x_COLOR()    -> None: pass
def x_LIST()     -> None: pass
def x_ENTER()    -> None: pass
def x_LET()      -> None: pass
def x_IF()       -> None: pass
def x_FOR()      -> None: pass
def x_NEXT()     -> None: pass
def x_GOTO()     -> None: pass
def x_GOTO()     -> None: pass
def x_GOSUB()    -> None: pass
def x_TRAP()     -> None: pass

# XBYE - Execute BYE
def x_BYE():
    #do_CLSALL() # Close IOCB 1-7
    #jmp BYELOC
    exit(0)

def x_CONT()     -> None: pass
def x_COM()      -> None: pass
def x_CLOSE()    -> None: pass
def x_CLR()      -> None: pass
def x_DEG()      -> None: pass
def x_DIM()      -> None: pass
def x_END()      -> None: pass

def x_NEW()      -> None:
    global variable_name_table
    global variable_value_table
    global statement_table
    global strings_and_arrays
    global program_stack
    variable_name_table = []
    variable_value_table = []
    statement_table = []
    strings_and_arrays = []
    program_stack = []
    pass

def x_OPEN()     -> None: pass
def x_LOAD()     -> None: pass
def x_SAVE()     -> None: pass
def x_STATUS()   -> None: pass
def x_NOTE()     -> None: pass
def x_POINT()    -> None: pass
def x_XIO()      -> None: pass
def x_ON()       -> None: pass
def x_POKE()     -> None: pass
def x_PRINT()    -> None: pass
def x_RAD()      -> None: pass
def x_READ()     -> None: pass
def x_RESTORE()  -> None: pass
def x_RETURN()   -> None: pass
def x_RUN()      -> None: pass
def x_STOP()     -> None: pass
def x_POP()      -> None: pass
def x_PRINT()    -> None: pass
def x_GET()      -> None: pass
def x_PUT()      -> None: pass
def x_GRAPHICS() -> None: pass
def x_PLOT()     -> None: pass
def x_POSITION() -> None: pass

# XDOS - Exit to DOS
def x_DOS():
    #do_CLSALL() # Close IOCB 1-7
    #jmp (DOSLOC)
    exit(0)

def x_DRAWTO()   -> None: pass
def x_SETCOLOR() -> None: pass
def x_LOCATE()   -> None: pass
def x_SOUND()    -> None: pass
def x_LPRINT()   -> None: pass
def x_CSAVE()    -> None: pass
def x_CLOAD()    -> None: pass
def x_ILET()     -> None: pass
def x_ERROR()    -> None: pass

# Statement Execution Table
# - Contains Statement Execution refs
# - Must be in same order as Statement Name Table
handler_table = [
    x_REM,      x_DATA,     x_INPUT,    x_COLOR,    x_LIST,
    x_ENTER,    x_LET,      x_IF,       x_FOR,      x_NEXT,
    x_GOTO,     x_GOTO,     x_GOSUB,    x_TRAP,     x_BYE,
    x_CONT,     x_COM,      x_CLOSE,    x_CLR,      x_DEG,
    x_DIM,      x_END,      x_NEW,      x_OPEN,     x_LOAD,
    x_SAVE,     x_STATUS,   x_NOTE,     x_POINT,    x_XIO,
    x_ON,       x_POKE,     x_PRINT,    x_RAD,      x_READ,
    x_RESTORE,  x_RETURN,   x_RUN,      x_STOP,     x_POP,
    x_PRINT,    x_GET,      x_PUT,      x_GRAPHICS, x_PLOT,
    x_POSITION, x_DOS,      x_DRAWTO,   x_SETCOLOR, x_LOCATE,
    x_SOUND,    x_LPRINT,   x_CSAVE,    x_CLOAD,    x_ILET,
    x_ERROR
]

# Syntax tables encodes required patterns for each command
s_REM      = ()
s_DATA     = ()
s_INPUT    = ()
s_COLOR    = ()
s_LIST     = ()
s_ENTER    = ()
s_LET      = ()
s_IF       = ()
s_FOR      = ()
s_NEXT     = ()
s_GOTO     = ()
s_GOTO     = ()
s_GOSUB    = ()
s_TRAP     = ()
s_BYE      = ()
s_CONT     = ()
s_COM      = ()
s_CLOSE    = ()
s_CLR      = ()
s_DEG      = ()
s_DIM      = ()
s_END      = ()
s_NEW      = ()
s_OPEN     = ()
s_LOAD     = ()
s_SAVE     = ()
s_STATUS   = ()
s_NOTE     = ()
s_POINT    = ()
s_XIO      = ()
s_ON       = ()
s_POKE     = ()
s_PRINT    = ()
s_RAD      = ()
s_READ     = ()
s_RESTORE  = ()
s_RETURN   = ()
s_RUN      = ()
s_STOP     = ()
s_POP      = ()
s_PRINT    = ()
s_GET      = ()
s_PUT      = ()
s_GRAPHICS = ()
s_PLOT     = ()
s_POSITION = ()
s_DOS      = ()
s_DRAWTO   = ()
s_SETCOLOR = ()
s_LOCATE   = ()
s_SOUND    = ()
s_LPRINT   = ()
s_CSAVE    = ()
s_CLOAD    = ()
s_ILET     = ()
s_ERROR    = ()

# Statement Syntax Table : Pointers to syntax tables from the "Statement Name Table"
statement_syntax_table = [
    s_REM,      s_DATA,     s_INPUT,    s_COLOR,    s_LIST,
    s_ENTER,    s_LET,      s_IF,       s_FOR,      s_NEXT,
    s_GOTO,     s_GOTO,     s_GOSUB,    s_TRAP,     s_BYE,
    s_CONT,     s_COM,      s_CLOSE,    s_CLR,      s_DEG,
    s_DIM,      s_END,      s_NEW,      s_OPEN,     s_LOAD,
    s_SAVE,     s_STATUS,   s_NOTE,     s_POINT,    s_XIO,
    s_ON,       s_POKE,     s_PRINT,    s_RAD,      s_READ,
    s_RESTORE,  s_RETURN,   s_RUN,      s_STOP,     s_POP,
    s_PRINT,    s_GET,      s_PUT,      s_GRAPHICS, s_PLOT,
    s_POSITION, s_DOS,      s_DRAWTO,   s_SETCOLOR, s_LOCATE,
    s_SOUND,    s_LPRINT,   s_CSAVE,    s_CLOAD,    s_ILET,
    s_ERROR
]

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
    for i in range(len(statement_name_table)):
        cmd = statement_name_table[i]
        pcs = cmd.split('.')
        name = ''.join(pcs)
        v = {
            'name': name,
            'abbrev': pcs[0] + '.' if len(pcs) > 1 else name,
            'minlen': len(pcs[0])
        }
        if v['abbrev'] == '.': v['abbrev'] = '. '
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

def _colour_for_token(tok_type: str) -> str:
    """Return the ANSI colour name for a token type."""
    return {
        'command':  'yellow',
        'function': 'light_red',
        'number':   'white',
        'string':   'light_blue',
        'variable': 'green',
        'data':     'grey'
    }[tok_type]

def emit_arg_rest_of_line(start, end):
    """The command argument is the rest of the line"""
    colorize(atascii_to_unicode_str(statement_table[start:end]), _colour_for_token('data'), end='')
    pass

def op_func_string(atok):
    """Return the string for the given operator or function token"""
    if atok < len(ops_and_funcs): return ops_and_funcs[atok]
    return f"<{atok:02X}>"

def print_variable_tables():
    """Print all the variables with their types and sizes"""
    if len(variable_value_table):
        print("Variables:")
    else:
        print("No Variables")
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

    # 80-FF Variable ID
    if atok & 0x80:
        colorize(variable_name(atok), color=_colour_for_token('variable'), end='')
        return 1

    # 14 (20) End of Statement
    #if atok == 0x14:
    #    print(ops_and_funcs[atok], end='')
    #    return 1

    # 16 (22) End of Last Statement
    #if atok == 0x16:
    #    print(ops_and_funcs[atok], end='')
    #    return 1

    # Advance past token
    i += 1

    # 0E (14) BCD Literal, Next 6 bytes
    if atok == 0x0E:
        bcd_bytes = statement_table[i:i+6]
        bcd_value = decode_bcd(bcd_bytes)
        colorize(bcd_value, color=_colour_for_token('number'), end='')
        return 7

    # 0F (15) String Literal, Next byte is length
    if atok == 0x0F:
        strlen = statement_table[i]
        str_bytes = statement_table[i+1:i+1+strlen]
        colorize('"' + atascii_to_unicode_str(str_bytes) + '"', color=_colour_for_token('string'), end='')
        return strlen + 2

    is_func = atok >= 0x3D
    color_type = 'function' if is_func else 'command'

    # Other operators and functions
    colorize(op_func_string(atok), color=_colour_for_token(color_type), end='')
    return 1

def list_program(start=0, end=32767, abbrev=args_abbrev, colorify=None):
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

    # Global state during listing
    global list_colorify
    list_colorify = colorify if colorify else args_colorify

    # Loop through the program's lines
    while True:
        # The first two bytes are the little-endian line number
        lineno = getint(statement_table[i:i+2])
        if lineno > end: break

        # The next byte is the offset to the next line
        line_len = statement_table[i+2]
        nextline = i + line_len

        # Reached the first listing line yet?
        if lineno < start: i = nextline ; continue

        # Remember the index of the start of the line
        thisline = i

        # Print out the line number and detokenize the rest of the line
        #print(lineno, end=' ')

        if DEBUG_CODE: print(f"{{{line_len}}}", end='')
        colorize(f"{lineno}", color=_colour_for_token('number'), end=' ')

        # Skip to the first statement
        i += 3

        # Detokenize statements
        while i < nextline:
            # Get the statement offset from start of line
            st_off = statement_table[i] ; i += 1
            if DEBUG_CODE: print(f"{{{st_off}}}", end='')
            next_st = thisline + st_off

            # Get the command token
            cmd_tok = statement_table[i] ; i += 1
            if DEBUG_CODE: print(f"<{cmd_tok:02X}>", end='')

            # REM, DATA, ERROR
            is_fluff = cmd_tok in (kREM, kDATA, kERROR)
            tok_type = 'data' if is_fluff else 'command'

            # Print the command (if it's not "implied LET")
            tstr = string_for_command_token(cmd_tok, abbrev)
            if tstr != "": colorize(tstr, color=_colour_for_token(tok_type), end=qsp)

            # REM, DATA, ERROR
            if is_fluff:
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

    list_colorify = False

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

def skip_blanks(inbuf, index):
    """
    Scan the input text starting at the given index.
    Return the index of the first non-blank character.
    """
    while index < len(inbuf) and inbuf[index] == ' ':
        index += 1
    return index

import math

def get_line_number(inbuf:bytes, index: int):
    """
    Extract a numeric token from 'inbuf' starting at 'index'.

    The token may be an integer (e.g. "123") or a floating point number
    (e.g. "234.5").  The token ends at the first character that is not
    a digit or a decimal point.

    After extraction, 'index' is advanced to the position of that
    terminating character.  The numeric value is converted to a floor
    integer and stored in a two‑byte little‑endian bytearray.

    Parameters
    ----------
    inbuf : bytearray
        The source text.
    index : int
        Current position in 'inbuf'.

    Returns
    -------
    tuple[bytearray, int]
        The two‑byte little‑endian representation of the floored number
        and the updated index.
    """

    # Pull out the numeric token as a string
    start = index
    got_dot = False
    while index < len(inbuf):
        c = chr(inbuf[index])
        d = c.isdigit()
        #print(f"Scan for num ... {index} = {c} ({d})")
        is_dot = c == '.'
        if not (is_dot or c.isdigit()): break
        if is_dot:
            if got_dot: break
            got_dot = True
        index += 1

    # If nothing was read, return the default line number (0x00, 0x80)
    if index == start:
        return bytearray([0x00, 0x80]), index

    # Decode the numeric bytes into a string
    num_str = inbuf[start:index]
    try:
        num_val = float(num_str)
    except ValueError:
        # If parsing fails, fall back to the default
        return bytearray([0x00, 0x80]), index

    floored = math.floor(num_val)

    # Pack into a little‑endian two‑byte array
    line_num = bytearray([floored & 0xFF, (floored >> 8) & 0xFF])

    return line_num, index

def delete_line(line_no):
    """
    Delete a tokenized line from the Statement Table
    """
    # TODO:
    # - Scan the statement table to find the line with the given number.
    # - Get the length of that line.
    # - Contract the Statement Table to chop out the line.
    print(f"Deleting line {line_no}")
    pass

def tokenize_line(inbuff:bytes):
    """
    Tokenize a complete input line of AtariBASIC.

    Parameters
    ----------
    inbuff : bytes
        The raw text of the BASIC line to be tokenized.

    Returns
    -------
    dict
        A dictionary containing at least the parsed line number.
        If the line could not be tokenized, an empty dictionary is returned
        (you can change this to raise an exception or return a special
        "ERROR" token if you prefer).
    """

    # Reset the tokenized line buffer
    tokenized = bytearray()

    # Init the input and output indexes to zero
    cix, cox = 0, 0

    # Highest cix so far
    maxcix = 0

    # Direct statement?
    direct_flag = False

    # Saved name table output
    svontx, svontc, svvvte = 0, 0, 0

    # Point to the variable name table
    svvntp = 0 # (Offset into) variable_name_table

    # Skip leading blanks
    cix = skip_blanks(inbuff, cix)

    # Get the line number
    line_num, cix = get_line_number(inbuff, cix)

    # Init the tokenized line with the line num
    tokenized = line_num
    cox += 2

    # Line Number as an int
    line_val = line_num[0] + line_num[1] * 256

    print(f"[in={cix}, out={cox}] Stored Line Number {line_val}")

    # The original immediate trick is to use line 32768
    # so AtariBASIC programs are limited to 32768 lines.
    direct_flag = line_val >= 32768

    # Store a dummy line length that will be populated later
    tokenized.append(0)
    cox += 1

    print(f"[in={cix}, out={cox}] Appended dummy line len")

    # Skip following blanks
    cix = skip_blanks(inbuff, cix)

    # Remember the start of the statement for processing
    statement_start = cix

    print(f"[in={cix}, out={cox}] Skipped blanks")

    # Is the next character a CR?
    # If so we'll be deleting the given line number.
    c = inbuff[cix]
    if c == ATEOL:
        # Find the line in the statement table and delete it
        delete_line(line_val)
        return 101 # For now, 101 = continue on to the next line of input

    # Get the rest of the bytes and print their hex values
    while cix < len(inbuff):
        c = inbuff[cix]
        print(f"{cix} : {c}")
        cix += 1
    print("<end>")

    return

    # Dictionary for a Python level tokenized line
    line_dict = {}

    #
    # Process strategy:
    #
    # The original implementation scans bytes and does sub-calls
    # that also scan bytes, modifying the scan pointer as they
    # go along. So here we should follow that rather than use
    # split() and other modern Python tricks.
    # We also have to follow the BNF once we identify the command,
    # fall back to implied LET, or fall back to error.
    #
    # So, when we grab the line number we must advance the input index.
    # Once we have the command we test each BNF connected by _OR
    # and dive down the BNF rabbit hole. Some BNF are just char match,
    # while others use callbacks for tests. We advance the input as we go,
    # but can fall back when subitems fail.
    #


    # -------------------------------------------------------------
    # Step 1. Grab the line number (if present)
    #   ^\s*     – any leading spaces (already stripped, but harmless)
    #   (\d+)    – one or more digits
    #   \b       – word boundary (ensures we stop at the first non‑digit)
    line_no_match = re.match(r'^(\d+)\b', inbuff)

    if line_no_match:
        # Has a line number
        line_no = int(line_no_match.group(1))   # convert to an integer
        line_dict['line'] = line_no
    else:
        # Immediate Line has number 32768. (Too bad it's not "0" so we could have 65535 lines.)
        line_dict['line'] = 32768

    return line_dict

def tokenize_and_apply_line(inbuff:bytes):
    """
    Tokenize a complete input line of AtariBASIC and apply it.
    - Lines with no number are executed right away (as if they were the last line in the program).
    - Lines with only a number cause the line with matching number to be deleted from the program.
    - Lines with a number and one or more statements are added to the program.
    """
    tokenized_line = tokenize_line(inbuff)
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
        list_program(abbrev=abbrev)
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
            line_str = input() # String
        except (KeyboardInterrupt, EOFError) as exc:
            if isinstance(exc, KeyboardInterrupt):
                # Ctrl‑C logic
                print(" BREAK")
                continue
            else:
                # Ctrl‑D logic
                print()
                break

        cmd = line_str.strip().upper()
        if cmd in ("QUIT", "EXIT", "Q"):
            print("\nBye!\n")
            break

        cmd_name = cmd.split(' ')[0]
        handler = COMMAND_HANDLERS.get(cmd_name)
        if handler:
            handler()
            nowReady = True

        elif line_str != "":
            # Let's just dive into tokenizing the line
            # Convert from UTF-8 bytes to ATASCII bytes and append ATEOL
            line_bytes = unicode_to_atascii_str(bytes(line_str, "utf-8")) + bytes([ATEOL])
            tokenize_and_apply_line(line_bytes);

            #print(f"Unknown command: {cmd_name!r}.  Valid commands are: {', '.join(COMMAND_HANDLERS)}")

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
    # Requires Python 3
    if sys.version_info[0] < 3:
        print("This script requires Python 3")
        sys.exit(1)

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
    parser.add_argument('-c', '--clist', action="store_true", help='List the colorized program and exit.')
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
        if args.clist: parser.error("The '-c/--clist' option requires an input file.")
        if args.tvars: parser.error("The '-t/--tvars' option requires an input file.")

    # Display listings with abbreviated keywords, minimal whitespace
    global args_abbrev
    if args.abbrev: args_abbrev = True

    # Display listings in a structured format
    global args_struct
    if args.struct: args_struct = True

    # Display listings in color
    global args_colorify
    if args.clist:
        args.list = True
        args_colorify = True

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
            list_program(abbrev=args_abbrev)
            print()
            exit(0)
        elif args.tvars:
            print_variable_tables()
            exit(0)

    interactive_mode()

if __name__ == "__main__":
    main()
