#!/usr/bin/env python3
#
# basic_util.py
# Functions based on the original BASIC assembler sources
# that deal with program data.
#

CIX = 0
COX = 0
DIRFLG = False
OUTBUFF = []
LBUFF = ""

MAXCIX = 0 # Value of CIX where a syntax error was detected
SVVNTP = 0 # Saved VNT Pointer to restore in case of a syntax error
SVVVTE = 0 # Number of new vars in the current line

# EXPAND / CONTRACT
# Used to expand and contract the Statement Table, Variable Tables, etc.
def util_EXPAND(offset, size):
    pass
def util_CONTRACT(offset, size):
    pass

# EXECNL - Execute Next Line
def util_EXECNL():
    # Set up state via SETLN1
    # Fall through (so to speak) to EXECNS
    pass

# EXECNS - Execute Next Statement
def util_EXECNS():
    # - If BREAK is pressed do STOP
    # - Get offset to next statement line NXTSTD
    # - At end of line? Done if direct command (i.e., 32768).
    # - Go to next line via GNXTL.
    # - Test for end of statement table via TENDST.
    # - Execute Next Line via EXECNL, of return up to loop.
    pass

# GNXTL - Advance STMCUR by A bytes
def util_GNXTL(A):
    STMCUR += A

# GETSTMT - Get Statement in Statement Table
# - Save current line address STMCUR in SAVCUR.
# - Search for statement that has line number TSLNUM.
# - Set STMCUR to point to it if found.
#   or to where it would go if not found.
# - Return True if not found, false if found.
def util_GETSTMT(TSLNUM):
    pass

# TENDST - Test End of Statement Table
def util_TENDST():
    pass

# TESTBRK - Test for Break
def util_TESTBRK():
    # We can get as low level as this:
    #Y = 0xFF if BRKBYTE == 0 else 0x00
    #A = Y
    # But the effective logic is:
    return BRKBYTE == 0

# SKIPBLANK ($DBA1) - Skip blanks in the parser input buffer.
def util_SKIPBLANK():
    while LBUFF[CIX] in (' ', '\t'):
        CIX += 1

# SETCODE ($A2C8) - Set a code in the OUTBUFF and increment COX
def util_SETCODE(T):
    OUTBUFF[COX] = T
    COX += 1
    pass

# SEARCH ($A462)
# - Match the input to a statement in the Statement Name Table
#   - Unrecognized statements are assumed to be Implied Let.
# - Set STENNUM to the statement number, if found.
# - Add the statement token to OUTBUFF. Remember this token's position (address) in SRCADR.
#
def util_SEARCH():
    pass


#
# Syntax Primitives
#

#
# Program Editor Vars
#
# INBUFF ($F3-$F4) - The pointer to LBUFF used for 6502 indexing. Not used here.
# LBUFF ($580) - Input Buffer for the text line. (A line over 128 bytes will step on Page 6.)
# OUTBUFF - Output Buffer for a tokenized line that may be inserted into the Statement Table.
# CIX ($F2) - Index into the user input line in the line buffer (LBUFF)
# COX ($94) - Index into the output buffer (OUTBUFF)
# DIRFLG ($A6) - Direct input line flag (input line has no number, use pseudo-line 32768)
# MAXCIX ($9F) - Value of CIX where a syntax error was detected.
# SVVNTP ($AD-$AE) - Saved VNT Pointer to restore in case of a syntax error.
#                    More memory-efficient on 6502 to add vars and revert on error, rather than
#                    maintain a buffer of new vars to add when the line is done processing.
# SVVVTE ($B1) - Number of new vars in the line. Init to 0 before processing the line.
#

#
# The Line Parsing Process
#
# A Program is composed of numbered lines containing one or more statements (STMT).
# A statement consists of a full or abbreviated command followed by one or more parameters.
#
# Line Parsing:
# - Skip over blanks
# - Process the Line Number.
#   The line number is processed as a float before rounding to 16 bit little-endian integer.
#   Line int is stored in TSLNUM and added to OUTBUFF. No line number? Set DIRFLG and line 32768.
# - Add a placeholder byte in OUTBUFF for the offset to next line, to be populated later.
# - Do SKIPBLANKS then save CIX in STMSTRT to remember the start of the first statement.
#   This can be used later if there is a syntax error in the line.
# - Nothing but a line number? Delete the line with that number, if any, using util_CONTRACT.
#
# Statement Parsing:
# An input line consists of one or more statements. For each statement:
# - Init, storing COX in STMLBD. This index in OUTBUFF will contain the offset to next statement byte.
# - Add a placeholder byte to OUTBUFF for the offset to next statement, to be populated later.
# - Recognize the Statement Name
#   - Skip input blanks and process the statement name with util_SEARCH.
#   - A bad statement/variable name is a syntax error. (It must start with a letter.)
# - Transfer control to the pre-compiler, which:
#   - Places the appropriate tokens in OUTBUFF,
#   - Increments CIX, COX to next locations.
#   - Returns a flag to indicate if there was an error.
# - If a Syntax Error is detected:
#   - The editor gets back a flag indicating the error.
#   - MAXCIX will contain the CIX offset into LBUFF where the error was detected. Invert this character.
#   - Set the flag 0x40 in DIRFLG to indicate the error. (0x80 indicates Direct Statement.)
#   - Set CIX = STMSTRT
#   - Set STMLBD to indicate the location of the first Statement Length byte in OUTBUFF.
#   - Set COX to the index of the first statement token in OUTBUFF, replace with cERR.
#   - Copy the entire line after the line number from INBUFF to OUTBUFF. (Later OUTBUFF will be inserted into the Statement Table.)
#
# - Final Statement Processing (for both good and erroneous lines):
#   - Set OUTBUFF[STMLBD] = COX to populate the Next Statement offset.
#   - Check whether the next character is CR.
#     - If not, go on to process the next statement.
#     - If so...
#       - Set OUTBUFF[2] = COX to populate the Next Line Offset.
#       - Insert or Replace the line in the Statement Table.
#
# Inserting / Adding to the Statement Table
# - Determine where in the Statement Table the new line will go:
#   - A new line will always expand the buffer, except when it's the last line.
#   - A replacement line will expand or contract the Statement Table space as-needed.
#   - (Our Python implementation of the Statement Table is an array indexed by line number,
#     and we only synthesize a contiguous buffer for SAVE.)
# - Copy OUTBUFF to the allocated space in the Statement Table.
#
# Line Wrap-up
# After the line has been added to the Statement Table the editor checks DIRFLG for the
# syntax error indicator (0x40).
#
# Error Wrap-up
# - If there is an error:
#   - Any variables added by the line are removed. i.e., The VNT and VVT are contracted.
#   - The editor lists the line, which will show "ERROR -  " and the raw text of the line
#     with the character at the error index inverted.
#
# Handling Correct Lines
# - If the line was syntactically okay, the editor checks DIRFLG flag 0x80 for immediate input.
# - If the line was not immediate then the editor continues to accept input.
# - An immediate line will have been assigned line # 32768 and added to the end of the table.
#   During editing STMCUR ($8A-$8B) points to this line.
# - The editor transfers control to Execution Control ($A95F) to execute the direct statement,
#   which may have a GOTO, GOSUB, TRAP, etc. that jumps into the stored program.
#

#
# Syntax Tables for the Atari BASIC Syntaxer
#
# Each command has an expected syntax that we can enforce using the 81 ABML rules
# tables below.
#
# At bottom the rulesets state exactly what kind of tokens are allowed at each
# successive position in a statement after the command token. As soon as an allowed
# token's pattern is identified it passes the rule being tested.
#
# Development Plan:
# - Fill in all the rules as ABML.
# - Build the ABML processor that emulates the original validation + tokenizing process
#   using a central loop that does all the logic and dispatching. The loop can check if a
#   rule element is a function ref, a ref to another rule, a rule token, an operator/function
#   token, etc. It would maintain the expression stack, "back up" when needed, modify chars,
#   and deal with rules calling sub-rules and recursing.
# - Also translate the 81 rulesets into Python function calls using language features to
#   accomplish the same process. e.g., ABML "cUSR" becomes a direct call to f_SRCONT(cUSR) to
#   check if the input string matches the name of that function, and if it does, advance CIX,
#   and insert the token into the OUTBUFF, and return 'pass'. ABML "cPLUS CHNG cUPLUS" becomes
#   f_CHNG(cPLUS, cUPLUS) to check for match and apply the change.
#

#
# Atari BASIC Meta-Language (ABML) Commands
# ABML is derived from BNF.
#  - 0x00..0x0F : Commands
#  - 0x10..0x7F : Operator and Function Tokens
#  - 0x80..0xBF : Relative Non-Terminal Vectors - shorter than ANTV
#
#  - JS(N) Make a Relative Non-Terminal Vector for rule N
#  - AD(N) Make a Word Address for rule N following an ABML Command such as ANTV
#
# Validating syntax consists of checking the input against the rules,
# where each rule reports pass/fail. Higher level rules use lower level rules.
# Rules are similar to regexes in being used for matching and capturing.
# When one rule fails, keep trying the next 'OR' rule, until all rules have failed.
# Each symbol in the input must pass before the next is checked. The first character
# being checked or changed in a rule is the last character added to OUTBUFF, so, e.g.,
# a USR rule needs to match a cUSR token. The deepest sort of rule checks for a valid
# character, such as a letter, digit, or operator.
# When the "or nothing" rule is passed we do not advance to the next rule symbol.
#
# Backing Up
# Sometimes we need to back up over symbols already processed so we
# can check the next rule after a fail. The rule caller must remember
# the current position before checking sub-conditions so it can back up to
# check the next rule.
#
# Location of Syntax Error
# Highlight the character after the rightmost valid symbol during various back-ups.
#
kANTV = 0x00 # Absolute Non-Terminal Vector (ANTV) to sub-call another rule
kESRT = 0x01 # External Subroutine Call (ESRT) to call a handler for more complex rules
kOR   = 0x02 # ABML or
kRTN  = 0x03 # (aka <END>) Return, marks the end of an ABML rule. Return pass or fail.
kVEXP = 0x0E # (aka <EXP>) Expression Non-Terminal Vector. Shorthand for ANTV AD(EXP)
kCHNG = 0x0F # Change Last Token to X. e.g., to rectify '=' as assign or compare.

# For this adaptation we'll use direct references to other rules in place of ANTV, VEXP, and ESRT.
# So 'ANTV.AD(RULE)' and 'ESRT.AD(RULE)' become 's_RULE' and 'VEXP' becomes 's_EXP'.
# When converting to code use 'f_RULE()' and 'f_EXP()',
#   'OR' => 'or', 'RTN' => 'return False'. Empty rule 'OR RTN' permits NADA.

# Search ONT
# SRCONT: Check if the current symbol matches the terminal symbol represented by the current operator token (according to the Operator Name Table).
#         For example, if the rule specifies cCHR this checks whether the input matches "CHR$".
#         On match, add the token to OUTBUFF and return 'pass'. Else return 'fail'.
def f_SRCONT(): pass

# TNVAR: Examine the current source symbol for a numeric variable. Array var names end with '('.
#        On success create the var, put the var token in OUTBUFF, return 'pass'. Else return 'fail'.
def f_TNVAR(): pass

# TSVAR: Examine the current source symbol for a string variable. String var names end with '$'.
#        On success create the var, put the var token in OUTBUFF, return 'pass'. Else return 'fail'.
def f_TSVAR(): pass

# TNCON: Examine the current source symbol for a numeric constant (e.g., using the floating point package).
#        On success put the 6-byte BCD value in OUTBUFF and return 'pass'. Else return 'fail'.
def f_TNCON(): pass

# TSCON: Examine the current source symbol for a string constant. $9B and $22 chars disallowed.
#        On success put the string token, len, and chars into OUTBUFF and return 'pass'. Else return 'fail'.
def f_TSCON(): pass

# When the stack is reset, actually reset, to $FF it erases all the accumulated JSRs so it can carry on in
# the program main loop, which we know to be the top level. To do the same we might be able to use try/catch.

# EIF : Continue with statement processing after IF ... THEN
#       Reset stack, set statement length byte in OUTBUFF, continue at statement after THEN...
#       This causes the Syntaxer to break out of the rule and go to the start of the statement
#       processor where we get the Command token.
def f_EIF(): pass

# XDATA : Copy the rest of the line up to CR into OUTBUFF advancing CIX, COX, set the statement length, set line length, then go on to the next line.
def f_XDATA(): pass

# EREM : Reset the stack, copy the rest of the line up to CR into OUTBUFF advancing CIX, COX, set the statement length, set line length, then go on to the next line.
#        In the original code EREM/EDATA jumps to 'XDATA' in middle of 'SYNTAX' where data copying up to CR happens just ahead of the statement/line loop logic.
#        The nearest thing in our implementation is to update the core state machine.
def f_EREM(): pass
f_EDATA = f_EREM

# <EXP> = (<EXP>)<NOP> | <UNARY><EXP> | <NV><NOP> <END>
# ABML: cLPRN CHNG cALPRN JS(EXP) cRPRN JS(NOP) OR JS(UNARY) JS(EXP) OR JS(NV) JS(NOP) RTN
s_EXP = (
    cLPRN, kCHNG, cALPRN, s_EXP, cRPRN, s_NOP, kOR,
    s_UNARY, s_EXP, kOR,
    s_NV, s_NOP, kRTN)

# Defining s_EXP as a function, we get:
#   def f_EXP():
#       return (f_SRCONT(cLPRN) and f_CHNG(cALPRN) and f_EXP() and f_SRCONT(cRPRN) and f_NOP())
#           or (f_UNARY() and f_EXP())
#           or (f_NF() and f_NOP())
#
# NOTE: Each condition test requires rewinding on fail so the next test can start from the same place.
#

# Unary Plus or Minus Sign, or NOT operator.
# <UNARY> = + | - | NOT <END>
# ABML: cPLUS CHNG cUPLUS OR cMINUS CHNG cUMINUS OR cNOT RTN
s_UNARY = (cPLUS, kCHNG, cUPLUS, kOR, cMINUS, kCHNG, cUMINUS, kOR, cNOT, kRTN)

# Defining s_UNARY as a function, we get:
#   def f_UNARY():
#       return (f_SRCONT(cPLUS) and f_CHNG(cUPLUS))
#           or (f_SRCONT(cMINUS) and f_CHNG(cUMINUS))
#           or f_SRCONT(cNOT)
#

# Numeric Value as a Function, Variable, Numeric Constant, or String Compare.
# <NV> = <NFUN> | <NVAR> | <NCON> | <STCOMP> <END>
# ABML: JS(NFUN) OR JS(NVAR) OR ESRT.AD(TNCON) OR ANTV.AD(STCOM) RTN
s_NV = (s_NFUN, kOR, s_NVAR, kOR, f_TNCON, kOR, s_STCOMP, kRTN)

# Defining s_NV as a function, we get:
#   def f_NV():
#       return f_NFUN() or f_NVAR() or f_TNCON() or f_STCOMP()

# Next Operator and Expression. May be empty.
# <NOP> = <OP> <EXP> | <NADA> <END>
# ABML: JS(OP) JS(EXP) OR RTN
s_NOP = (s_OP, s_EXP, kOR, kRTN)

# Defining s_NOP as a function, we get:
#   def f_NOP():
#       return (f_OP() and f_EXP()) or f_NADA()

# Operator (Math)
# <OP> = ** | * | / | <= | >= | <> | < | > | = | AND | OR <END>
# ABML: cEXP OR cPLUS OR cMINUS OR cMUL OR cDIV OR cLE OR cGE OR cNE OR cLT OR cGT OR cEQ OR cAND OR cOR OR RTN
s_OP = (cEXP, kOR, cPLUS, kOR, cMINUS, kOR, cMUL, kOR, cDIV, kOR, cLE, kOR, cGE, kOR, cNE, kOR, cLT, kOR, cGT, kOR, cEQ, kOR, cAND, kOR, cOR, kOR, kRTN)

# Numeric Variable
# <NVAR> = <TNVAR> <NMAT> <END>
# ABML: ESRT.AD(TNVAR) JS(NMAT) RTN
s_NVAR = (f_TNVAR, s_NMAT, kRTN)

# Parentheses wrapping One or more comma-delimited Mathematical expressions
# <NMAT> = ( <EXP> <NMAT2> ) | <NADA> <END>
# ABML: cLPRN CHNG cALPRN VEXP JS(NMAT2) cRPRN OR RTN
s_NMAT = (cLPRN, kCHNG, cALPRN, s_EXP, s_NMAT2, cRPRN, cOR, kRTN)
# Continuation of comma-delimited Mathematical expressions
# <NMAT2> = , <EXP> | <NADA> <END>
# ABML: cCOM CHNG cACOM VEXP OR RTN
s_NMAT2 = (cCOM, kCHNG, cACOM, s_EXP, kOR, kRTN)

# Numeric Function (cATN <= c <= cSGN). May take a number, a string, or be a USR call.
# <NFUN> = <NFNP> <NFP> | <NFSP> <SFP> | <NFUSR> <END>
# ABML: cNFNP JS(NFP) OR ANTV.AD(NFSP) JS(SFP) OR JS(NFUSR) RTN
s_NFUN = (cNFNP, s_NFP, cOR, s_SFP, cOR, s_NFUSR, kRTN)

# Defining s_NFUN as a function, we get:
#   def f_NFUN():
#       return (f_SRCONT(cNFNP) and f_NFP())
#           or f_NFSP()
#           or f_NFUSR()

# USR Function taking one or more numeric expressions. Changes previous '(' token 0x2B to 0x3A.
# <NFUSR> = USR ( <PUSR> ) <END>
# ABML: cUSR cLPRN CHNG cFLPRN ANTV.AD(PUSR) cRPRN RTN
s_NFUSR = (cUSR, cLPRN, kCHNG, cFLPRN, s_PUSR, cRPRN, kRTN)

# Function taking one expression. Changes previous '(' token 0x2B to 0x3A.
# <NFP> = ( <EXP> ) <END>
# ABML: cLPRN CHNG cFLPRN VEXP cRPRN RTN
s_NFP = (cLPRN, kCHNG, cFLPRN, s_EXP, cRPRN, kRTN)

# Function taking one string. Changes previous '(' token 0x2B to 0x37.
# <SFP> = <STR>) <END>
# ABML: cLPRN CHNG cFLPRN JS(STR) cRPRN RTN
s_SFP = (cLPRN, kCHNG, cFLPRN, s_STR, cRPRN, kRTN)

# String comparison
# <STCOMP> = <STR><SOP><STR> <END>
# ABML: JS(STR) JS(SOP) JS(STR) RTN
s_STCOMP = (s_STR, s_SOP, s_STR, kRTN)

# String function, var, or constant, else fail
# <STR> = <SFUN> | <SVAR> | <SCON> <END>
# ABML: JS(SFUN) OR JS(SVAR) OR ESRT.AD(TSCON) RTN
s_STR = (s_SFUN, kOR, s_SVAR, kOR, s_TSCON, kRTN)

# String Function
# <SFUN> = SFNP <NFP> <END>
# ABML: ANTV.AD(SFNP) JS(NFP) RTN
s_SFUN = (s_SFNP, s_NFP, kRTN)

# String Variable
# <SVAR> = <TSVAR> <SMAT> <END>
# ABML: ESRT.AD(TSVAR) JS(SMAT) RTN
s_SVAR = (f_TSVAR, s_SMAT, kRTN)
# <SMAT> = (<EXP><SMAT2>) | <NADA> <END>
# ABML: cLPRN CHNG cALPRN VEXP JS(SMAT2) cRPRN OR RTN
s_SMAT = (cLPRN, kCHNG, cALPRN, s_EXP, s_SMAT2, cRPRN, kOR, kRTN)
# <SMAT2> = ,<EXP> | <NADA> <END>
# ABML: cCOM CHNG cACOM VEXP OR RTN
s_SMAT2 = (cCOM, kCHNG, cACOM, s_EXP, kOR, kRTN)

# String operator - Convert compare opss to string compares
# <SOP> =
#   <= cLE :CHNG cSLE <OR>
#   <> cNE :CHNG cSNE <OR>
#   <  cLT :CHNG cSLT <OR>
#   >  cGT :CHNG cSGT <OR>
#   <  cGT :CHNG cSGT <OR>
#   <  cEQ :CHNG cSEQ <END>
# ABML: cLE CHNG cSLE OR cNE CHNG cSNE OR cLT CHNG cSLT OR cGT CHNG cSGT OR cEQ CHNG cSEQ RTN
s_SOP = (
    cLE, kCHNG, cSLE, kOR,
    cNE, kCHNG, cSNE, kOR,
    cLT, kCHNG, cSLT, kOR,
    cGT, kCHNG, cSGT, kOR,
    cEQ, kCHNG, cSEQ, kRTN)

# ========================================

# PUT Command with Device #, Comma, Numeric Parameter
# <PUT> = <D1> , <EXP> <EOS> <END>
# ABML: cPND VEXP cCOM ... VEXP ... JS(EOS) RTN
s_PUT = (cPND s_EXP cCOM s_EXP s_EOS kRTN)

# Statements with one Numeric Parameter
# <GOTO> = <EXP> <EOS> <END>
# ABML: VEXP ... JS(EOS) RTN
s_GR = (s_EXP s_EOS kRTN)
s_TRAP, s_GOTO, s_GOSUB, s_COLOR = s_GR, s_GR, s_GR, s_GR

# Statements with no parameters
# <DOS> = <EOS> <END>
# ABML: JS(EOS) RTN
s_RAD = (s_EOS kRTN)
s_CSAVE, s_CLOAD, s_DOS, s_CLR, s_RET, s_END, s_STOP, s_POP, s_NEW, s_BYE, s_CONT, s_DEG = s_RAD, s_RAD, s_RAD, s_RAD, s_RAD, s_RAD, s_RAD, s_RAD, s_RAD, s_RAD, s_RAD, s_RAD

# ========================================

# LET and Implied LET. Numeric or String.
# <LET> = <NVAR> = <EXP> <EOS> | <SVAR> = <STR> <EOS> <END>
# ABML: ANTV.AD(NVAR) cEQ CHNG cAASN VEXP JS(EOS) OR ANTV.AD(SVAR) cEQ CHNG cSASN ANTV.AD(STR) JS(EOS) RTN
s_LET = (
    s_NVAR, cEQ, kCHNG, cAASN, s_EXP, s_EOS, kOR,
    s_SVAR, cEQ, kCHNG, cSASN, s_STR, s_EOS, kRTN)
s_ILET = s_LET

# FOR Statement
# <FOR> = <TNVAR> = <EXP> TO <EXP> <FSTEP> <EOS> <END>
# ABML: ESRT.AD(TNVAR) cEQ CHNG cAASN VEXP cTO VEXP JS(FSTEP) JS(EOS) RTN
s_FOR = (f_TNVAR, cEQ, kCHNG, cAASN, s_EXP, cTO, s_EXP, s_FSTEP, s_EOS, kRTN)
# STEP with Numeric Expression at the end of a FOR Statement
# <FSTEP> = STEP <EXP> | <NADA>
# ABML: cSTEP VEXP OR RTN
s_FSTEP = (cSTEP, s_EXP, kOR, kRTN)

# LOCATE Statement
# <LOCATE> = <EXP> , <EXP> , <TNVAR> <EOL> <END>
# ABML: VEXP cCOM VEXP cCOM JS(NEXT) RTN
s_LOCATE = (s_EXP, cCOM, s_EXP, cCOM, s_NEXT, kRTN)

# GET Statement - Requires Device #, comma, Variable Name
# <GET> = <D1> , <TNVAR> <END>
# ABML: JS(D1) cCOM ... ESRT.AD(TNVAR) JS(EOS) RTN
s_GET = (s_D1, cCOM, f_TNVAR, s_EOS, kRTN)

# NEXT Statement - Requires Variable Name
# <NEXT> = <TNVAR> <EOS> <END>
# ABML: ESRT.AD(TNVAR) JS(EOS) RTN
s_NEXT = (f_TNVAR, s_EOS, kRTN)

# ========================================
# RESTORE Statement with Optional Line Number
# <RESTORE> = <EXP> <EOS> | <EOS> <END>
# ABML: VEXP JS(EOS) OR JS(EOS) RTN
s_RESTORE = (s_EXP, s_EOS, kOR, s_EOS, kRTN)

# ========================================
# INPUT Statement with Optional Prompt and One or More Variables
# <INPUT> = <OPD> <READ> <END>
# ABML: JS(OPD) JS(NSVRL) JS(EOS) RTN
s_INPUT = (s_OPD, s_NSVRL, s_EOS, kRTN)

# ========================================
# READ Statement with One or More Variables
# <READ> = <NSVARL> <EOS> <END>
# ABML: JS(NSVRL) JS(EOS) RTN
s_READ = (s_NSVRL, s_EOS, kRTN)

# ========================================
# End of Statement or End of Line
# <EOS> = : | CR <END>
# ABML: cEOS OR cCR RTN
s_EOS = (cEOS, kOR, cCR, kRTN)
s_EOS2 = s_EOS # Identical to EOS

# ========================================
# PRINT Statement with optional Device # and Delimited Values
# <PRINT> = <D1> <EOS> | <D1> <PR1> <EOS> <END>
# ABML: JS(D1) JS(EOS) OR JS(OPD) ANTV.AD(PR1) JS(EOS) RTN
s_PRINT = (s_D1, s_EOS, kOR, s_D1, s_PR1, s_EOS, kRTN)
# LPRINT Statement with optional Delimited Values
# <LPRINT> = <PR1> <EOS> <END>
# ABML: ANTV.AD(PR1) JS(EOS) RTN
s_LPRINT = (s_PR1, s_EOS, kRTN)

# ========================================
# Device ID
# <D1> = <cPND> <EXP> <END>
# ABML: cPND VEXP RTN
s_D1 = (cPND, s_EXP, kRTN)

# ========================================
# Numeric or String Variable
# <NSVAR> = <NVAR> | <SVAR> <END>
# ABML: ESRT.SD(TNVAR) OR ESRT.AD(TSVAR) RTN
s_NSVAR = (f_TNVAR, kOR, f_TSVAR, kRTN)

# Numeric and/or String Variable List
# <NSVRL> = <NSVAR> <NSV2> | <NADA> <END>
# ABML: JS(NVAR) JS(NSV2) OR RTN
s_NSVRL = (s_NSVAR, s_NSV2, kOR, kRTN)
# Continuation of comma-delimited Numeric and/or String Variables
# <NSV2> = ,<NSVRL> | <NADA> <END>
# ABML: cCOM JS(NSVRL) OR RTN
s_NSV2 = (cCOM, s_NSVRL, kOR, kRTN)

# ========================================
# XIO Command Arguments
# Example: XIO 18,#6,0,0,"S:"
# <XIO> = <EXP> , <D1> , <EXP> , <EXP> , <FS> <EOS> <END>
# ABML: VEXP cCOM JS(D1) cCOM JS(TEXP) cCOM JS(FS) JS(EOS) RTN
s_XIO = (s_EXP, cCOM, s_D1, cCOM, s_TEXP, cCOM, s_FS, s_EOS, kRTN)

# ========================================
# OPEN Command Arguments
# Example: OPEN #1,4,0,"K:"
# <OPEN> = <D1> , <EXP> , <EXP> , <FS> <EOS> <END>
# ABML: JS(D1) cCOM JS(TEXP) cCOM JS(FS) JS(EOS) RTN
s_OPEN = (s_D1, cCOM, s_TEXP, cCOM, s_FS, s_EOS, kRTN)

# ========================================
# CLOSE Command Arguments
# <CLOSE> = <D1> <EOS> <END>
# ABML: JS(D1) JS(EOS) RTN
# Example: CLOSE #1
s_CLOSE = (s_D1, s_EOS, kRTN)

# ========================================
# ENTER, LOAD, SAVE Command Arguments
# <LOAD> = <FS><EOS> <END>
# ABML: JS(FS) JS(EOS) RTN
s_ENTER = (s_FS, s_EOS, kRTN)
s_LOAD, s_SAVE = s_ENTER, s_ENTER

# ========================================
# RUN Command Arguments
# <RUN> = <FS> <EOS> | <EOS> <END>
# ABML: JS(FS) JS(EOS) OR JS(EOS) RTN
s_RUN = (s_FS, s_EOS, kOR, s_EOS, kRTN)

# ========================================
# Optional Device ID argument
# <OPD> = <D1>, | <NADA> <END>
# ABML: JS(D1) cCOM OR RTN
s_OPD = (s_D1, cCOM, kOR, kRTN)

# ========================================
# List Command Arguments
# <LIST> = <FS> : | <FS> , <LIS> | <LIS> <END>
# (But original source has "<LIST> = <FS>;<L2> | <L2> <END>")
# ABML: JS(FS) JS(EOS) OR JS(FS) cCOM JS(LIS) OR JS(LIS) RTN
# Examples:
#   LIST "C:PROGRAM.LST"
#   LIST "C:PROGRAM.LST", 30, 80
#   LIST 20
#   LIST
s_LIST = (s_FS, s_EOS, kOR, s_FS, cCOM, s_LIS, kOR, s_LIS, kRTN)

# Zero, One, or Two Line Numbers and End of Statement
# <LIS> = <L1> <EOS> <END>
# ABML: ANTV.AD(L1) JS(EOS) RTN
s_LIS = (s_L1, s_EOS, kRTN)

# ========================================
# STATUS Command Complete Statement
# <STATUS> = <STAT> <EOS> <END>
# ABML: JS(STAT) JS(EOS) RTN
s_STATUS = (s_STAT, s_EOS, kRTN)
# Status command arguments
# <STAT> = <D1> , <NVAR> <END>
# ABML: JS(D1) cCOM JS(NVAR) RTN
s_STAT = (s_D1, cCOM, s_NVAR, kRTN)

# ========================================
# NOTE, POINT Complete Statement
# <NOTE> = <STAT> , <NVAR> <EOS> <END>
# ABML: JS(STAT) cCOM ANTV.AD(NVAR) JS(EOS) RTN
s_NOTE = (s_STAT, cCOM, s_NVAR, s_EOS, kRTN)
s_POINT = s_NOTE

# ========================================
# Filestring is just a String
# <FS> = <STR>
# ABML: JS(STR) RTN
s_FS = (s_STR, kRTN)

# ========================================
# TEXP : Two Expressions Rule
# <TEXP> = <EXP> , <EXP> <END>
# ABML: VEXP cCOM VEXP RTN
s_TEXP = (s_EXP, cCOM, s_EXP, kRTN)

# ========================================
# SOUND Command Complete Statement (4 Numeric Expressions)
# <SOUND> = <EXP> , <EXP> , <EXP> , <EXP> <EOS> <END>
# ABML: VEXP cCOM VEXP cCOM VEXP cCOM VEXP JS(EOS) RTN
s_SOUND = (s_EXP, cCOM, s_EXP, cCOM, s_EXP, cCOM, s_EXP, s_EOS, kRTN)

# ========================================
# SETCOLOR Command Complete Statement (3 Numeric Expressions)
# <SETCOLOR> = <EXP> , <EXP> , <EXP> <EOS> <END>
# ABML: VEXP cCOM VEXP cCOM VEXP JS(EOS) RTN
s_SETCOLOR = (s_EXP, cCOM, s_EXP, cCOM, s_EXP, s_EOS, kRTN)

# ========================================
# POKE, PLOT, POSITION, DRAWTO Complete Statement (2 Numeric Expressions)
# <POKE> = <EXP> , <EXP> <EOS> <END>
# ABML: JS(TEXP) JS(EOS) RTN
s_POKE = (s_TEXP, s_EOS, kRTN)
s_PLOT, s_POS, s_DRAWTO = s_POKE, s_POKE, s_POKE

# ========================================
# DIM, COM Complete Statement (Zero or more dimensioned vars)
# (Yes, DIM and COM can have zero vars.)
# <DIM> = <NSML> <EOS> <END>
# ABML: JS(NSML) JS(EOS) RTN
s_DIM = (s_NSML, s_EOS, kRTN)
s_COM = s_DIM

# ========================================
# ON : ON...GOTO/GOSUB Complete Statement
# <ON> = <EXP> <ON1> <EXPL> <EOS> <END>
# ABML: VEXP JS(ON1) JS(EXPL) JS(EOS) RTN
s_ON = (s_EXP, s_ON1, s_EXPL, s_EOS, kRTN)
# <ON1> = GOTO | GOSUB <END>
# ABML: cGOTO OR cGOSUB RTN
s_ON1 = (cGOTO, kOR, cGOSUB, kRTN)
# EXPL : Expression List
# <EXPL> = <EXP> <EXPL1> <END>
# ABML: VEXP JS(EXPL1) RTN
s_EXPL = (s_EXP, s_EXPL1, kRTN)
# EXPL1 : Expression List Continuation
# <EXPL1> = , <EXPL> | <NADA> <END>
# ABML: cCOM JS(EXPL) OR RTN
s_EXPL1 = (cCOM, s_EXPL, kOR, kRTN)

# ========================================
# EOS : End of Statement or End of Line
# <EOS> = CEOS | CCR <END>
# ABML: cEOS OR cCR RTN
s_EOS = (cEOS, kOR, cCR, kRTN)

# ========================================
# String or Array Definition - Used for DIM / COM.
# (Quirk: DIM/COM with zero items will tokenize but not run.)
# <NSMAT> = <TNVAR> ( <EXP> <NSMAT2> ) | <TSVAR> ( <EXP> ) <END>
#   (Original code just has: "<NSMAT> = <TNVAR> ( <EXP> <NSMAT2> )")
# ABML: ESRT.AD(TNVAR) cLPRN CHNG cDLPRN VEXP ANTV.AD(NMAT2) cRPRN OR ESRT.AD(TSVAR) cLPRN CHNG cDSLPR VEXP cRPRN RTN
s_NSMAT = (
    f_TNVAR, cLPRN, kCHNG, cDLPRN, s_EXP, s_NMAT2, cRPRN, kOR,
    f_TSVAR, cLPRN, kCHNG, cDSLPR, s_EXP, cRPRN, kRTN)

# Zero or More String/Array Definitions
# <NSML> = <NSMAT> <NSML2> | <NADA> <END>
# ABML: JS(NSMAT) JS(NSML2) OR RTN
s_NSML = (s_NSMAT, s_NSML2, kOR, kRTN)
# Another Numeric Array Variable with Dimension(s) or Nothing
# <NSML2> = , <NSML> | <NADA> <END>
# ABML: cCOM JS(NSML) OR RTN
s_NSML2 = (cCOM, s_NSML, kOR, kRTN)

# IF ... THEN ... Complete Statement
# <IF> = <EXP> THEN <IFA> <EOS> <END>
# ABML: VEXP cTHEN JS(IFA) JS(EOS) RTN
s_IF = (s_EXP, cTHEN, s_IFA, s_EOS, kRTN)
# Following THEN, a Line Number expression or a Statement
# See special notes about EIF.
# <IFA> = <TNCON> | <EIF>
# ABML: ESRT.AD(TNCON) OR ESRT.AD(EIF)
s_IFA = (f_TNCON, kOR, f_EIF)

# ========================================
# Arguments for PRINT
# ========================================

# ========================================
# Numeric/String Expression Group
#   or Separators plus Expression-Group-or-Nothing
#   or Nothing
# <PR1> = <PEL> | <PSL> <PR2> | <NADA> <END>
# ABML: JS(PEL) OR JS(PSL) JS(PR2) OR RTN
s_PR1 = (s_PEL, kOR, s_PSL, s_PR2, kOR, kRTN)
# Numeric/String Expression Group, or Nothing
# <PR2> = <PEL> | <NADA> <END>
# ABML: JS(PEL) OR RTN
s_PR2 = (s_PEL, kOR, kRTN)
# Numeric/String Expression Group
# <PEL> = <PES> <PELA> <END>
# ABML: JS(PES) JS(PELA) RTN
s_PEL = (s_PES, s_PELA, kRTN)
# Numeric Expression or String Expression
# <PES> = <EXP> | <STR>
# ABML: VEXP OR ANTV.AS(STR) RTN
s_PES = (s_EXP, kOR, s_STR, kRTN)
# Separator plus Expression-Group-or-Nothing, or Nothing
# <PELA> = <PSL> <PEL> | <NADA> <END>
# ABML: JS(PSL) JS(PEL) OR RTN
s_PELA = (s_PSL, s_PEL, kOR, kRTN)

# ========================================
# One or More Print Separators
# <PSL> = <PS> <PSLA> <END>
# ABML: JS(PS) JS(PSLA) RTN
s_PSL = (s_PS, s_PSLA, kRTN)
# Print Separator or Nothing
# <PSLA> = <PSL> | <NADA> <END>
# ABML: JS(PSL) OR RTN
s_PSLA = (s_PSL, kOR, kRTN)
# Print Separator - Comma or Semicolon
# <PS> = , | ; <END>
# ABML: cCOM OR cSCOL RTN
s_PS = (cCOM, kOR, cSCOL, kRTN)

# ========================================
# List of Zero, One, or Two Expressions (e.g., for LIST)
# <L1> = <EXP> <L2> | <NADA> <END>
# ABML: VEXP JS(L2) OR RTN
s_L1 = (s_EXP, s_L2, kOR, kRTN)
# One More Expression or Nothing
# <L2> = ,<EXP> | <NADA> <END>
# ABML: cCOM VEXP OR RTN
s_L2 = (cCOM, s_EXP, kOR, kRTN)

# ========================================
# REM Statement takes the rest of the line. Note the lack of OR and RTN in this rule.
# <REM> = <EREM>
# ABML: ESRT.AD(EREM)
s_REM = (f_EREM)

# Defining s_REM as a function, we get:
#   def f_REM():
#       return f_EREM()

# DATA Statement takes the rest of the line
# <DATA> = <EDATA>
# ABML: ESRT.AD(EDATA)
s_DATA = (f_EDATA)

# Defining s_DATA as a function, we get:
#   def f_DATA():
#       return f_EDATA()

# ========================================
# ASC, VAL, or LEN Function Name
# <NFSP> = ASC | VAL | LEN <END>
# ABML: cASC OR cVAL OR cLEN RTN
s_NFSP = (cASC, kOR, cVAL, kOR, cLEN, kRTN)

# Defining s_NFSP as a function, we get:
#   def f_NFSP():
#       return (f_SRCONT(cASC) or f_SRCONT(cVAL) or f_SRCONT(cLEN))

# STR$ or CHR$ Function Name
# <SFNP> = STR | CHR <END>
# ABML: cSTR OR cCHR RTN
s_SFNP = (cSTR, kOR, cCHR, kRTN)

# Defining s_SFNP as a function, we get:
#   def f_SFNP():
#       return (f_SRCONT(cSTR) or f_SRCONT(cCHR))

# ========================================
# Unlimited Arguments for USR
# <PUSR> = <EXP> <PUSR1> <END>
# ABML: VEXP JS(PUSR1) RTN
s_PUSR = (s_EXP, s_PUSR1, kRTN)

# Defining s_PUSR as a function, we get:
#   def f_PUSR():
#       return f_EXP() and f_PUSR1()
#

# Continuation of USR Arguments
# <PUSR1> = ,<PUSR> | <NADA> <END>
# ABML: cCOM CHNG cACOM JS(PUSR) OR RTN
s_PUSR1 = (cCOM, kCHNG, cACOM, s_PUSR, kOR, kRTN)

# Defining s_PUSR1 as a function:
#   def f_PUSR1():
#       return (f_SRCONT(cCOM) and f_CHNG(cACOM) and f_PUSR())
#           or f_RTN()  # Also allow nothing (EOS, EOL, close paren?)
