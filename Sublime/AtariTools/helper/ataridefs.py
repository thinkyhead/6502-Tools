"""
ataridefs.py - AtariBASIC language data
"""

DEBUG_DEFS = False

# --------------------------------------------------------------------------- #
# Static lookup Tables
# --------------------------------------------------------------------------- #

# Commands indexed by their Token ID
# In the original source the last byte of each string is marked with a high bit.
# For the '.' wildcard we just match the first command that satisfies it, so
# there is no need to mark the shortest allowed abbreviation in the data.
# This also means that rearranging commands will change their abbreviations.
statement_name_table = (
    ".REM", "D.ATA", "I.NPUT", "C.OLOR", "L.IST", "E.NTER", "LE.T", "IF", "F.OR", "N.EXT",
    "G.OTO", "GO .TO", "GOS.UB", "T.RAP", "B.YE", "CON.T", "COM", "CL.OSE", "CLR", "DE.G",
    "DI.M", "END", "NEW", "O.PEN", "LO.AD", "S.AVE", "ST.ATUS", "NO.TE", "P.OINT", "X.IO",
    "ON", "POK.E", "PR.INT", "RA.D", "REA.D", "RES.TORE", "RET.URN", "RU.N", "STO.P", "POP",
    "?", "GE.T", "PU.T", "GR.APHICS", "PL.OT", "POS.ITION", "DO.S", "DR.AWTO", "SE.TCOLOR", "LOC.ATE",
    "SO.UND", "LP.RINT", "CS.AVE", "CL.OAD", "(let)" if DEBUG_DEFS else "", "ERROR  -"
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
cEQ     = 0x22  # =      IF A=B
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

# Init as soon as created
init_lookups()
