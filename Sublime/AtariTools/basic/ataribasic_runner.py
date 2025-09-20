#!/usr/bin/env python3
#
# ataribasic_runner.py
#
# Execute an AtariBASIC program
#

from ataribasic import *

immediate_line = None

# STMCUR points to the current statement during Program Execution.
# In Interactive Mode it points to the start of the Immediate Line.
current_statement = immediate_line

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
