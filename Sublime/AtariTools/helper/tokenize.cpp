/**
 * AtariBASIC Tokenizer
 *
 * Usage: tokenize [-ad] filename
 *  -a Standard AtariBASIC
 *  -d Detokenize
 *    -c Compressed listing
 *    -r Readable listing
 *
 * This program can tokenize or de-tokenize (list) an AtariBASIC
 * listing or tokenized file. The -a flag is a hint whether to expect
 * a Unicode listing or an Atari-native file listing. Unicode listing
 * will be converted with code from atasci.cpp.
 * 
 * The tokenizer converts BASIC code one line at a time to create
 * a final valid .BAS output file.
 *
 * The de-tokenizer converts a .BAS file to a source listing,
 * with a variety of options, such as -c (compressed) and -r (readable)
 *
 * REFERENCES
 *
 * - https://www.atariarchives.org/dere/chapt10.php
 *
 * BASIC file format:
 *  0 LOMEM - LOMEM     : Start of program storage (0000)
 *  2 VNTP - LOMEM      : Variable Name Table Pointer (offset)
 *  4 VNTD - LOMEM      : VNT end, plus one
 *  6 VVTP - LOMEM      : Variable Value Table Pointer (offset)
 *  8 STMTAB - LOMEM    : Statement Table Pointer (offset)
 * 10 STMCUR - LOMEM    : Current Statement Pointer (i.e., usually the offset to line 32768, but SAVE can be used in any program)
 * 12 STARP - LOMEM     : 
 * 14 ...               : Variable Name Table
 *    ...               : Variable Value Table (considered useless)
 *    ...               : Statement Table (tokenized program)
 * 
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>

typedef unsigned char  byte;
typedef unsigned char  uint8_t;
typedef unsigned char  ascii_t;
typedef unsigned short uint16_t;
typedef uint16_t atariptr_t;

#define COUNT(A) (sizeof(A)/sizeof(*A))

atariptr_t ITOA(const uint16_t I) { return (uint16_t(I) << 8) | (uint16_t(I) >> 8); }

typedef enum VarType {
  VarScalar       = 0x00,       // Scalar
  VarArrayUndim   = 0x40,       // Array (unDIMed)
  VarArray        = 0x41,       // Array (DIMed)
  VarStringUndim  = 0x80,       // String (unDIMed)
  VarString       = 0x81        // String (DIMed)
} var_t;

// Entry in the Variable Value Table (VVT)
typedef struct {
  VarType type;
  uint8_t varnum;
  union {
    struct { byte exp, digits[5]; } bcd;          // Binary coded decimal value
    struct { uint16_t offs, ref1, ref2; } arr;    // Array
    struct { uint16_t offs, length, dim; } str;   // String
  } val;
} vvt_entry_t;

typedef uint16_t AtariLineNum;  // Two byte little-endian integer

/*

Header saves memory locations $80-$8D

00 00 00 01  05 01 06 01  26 01 A4 01  B8 01

00000000   00 00 LOMEM
00000002   00 01 VNTP   +$0100  (5 bytes long)
00000004   05 01 VNTD   +$0105  end of table plus one
00000006   06 01 VVTP   +$0106
00000008   26 01 STMTAB +$0126
0000000A   A4 01 STMCUR +$01A4
0000000C   B8 01 STARP  +$01B8

Variable Name Table. Each variable name's last character has bit 7 set.

100 0000000E   C9 "I"
101 0000000F   CA "J"
102 00000010   D4 "T"
103 00000011      4B A4 "K$"    (VNTD - 1)

105 00000013            00  Fewer than 128 variables, dummy zero byte

Variable Value Table

106 00000014                00 00 40 05  13 20 00 00                    Scalar 0 5.132
    0000001C                                          00 01 40 06
    00000020   13 20 00 00                                              Scalar 1 6.132
    00000024                00 02 00 00  00 00 00 00                    Scalar 2 0
    0000002C                                          81 03 00 00
    00000030   06 00 06 00                                              String 3 0, len=6, dim=6

Line 10: ? "HELLO WORLD":IF 2<1 THEN 10
                            | 10|153|139  ? st 11  H   E  L  L  O
126 00000034                0A 00 2D 13  28 0F 0B 48  45 4C 4C 4F  ......-.(..HELLO
                _  W  O  R   L  D
    00000040   20 57 4F 52  4C 44                                   WORLD

                                   :153  IF sc |   2            |
    00000046                      14 2D  07 0E 40 02  00 00 00 00        .-..@.....
                < sc |   1            |  TH sc |  10            |
    00000050   20 0E 40 01  00 00 00 00  1B 0E 40 10  00 00 00 00   .@.......@.....
               ES
    00000060   16

Line 20: I=5.132
                  | 20| | 162| le #0 S=  sc |   5.13  2      | ES
153 00000061      14 00 0F  0F 36 80 2D  0E 40 05 13  20 00 00 16  .....6.-.@.. ...

Line 30: J=I+1
               | 30| |173|  le #1  = #0   + sc |     1          |
162 00000070   1E 00 11 11  36 81 2D 80  25 0E 40 01  00 00 00 00
               es
    00000080   16

Line 39: DIM K$(6)
                  | 39| | 183| di #3 (   sc |     6          | )
173 00000081      27 00 10  10 14 83 3B  0E 40 06 00  00 00 00 2C  .'.....;.@.....,
               es
    00000090   16

Line 40: K$="STRING"
                  | 40| | 193| LET #3 $= st  6  S  T   R  I  N  G
183 00000091      28 00 10  10 36 83 2E  0F 06 53 54  52 49 4E 47  .(...6....STRING
               es
    000000A0   16

Line 50: ? K$;" AND ";K$
                  | 50|  1A4 |  ? #3  ;  st  5  _  A   N  D  _ ;
193 000000A1      32 00 11  11 28 83 15  0F 05 20 41  4E 44 20 15  .2...(.... AND .
               #3 es
    000000B0   83 16

Line 32768 (immediate): S."H:TOKENS.BAS"
                     |imm|  end | sa st  12  H  :  T   O  K  E  N
1A4 000000B2         00 80  14 14 19 0F  0C 48 3A 54  4F 4B 45 4E  .........H:TOKEN
                S  .  B  A   S es
    000000C0   53 2E 42 41  53 16                                  S.BAS.


Line tokenizing notes:
  - 00 Line Number
  - 01 Next Line Offset (filled in after statement tokenization)
  - 02 Next Statement Offset (filled in after line tokenization)
  - 03 First Statement Token.
  - 04 ...additional tokens

BCD Tokenizing notes:
  - First byte is 40 for 10^0, no shifting. EX: 41 1000.000000 = 1E3 = 1000
                                                40 01.00000000 = 1
                                                45 140000000000. = 1.4E11 = 140,000,000,000
                                                41 0999.900000 = 999.9

  - 0x40 is the center where the byte 0 is 10^0
  - For each one over 0x40 multiply the basis by 100
  - For each one under 0x40 divide the basis by 100

 */

// Statements
// All statements 
const char * statement_str[] = {
  "R.EM",      //  0 (also ".")
  "D.ATA",     //  1
  "I.NPUT",    //  2
  "C.OLOR",    //  3
  "L.IST",     //  4
  "E.NTER",    //  5
  "LE.T",      //  6
  "IF",        //  7
  "F.OR",      //  8
  "N.EXT",     //  9
  "G.OTO",     // 10
  "GO TO",     // 11
  "GOS.UB",    // 12
  "T.RAP",     // 13
  "B.YE",      // 14
  "CONT",      // 15
  "COM",       // 16
  "CL.OSE",    // 17
  "CLR",       // 18
  "DEG",       // 19
  "DIM",       // 20
  "END",       // 21
  "NEW",       // 22
  "O.PEN",     // 23
  "LO.AD",     // 24
  "S.AVE",     // 25
  "ST.ATUS",   // 26
  "NO.TE",     // 27
  "P.OINT",    // 28
  "XIO",       // 29
  "ON",        // 30
  "POK.E",     // 31
  "PR.INT",    // 32
  "RAD",       // 33
  "REA.D",     // 34
  "RES.TORE",  // 35
  "RET.URN",   // 36
  "RU.N",      // 37
  "STO.P",     // 38
  "POP",       // 39
  "?",         // 40
  "GET",       // 41
  "PUT",       // 42
  "GR.APHIC",  // 43
  "PL.OT",     // 44
  "POS.ITION", // 45
  "DOS",       // 46
  "DR.AWTO",   // 47
  "SE.TCOLOR", // 48
  "LOC.ATE",   // 49
  "SO.UND",    // 50
  "LP.RINT",   // 51
  "CS.AVE",    // 52
  "CL.OAD",    // 53
  "{let}"      // 54 - Implied LET precedes a var-ref or substr-ref
};

// Particle Tokens (for the de-tokenizer).
// Tokenization has to consider context.

const char * cont_str[] = {
  "",          // 14 BCD Literal, Next 6 bytes
  "",          // 15 String Literal, Next byte is length
  "",          // 16 UNUSED
  "",          // 17 UNUSED
  ",",         // 18 , in PRINT statement
  "$",         // 19 ???
  "(eos)",     // 20 End of expr / statement 
  ";",         // 21 ; in PRINT statement
  "(eol)",     // 22 End of expr / line
  "GOTO",      // 23 ON...GOTO
  "GOSUB",     // 24 ON...GOSUB
  "TO",        // 25 FOR A=X TO
  "STEP",      // 26 FOR A=X TO STEP
  "THEN"       // 27 IF...THEN
  "#",         // 20 OPEN #
  "<=",        // 29 IF A<=B
  "<>",        // 30 IF A<>B
  ">=",        // 31 IF A<=B
  "<",         // 32 IF A<B
  ">",         // 33 IF A>B
  "=",         // 34 IF A=B
  "^",         // 35 A=B^C
  "*",         // 36 A=B*C
  "+",         // 37 A=B+C
  "-",         // 38 A=B-C
  "/",         // 39 A=B/C
  "NOT",       // 40 ... Boolean logic
  "OR",        // 41
  "AND",       // 42
  "(",         // 43 ... ( math
  ")",         // 44 ... close for all parens
  "=",         // 45 ... S1=S2
  "=",         // 46 ... N$=A$
  "<=",        // 47 ... cmp A$<=B$
  "<>",        // 48 ... cmp A$<>B$
  ">=",        // 49 ... cmp A$>=B$
  "<",         // 50 ... cmp A$<B$
  ">",         // 51 ... cmp A$<B$
  "=",         // 52 ... cmp A$=B$
  "+",         // 53 ... POSITIVE
  "-",         // 54 ... NEGATIVE
  "(",         // 55 ... A$( G.MPUSH
  "(",         // 56 ... SC(
  "(",         // 57 ... DIM SC(
  "(",         // 58 ... CHR$(, PEEK(
  "(",         // 59 ... DIM S$(
  ","          // 60 ... comma

  // Functions

  "STR$",      // 61
  "CHR$",      // 62
  "USR",       // 63
  "ASC",       // 64
  "VAL",       // 65
  "LEN",       // 66
  "ADR",       // 67
  "ATN",       // 68
  "COS",       // 69
  "PEEK",      // 60
  "SIN",       // 71
  "RND",       // 72
  "FRE",       // 73
  "EXP",       // 74
  "LOG",       // 75
  "CLOG",      // 76
  "SQR",       // 77
  "SGN",       // 78
  "ABS",       // 79
  "INT",       // 80
  "PADDLE",    // 81
  "STICK",     // 82
  "PTRIG",     // 83
  "STRIG"      // 84
};

// The tokenizer modes are many
enum TokenizerMode {
  LineStart,        // Expecting a line number
  LineNumber,       // Getting the Line number
  Statement,        // Statement or implicit LET
  Arguments,        // Not looking for statements
  LET,              // Letter not recognized as statement
  SYNTAX_ERROR,     // Reached a dead-end. Tokenize as a syntax error.
};

// End of line characters
#define EOL 0x9B
#define LF  0x0A
#define CR  0x0D

// LO and HI bytes of a native 16-bit integer
#define LO(W)       byte((W)&0xFF)
#define HI(W)       byte(((W)>>8)&0xFF)

// Output of a byte
#define OUT_B(B) do{ putchar(B); }while(0)
#define OUT_W(W) do{ putchar(LO(W)); putchar(HI(W)); }while(0)

#define NWS(C) ((C) == ' ' || (C) == LF)
#define AWS(C) ((C) == ' ' || (C) == EOL)
#define NEOL(C) ((C) == LF)
#define AEOL(C) ((C) == EOL)
#define WITHIN(V,A,B) ((V) >= (A) && (V) <= (B))
#define NUM(C) WITHIN(C,'0','9')

void usage(char * const bin) { fprintf(stderr, "Usage: %s [-ad] filename\n -a Standard AtariBASIC\n -d Detokenize\n", basename(bin)); }

/**
 * Search the statement list for the given string
 * and if it matches, return the matching token.
 *
 *
 * The input string must be nul-terminated.
 * This is called on the input string as it grows,
 * until it matches, or until it is clear there is
 * no possible match, when it gets tested to see if
 * it needs to be treated like a variable name.
 *
 */
int8_t statement_token(const char *str) {
  // TODO: Convert statement search to a binary tree
  // Small search space, small programs, so linear is fine for now!
  uint8_t c, d;
  for (uint8_t i = 0; i < COUNT(statement_str) - 1; i++) {
    uint8_t s = 0;
    bool dot_ok = false, mismatch = false;
    for (uint8_t n = 0; !mismatch && (c = statement_str[i][n]); ++n, ++s) {
      if (c == '.') {               // dot in compare string?
        dot_ok = true;              // then a dot is okay
        c = statement_str[i][n++];  // next non-dot char
      }
      // A dot at the end of the given string
      if (dot_ok && str[s] == '.') { str += s + 1; return i; } // Matching statement ending in .
      if (str[s] != c) mismatch = true;
    }

  }
}

atariptr_t LOMEM = 0,
           VNTP, VNTD, VVTP,
           STMTAB, STMCUR, STARP;

uint16_t vnt_ind = 0, vvt_ind = 0, stmtab_ind = 0;
byte vnt[32000], vvt[32000], stm[32000]; // Variable names, values. Statements.

// Gathered parts of a single tokenized statement
typedef struct {
  uint8_t line_num_lo, line_num_hi, next_line_offs;
} tokenized_line_t;

// Gathered parts of a single tokenized statement
typedef struct {
  uint8_t next_statement_offs;
  uint8_t *tokenized;     // Pointer to allocated tokens block
} tokenized_statement_t;

uint8_t line_ind;         // Index into the input line storage
char line_string[256];    // Storage for the input line as it is read
                          // An error anywhere in the line puts this into the error string

// Parse a single line of text into a tokenized statement
// TODO: Convert line_string from UTF-8 to ATASCII if no -a flag was used
void parse_line() {

  uint16_t line_num;

  uint8_t token_ind,        // The index into the token storage
          tokenized[128];

  uint8_t word_ind, rval_ind;
  char parse_word[20]; // First Keyword has to match one from the statement list
                       // A non-matching valid string is treated as "let STRING" and expects = next

  static TokenizerMode st = LineStart;  // Start in LineStart mode
  bool deciding = false;
  do {
    switch (st) {
      case LineStart:
        if (NWS(c)) break;                          // Ignore (native) whitespace
        if (!NUM(c)) { st = SYNTAX_ERROR; break; }  // No immediate lines
        st = LineNumber; line_num = 0;
      case LineNumber:
        if (NUM(c)) { line_num = line_num * 10 + c - '0'; break; }
        st = Statement; word_ind = 0;
      case Statement:
        if (NWS(c)) break;
        if (!WITHIN(c,'A','Z') && c != '.') { st = SYNTAX_ERROR; break; }
        parse_word[word_ind++] = c; parse_word[word_ind] = '\0';
        uint8_t tok = statement_token(parse_word);
        if (tok >= 0) {
          st = Arguments;
          token_ind = 0;
          tokenized[0] = tok;
          break;
        }
        break;
      case LET:
        break;
    }
  } while (deciding);
}

int tokenize(uint8_t *buffer) {
  return 0; // ok
}

int main(int argc, char *argv[]) {

  char aflag = false, dflag = false;

  // Get and check program arguments

  for (char k; (k = getopt(argc, argv, "ad")) != -1;) switch (k) {
    case 'a': aflag = true; break;
    case 'd': dflag = true; break;
    case '?': usage(argv[0]); return EXIT_FAILURE;
    default: abort();
  }

  // Check for incompatible options
  //if (dflag + iflag + nflag + uflag > 1) { fprintf(stderr, "Only one of -a -i -n -u may be used at a time.\n"); return EXIT_FAILURE; }

  // A filename parameter is required
  if (optind >= argc) { usage(argv[0]); return EXIT_FAILURE; }

  // Open the file to read
  FILE *fp = fopen(argv[optind], "r");
  if (!fp) { perror("File open failed"); return EXIT_FAILURE; }

  for (;;) {
    int c = fgetc(fp); // note: int, not char, required to handle EOF
    if (c != EOF) {
      if (!aflag && c == LF) c = EOL;  // Convert EOL - To be replaced by full UTF8-ATASCII conversion
      line_string[line_ind++] = c;
    }
    if (c == EOL || c == EOF) {
      parse_line();
      line_ind = 0;
    }
    if (c == EOF) break;
  }

  return EXIT_OK; // ok
}
