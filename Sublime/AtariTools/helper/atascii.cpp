/**
 * atascii - Convert to ATASCII or UTF-8, send to stdout.
 *
 * Usage: atascii [-a|-n] [-p] [-u] filename >outname
 *     By default, ATASCII to UTF-8 E000-E0FF, E100-E1FF
 *  -a ATASCII to plain ASCII
 *  -i International set UTF-8 mapping
 *  -n ATASCII to naive UTF-8
 *  -p Printed with 'LIST "P:"'
 *  -u UTF-8 to ATASCII
 *
 * Atari Font Mapping:
 *
 *   $E000 for the normal set;
 *   $E080 for the inverse set;
 *   $E100 for the international set; and
 *   $E180 for the inverse international set."
 *
 *   3-byte UTF-8 : 1110 aaaa  10bb bbc.  10.. ....
 *         Normal :      1110  1000 000.  10.. ....   E00
 *         Invert :      1110  1000 001.  10.. ....   E08
 *   Int'l Normal :      1110  1000 010.  10.. ....   E10
 *   Int'l Invert :      1110  1000 011.  10.. ....   E18
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>

typedef unsigned char achar_t;

static const achar_t  LF = 0x0A,  // ASCII LF
                      CR = 0x0D,  // ASCII CR
                  ATESC  = 0x1B,  // ESCAPE
                  ATURW  = 0x1C,  // UP-ARROW
                  ATDRW  = 0x1D,  // DOWN-ARROW
                  ATLRW  = 0x1E,  // LEFT-ARROW
                  ATRRW  = 0x1F,  // RIGHT-ARROW
                  ATCLR  = 0x7D,  // CLEAR SCREEN CHARACTER
                  ATRUB  = 0x7E,  // BACK SPACE (RUBOUT)
                  ATTAB  = 0x7F,  // TAB
                  ATEOL  = 0x9B,  // END-OF-LINE
                  ATDELL = 0x9C,  // delete line
                  ATINSL = 0x9D,  // insert line
                  ATCTAB = 0x9E,  // clear TAB
                  ATSTAB = 0x9F,  // set TAB
                  ATBEL  = 0xFD,  // CONSOLE BELL
                  ATDEL  = 0xFE,  // delete char.
                  ATINS  = 0xFF;  // insert char.

#define REG_NORMAL 0xE000
#define REG_INVERT 0xE080
#define INT_NORMAL 0xE100
#define INT_INVERT 0xE180

enum { ASCII, UTF_1, UTF_2, UTF_3 };

void usage(char * const bin) { fprintf(stderr, "Usage: %s [-a|-n|-u] [-p] filename\n -a ATASCII to plain ASCII\n -i International set UTF-8 output\n -n ATASCII to naive UTF-8\n -p Printed with 'LIST \"P:\"'\n -u UTF-8 to ATASCII\n", basename(bin)); }

//                                              |E | |E |   | 1 ||  F  | |F |
//
// precursor  : 23-20                           1110 .... .... .... .... ....  E
// bits 15-12 : 19-16  XXXX .... .... ....  =>  .... XXXX .... .... .... ....  E
// "10"       : 15-14                           .... .... 10.. .... .... ....  8
// bits 11-06 : 13-08  .... XXXX XX.. ....  =>  .... .... ..XX XXXX .... ....  7
// "10"       : 7-6                             .... .... .... .... 10.. ....  B
// bits 05-00 : 5-0    .... .... ..XX XXXX  =>  .... .... .... .... ..XX XXXX  F
//     
int put_utf_trio(const unsigned char c, const bool intl=false) {
  const unsigned short chr = ((c & 0x80) ? (intl ? INT_INVERT : REG_INVERT) : (intl ? INT_NORMAL : REG_NORMAL)) + (c & 0x7F);
  const unsigned char c0 = 0xE0 | (chr >> 12), c1 = 0x80 | ((chr >> 6) & 0x3F), c2 = 0x80 | (chr & 0x3F);
  //fprintf(stderr, "Converted CHR(%i) to %02x %02x %02x\n", int(c), c0, c1, c2);
  if (putchar(c0) == EOF) return EOF;
  if (putchar(c1) == EOF) return EOF;
  return putchar(c2);
}

int putchar_a(const unsigned char c) { return putchar(c == LF ? ATEOL : c); }
int putchar_u(const unsigned char c) { return putchar(c == ATEOL ? LF : c); }

unsigned char
  chr00[] = { 0xE2, 0x99, 0xA5, 0 },        chr01[] = { 0xE2, 0x94, 0x9C, 0 },        chr02[] = { 0x20 },
  chr03[] = { 0xE2, 0x94, 0x98, 0 },        chr04[] = { 0xE2, 0x94, 0xA4, 0 },        chr05[] = { 0xE2, 0x94, 0x90, 0 },
  chr06[] = { 0xE2, 0x95, 0xB1, 0 },        chr07[] = { 0xE2, 0x95, 0xB2, 0 },        chr08[] = { 0xE2, 0x97, 0xA2, 0 },
  chr09[] = { 0xE2, 0x96, 0x97, 0 },        chr0A[] = { 0xE2, 0x97, 0xA3, 0 },        chr0B[] = { 0xE2, 0x96, 0x9D, 0 },
  chr0C[] = { 0xE2, 0x96, 0x98, 0 },        chr0D[] = { 0xE2, 0x96, 0x86, 0 },        chr0E[] = { 0xE2, 0x96, 0x82, 0 },
  chr0F[] = { 0xE2, 0x96, 0x96, 0 },        chr10[] = { 0xE2, 0x99, 0xA3, 0 },        chr11[] = { 0xE2, 0x94, 0x8C, 0 },
  chr12[] = { 0xE2, 0x94, 0x80, 0 },        chr13[] = { 0xE2, 0x94, 0xBC, 0 },        chr14[] = { 0xE2, 0x97, 0x8F, 0 },
  chr15[] = { 0xE2, 0x96, 0x84, 0 },        chr16[] = { 0xE2, 0x96, 0x8E, 0 },        chr17[] = { 0xE2, 0x94, 0xAC, 0 },
  chr18[] = { 0xE2, 0x94, 0xB4, 0 },        chr19[] = { 0xE2, 0x96, 0x8C, 0 },        chr1A[] = { 0xE2, 0x94, 0x94, 0 },
  chr1B[] = { 0xE2, 0x90, 0x9B, 0 },        chr1C[] = { 0xE2, 0x86, 0x91, 0 },        chr1D[] = { 0xE2, 0x86, 0x93, 0 },
  chr1E[] = { 0xE2, 0x86, 0x90, 0 },        chr1F[] = { 0xE2, 0x86, 0x92, 0 },        chr60[] = { 0xE2, 0x99, 0xA6, 0 },
  chr7B[] = { 0xE2, 0x99, 0xA0, 0 },        chr7D[] = { 0xE2, 0x87, 0xB1, 0 },
  chr7E[] = { 0xE2, 0x97, 0x80, 0 },        chr7F[] = { 0xE2, 0x96, 0xB6, 0 },        chr80[] = { 0xE2, 0x99, 0xA5, 0 },
  chr81[] = { 0xE2, 0x94, 0xA3, 0 },        chr82[] = { 0xE2, 0x96, 0x8A, 0 },        chr83[] = { 0xE2, 0x94, 0x9B, 0 },
  chr84[] = { 0xE2, 0x94, 0xAB, 0 },        chr85[] = { 0xE2, 0x94, 0x93, 0 },        chr86[] = { 0xE2, 0x95, 0xB1, 0 },
  chr87[] = { 0xE2, 0x95, 0xB2, 0 },        chr88[] = { 0xE2, 0x97, 0xA4, 0 },        chr89[] = { 0xE2, 0x96, 0x9B, 0 },
  chr8A[] = { 0xE2, 0x97, 0xA5, 0 },        chr8B[] = { 0xE2, 0x96, 0x99, 0 },        chr8C[] = { 0xE2, 0x96, 0x9F, 0 },
  chr8D[] = { 0xE2, 0x96, 0x86, 0 },        chr8E[] = { 0x3F },                       chr8F[] = { 0xE2, 0x96, 0x9C, 0 },
  chr90[] = { 0xE2, 0x99, 0xA3, 0 },        chr91[] = { 0xE2, 0x94, 0x8F, 0 },        chr92[] = { 0xE2, 0x94, 0x81, 0 },
  chr93[] = { 0xE2, 0x95, 0x8B, 0 },        chr94[] = { 0xE2, 0x97, 0x8F, 0 },        chr95[] = { 0xE2, 0x96, 0x80, 0 },
  chr96[] = { 0x3F },                       chr97[] = { 0xE2, 0x94, 0xB3, 0 },        chr98[] = { 0xE2, 0x94, 0xBB, 0 },
  chr99[] = { 0xE2, 0x96, 0x90, 0 },        chr9A[] = { 0xE2, 0x94, 0x97, 0 },        chr9B[] = { 0xE2, 0x90, 0x9B, 0 },
  chr9C[] = { 0xE2, 0x87, 0x91, 0 },        chr9D[] = { 0xE2, 0x87, 0x93, 0 },        chr9E[] = { 0xE2, 0x87, 0x90, 0 },
  chr9F[] = { 0xE2, 0x87, 0x92, 0 },        chrA0[] = { 0xE2, 0x96, 0x91, 0 },        chrA1[] = { 0x21 },
  chrA2[] = { 0x22 },                       chrA3[] = { 0x23 },                       chrA4[] = { 0x24 },
  chrA5[] = { 0x25 },                       chrA6[] = { 0x26 },                       chrA7[] = { 0x27 },
  chrA8[] = { 0x28 },                       chrA9[] = { 0x29 },                       chrAA[] = { 0x2A },
  chrAB[] = { 0x2B },                       chrAC[] = { 0x2C },                       chrAD[] = { 0x2D },
  chrAE[] = { 0x2E },                       chrAF[] = { 0x2F },                       chrB0[] = { 0xF0, 0x9D, 0x9F, 0xAC, 0 },
  chrB1[] = { 0xF0, 0x9D, 0x9F, 0xAD, 0 },  chrB2[] = { 0xF0, 0x9D, 0x9F, 0xAE, 0 },  chrB3[] = { 0xF0, 0x9D, 0x9F, 0xAF, 0 },
  chrB4[] = { 0xF0, 0x9D, 0x9F, 0xB0, 0 },  chrB5[] = { 0xF0, 0x9D, 0x9F, 0xB1, 0 },  chrB6[] = { 0xF0, 0x9D, 0x9F, 0xB2, 0 },
  chrB7[] = { 0xF0, 0x9D, 0x9F, 0xB3, 0 },  chrB8[] = { 0xF0, 0x9D, 0x9F, 0xB4, 0 },  chrB9[] = { 0xF0, 0x9D, 0x9F, 0xB5, 0 },
  chrBA[] = { 0x3A },                       chrBB[] = { 0x3B },                       chrBC[] = { 0x3C },
  chrBD[] = { 0x3D },                       chrBE[] = { 0x3E },                       chrBF[] = { 0x3F },
  chrC0[] = { 0x40 },                       chrC1[] = { 0xF0, 0x9D, 0x97, 0x94, 0 },  chrC2[] = { 0xF0, 0x9D, 0x97, 0x95, 0 },
  chrC3[] = { 0xF0, 0x9D, 0x97, 0x96, 0 },  chrC4[] = { 0xF0, 0x9D, 0x97, 0x97, 0 },  chrC5[] = { 0xF0, 0x9D, 0x97, 0x98, 0 },
  chrC6[] = { 0xF0, 0x9D, 0x97, 0x99, 0 },  chrC7[] = { 0xF0, 0x9D, 0x97, 0x9A, 0 },  chrC8[] = { 0xF0, 0x9D, 0x97, 0x9B, 0 },
  chrC9[] = { 0xF0, 0x9D, 0x97, 0x9C, 0 },  chrCA[] = { 0xF0, 0x9D, 0x97, 0x9D, 0 },  chrCB[] = { 0xF0, 0x9D, 0x97, 0x9E, 0 },
  chrCC[] = { 0xF0, 0x9D, 0x97, 0x9F, 0 },  chrCD[] = { 0xF0, 0x9D, 0x97, 0xA0, 0 },  chrCE[] = { 0xF0, 0x9D, 0x97, 0xA1, 0 },
  chrCF[] = { 0xF0, 0x9D, 0x97, 0xA2, 0 },  chrD0[] = { 0xF0, 0x9D, 0x97, 0xA3, 0 },  chrD1[] = { 0xF0, 0x9D, 0x97, 0xA4, 0 },
  chrD2[] = { 0xF0, 0x9D, 0x97, 0xA5, 0 },  chrD3[] = { 0xF0, 0x9D, 0x97, 0xA6, 0 },  chrD4[] = { 0xF0, 0x9D, 0x97, 0xA7, 0 },
  chrD5[] = { 0xF0, 0x9D, 0x97, 0xA8, 0 },  chrD6[] = { 0xF0, 0x9D, 0x97, 0xA9, 0 },  chrD7[] = { 0xF0, 0x9D, 0x97, 0xAA, 0 },
  chrD8[] = { 0xF0, 0x9D, 0x97, 0xAB, 0 },  chrD9[] = { 0xF0, 0x9D, 0x97, 0xAC, 0 },  chrDA[] = { 0xF0, 0x9D, 0x97, 0xAD, 0 },
  chrDB[] = { 0x5B },                       chrDC[] = { 0x5C },                       chrDD[] = { 0x5D },
  chrDE[] = { 0x5E },                       chrDF[] = { 0x5F },                       chrE0[] = { 0xE2, 0x99, 0xA6, 0 },
  chrE1[] = { 0xF0, 0x9D, 0x97, 0xAE, 0 },  chrE2[] = { 0xF0, 0x9D, 0x97, 0xAF, 0 },  chrE3[] = { 0xF0, 0x9D, 0x97, 0xB0, 0 },
  chrE4[] = { 0xF0, 0x9D, 0x97, 0xB1, 0 },  chrE5[] = { 0xF0, 0x9D, 0x97, 0xB2, 0 },  chrE6[] = { 0xF0, 0x9D, 0x97, 0xB3, 0 },
  chrE7[] = { 0xF0, 0x9D, 0x97, 0xB4, 0 },  chrE8[] = { 0xF0, 0x9D, 0x97, 0xB5, 0 },  chrE9[] = { 0xF0, 0x9D, 0x97, 0xB6, 0 },
  chrEA[] = { 0xF0, 0x9D, 0x97, 0xB7, 0 },  chrEB[] = { 0xF0, 0x9D, 0x97, 0xB8, 0 },  chrEC[] = { 0xF0, 0x9D, 0x97, 0xB9, 0 },
  chrED[] = { 0xF0, 0x9D, 0x97, 0xBA, 0 },  chrEE[] = { 0xF0, 0x9D, 0x97, 0xBB, 0 },  chrEF[] = { 0xF0, 0x9D, 0x97, 0xBC, 0 },
  chrF0[] = { 0xF0, 0x9D, 0x97, 0xBD, 0 },  chrF1[] = { 0xF0, 0x9D, 0x97, 0xBE, 0 },  chrF2[] = { 0xF0, 0x9D, 0x97, 0xBF, 0 },
  chrF3[] = { 0xF0, 0x9D, 0x98, 0x80, 0 },  chrF4[] = { 0xF0, 0x9D, 0x98, 0x81, 0 },  chrF5[] = { 0xF0, 0x9D, 0x98, 0x82, 0 },
  chrF6[] = { 0xF0, 0x9D, 0x98, 0x83, 0 },  chrF7[] = { 0xF0, 0x9D, 0x98, 0x84, 0 },  chrF8[] = { 0xF0, 0x9D, 0x98, 0x85, 0 },
  chrF9[] = { 0xF0, 0x9D, 0x98, 0x86, 0 },  chrFA[] = { 0xF0, 0x9D, 0x98, 0x87, 0 },  chrFB[] = { 0xE2, 0x99, 0xA0, 0 },
  chrFC[] = { 0x7C },                       chrFD[] = { 0xE2, 0x87, 0xB1, 0 },        chrFE[] = { 0xE2, 0x97, 0x80, 0 },
  chrFF[] = { 0xE2, 0x96, 0xB6, 0 };

unsigned char * naive[] = {
  chr00, chr01, chr02, chr03, chr04, chr05, chr06, chr07, chr08, chr09, chr0A, chr0B, chr0C, chr0D, chr0E, chr0F,
  chr10, chr11, chr12, chr13, chr14, chr15, chr16, chr17, chr18, chr19, chr1A, chr1B, chr1C, chr1D, chr1E, chr1F,
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,
  chr60, NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,
  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL,  NULL, chr7B,  NULL, chr7D, chr7E, chr7F,
  chr80, chr81, chr82, chr83, chr84, chr85, chr86, chr87, chr88, chr89, chr8A, chr8B, chr8C, chr8D, chr8E, chr8F,
  chr90, chr91, chr92, chr93, chr94, chr95, chr96, chr97, chr98, chr99, chr9A, chr9B, chr9C, chr9D, chr9E, chr9F,
  chrA0, chrA1, chrA2, chrA3, chrA4, chrA5, chrA6, chrA7, chrA8, chrA9, chrAA, chrAB, chrAC, chrAD, chrAE, chrAF,
  chrB0, chrB1, chrB2, chrB3, chrB4, chrB5, chrB6, chrB7, chrB8, chrB9, chrBA, chrBB, chrBC, chrBD, chrBE, chrBF,
  chrC0, chrC1, chrC2, chrC3, chrC4, chrC5, chrC6, chrC7, chrC8, chrC9, chrCA, chrCB, chrCC, chrCD, chrCE, chrCF,
  chrD0, chrD1, chrD2, chrD3, chrD4, chrD5, chrD6, chrD7, chrD8, chrD9, chrDA, chrDB, chrDC, chrDD, chrDE, chrDF,
  chrE0, chrE1, chrE2, chrE3, chrE4, chrE5, chrE6, chrE7, chrE8, chrE9, chrEA, chrEB, chrEC, chrED, chrEE, chrEF,
  chrF0, chrF1, chrF2, chrF3, chrF4, chrF5, chrF6, chrF7, chrF8, chrF9, chrFA, chrFB, chrFC, chrFD, chrFE, chrFF
};

int main(int argc, char *argv[]) {
  char aflag = false, iflag = false, nflag = false, pflag = false, uflag = false;

  // Get and check program arguments

  for (char k; (k = getopt(argc, argv, "ainpu")) != -1;) switch (k) {
    case 'a': aflag = true; break;
    case 'i': iflag = true; break;
    case 'n': nflag = true; break;
    case 'p': pflag = true; break;
    case 'u': uflag = true; break;
    case '?': usage(argv[0]); return EXIT_FAILURE;
    default: abort();
  }

  // Check for incompatible options
  if (aflag + iflag + nflag + uflag > 1) { fprintf(stderr, "Only one of -a -i -n -u may be used at a time.\n"); return EXIT_FAILURE; }
  if (pflag && uflag) { fprintf(stderr, "-p ignored\n"); }

  // A filename parameter is required
  if (optind >= argc) { usage(argv[0]); return EXIT_FAILURE; }

  // Open the file to read
  FILE *fp = fopen(argv[optind], "r");
  if (!fp) { perror("File open failed"); return EXIT_FAILURE; }

  int c; // note: int, not char, required to handle EOF

  if (uflag) {
    //
    // Convert from UTF-8 to ATASCII
    //
    char st = 0;
    unsigned char c1;
    while ((c = fgetc(fp)) != EOF) switch (st) {
      case ASCII:
        if (c == 0xEE) st = UTF_1; else putchar_a(c);
        break;
      case UTF_1:
        if ((c & 0xC0) == 0x80) {
          c1 = c;
          st = UTF_2;
        }
        else {
          putchar(0xEE); putchar_a(c);
          st = ASCII;
        }
        break;
      case UTF_2:
        c = (c1 & 3) << 6 | (c & 0x3F); putchar(c);
        st = ASCII;
        break;
    }
  }
  else {
    //
    // Convert from ATASCII to UTF-8
    //
    bool got_lf = false;
    while ((c = fgetc(fp)) != EOF) {
      if (pflag) {
        /**
         * The LIST "P:" command in atari800 replaces 0x9B with 0x0A (LF)
         * which makes it (nearly) impossible to tell whether LF was
         * actually in the original file. As a workaround for most cases,
         * only keep the LF if the next character is a numeric digit.
         *
         * A more robust heuristic would look for one or more digits
         * followed by a space.
         */
        if (got_lf) {
          if (c >= '0' && c <= '9') putchar(LF); // LF followed by a digit? Trust the LF.
          else put_utf_trio(0x0A, iflag);        // LF in the middle of a line, substitute
        }

        // Wait till the next character to decide what to do with LF
        if (c == LF) { got_lf = true; continue; }

        got_lf = false;
      }

      if (aflag) {
        //
        // Do a clean ASCII-only conversion
        //
             if (c  < 0x20) { putchar(c + 'a' - 1); }
        else if (c >= 0xC0) { putchar(c - 0xC0 + 'A' - 1); }
        else putchar_u(c);
      }
      else if (nflag) {
        //
        // Convert to common UTF-8 for Unicode fonts
        //
        if (c == ATEOL) putchar(LF);
        else {
          unsigned char *utf_bytes = naive[c];
          if (utf_bytes == NULL) putchar(c);
          else if (utf_bytes[0] < 0x80) putchar(utf_bytes[0]);
          else for (int i = 0; char n = utf_bytes[i]; i++) putchar(n);
        }
      }
      else {
        //
        // Convert to user-space UTF-8 for Atari Font
        //
        if (c == ATEOL) putchar(LF);
        else {
          const unsigned char b = c & 0x7F;
          if (b <= 0x1F || b == 0x60 || c >= 0x7B) put_utf_trio(c, iflag); else putchar_u(c);
        }
      }
    }
    // If the last character in the file was LF...
    if (got_lf) putchar(LF);
  }

  if (ferror(fp)) { perror("File read error!"); return EXIT_FAILURE; }

  fclose(fp);
  return EXIT_SUCCESS;
}
