/* OUTPUT.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* This file contains routines used to perform screen output.
 * The first set of routines is used to print cracked instructions.
 */

#include <stdio.h>
#include "main.h"
#include "decode.h"
#include "execute.h"
#include "state.h"
#include "output.h"

/* ®MDBO¯---------------------------------------------------®MDNM¯ */

/* disassembler constants for alu functions */
char *alu_names[] = {
                     /* 0 */ "",               /* 1 */ "not ",
                     /* 2 */ "and ",           /* 3 */ "nor ",
                     /* 4 */ "swap- ",         /* 5 */ "swap-c ",
                     /* 6 */ "or ",            /* 7 */ "nand ",
                     /* 8 */ "+ ",             /* 9 */ "+c ",
                     /* a */ "xor ",           /* b */ "xnor ",
                     /* c */ "- ",             /* d */ "-c ",
                     /* e */ "",               /* f */ "not "      };
char *swap_alu_names[] = {
                          /* 0 */ "",               /* 1 */ "not ",
                          /* 2 */ "and ",           /* 3 */ "nor ",
                          /* 4 */ "- ",             /* 5 */ "-c ",
                          /* 6 */ "or ",            /* 7 */ "nand ",
                          /* 8 */ "+ ",             /* 9 */ "+c ",
                          /* a */ "xor ",           /* b */ "xnor ",
                          /* c */ "swap- ",         /* d */ "swap-c ",
                          /* e */ "",               /* f */ "not "      };

/* disassembler constants for shifter functions */
char *shift_names[] = {
                       /* 0 */ "",               /* 1 */ "0< ",
                       /* 2 */ "2* ",            /* 3 */ "2*c ",
                       /* 4 */ "cU2/ ",          /* 5 */ "c2/ ",
                       /* 6 */ "U2/ ",           /* 7 */ "2/ ",
                       /* 8 */ "N2* ",           /* 9 */ "N2*c ",
                       /* A */ "D2* ",           /* B */ "D2*c ",
                       /* C */ "cUD2/ ",         /* D */ "cD2/ ",
                       /* E */ "UD2/ ",          /* F */ "D2/ "         } ;

/* disassembler constants for memory size specifiers */
char *memory_names[] = {
                        /* 0 */ "",               /* 1 */ "C"  } ;

/* MACROS FOR SIMPLIFIED PRINTING */

/* print out a shift operation */
#define SHIFT  (shift_names[instruction & 0xF])

/* print out the invert operation */
#define INVERT (alu_names[(instruction>>8) & 0x1])

/* print out an alu operation */
#define ALU      (alu_names[(instruction>>8) & 0xF])
#define SWAP_ALU (swap_alu_names[(instruction>>8) & 0xF])

/* print out a 3-bit alu operation */
#define ALU3      (alu_names[(instruction>>8) & 0xE])
#define SWAP_ALU3 (swap_alu_names[(instruction>>8) & 0xE])

/* extract the 5-bit short-lit/g-bus/user value */
#define SHORT_LIT  (instruction & 0x1F)

/* extract the bit that determines word vs. byte */
#define MEM  (memory_names[(instruction>>12) & 0x1])

/* extract the in-line 16-bit lit */
#define LIT  long_fetch(page, address)

#define dump_header(n)                                  \
  temp = byte_fetch( ((instruction<<1)&0xFFFF)-n);      \
  if (temp > 32  && temp < 128 )                        \
    putchar(byte_fetch( ((instruction<<1)&0xFFFF)-n));

/* ®MDBO¯---------------------------------------------------®MDNM¯ */

/* print a disassembled instruction to a designated output */
/* inputs: *outf = output file (e.g., stdout)
 *      opr = decoded instruction token
 *     instruction = 16-bit instruction value
 *     address = address of instruction (for branch computations
 */
void print_instruction(FILE *outf,  machine_op opr,
                       int instruction, int page, int address)
{ int temp;
  switch (opr)
    {
    case OP_0BRANCH:
      fprintf(outf,"0BRANCH %01X%04X ", page,
              target_addr(instruction,address));                break;
    case OP_ALU:             fprintf(outf,"%s%s", ALU,SHIFT);  break;
    case OP_ASIC_STREAM_MAC: fprintf(outf,"Asic Stream Mac");  break;
    case OP_BRANCH:
      fprintf(outf,"BRANCH %01X%04X ", page,
              target_addr(instruction,address));                break;
    case OP_BS_ONE_TICK:     fprintf(outf,"BS1' ");  break;
    case OP_BS_TICK:         fprintf(outf,"BS' ");  break;
    case OP_BUSLASH_TICK:    fprintf(outf,"BU/' ");  break;
    case OP_BUSTAR_TICK:     fprintf(outf,"BU*' ");  break;
    case OP_BUSTAR_TICK_TICK: fprintf(outf,"BU*'' ");  break;
    case OP_CALL:
      fprintf(outf,"CALL %01X%04X ", page,
              ((instruction<<1) & 0xFFFF) );
      dump_header(9);
      dump_header(8);
      dump_header(7);
      dump_header(6);
      dump_header(5);
      dump_header(4);
      dump_header(3);
      dump_header(2);
      dump_header(1);
      break;
    case OP_CLEAR_ACC:       fprintf(outf,"Clear Acc ");  break;
    case OP_CLEAR_SOFTINT:   fprintf(outf,"Clear Softint ");  break;
    case OP_C_TICK:          fprintf(outf,"C' ");  break;
    case OP_DDUP_ALU:        fprintf(outf,"DDUP %s%s", ALU, SHIFT);  break;
    case OP_DDUP_STORE:      fprintf(outf,"DDUP %s! ", MEM);  break;
    case OP_DDUP_STORE_WITH_ALU: fprintf(outf,"DDUP %s! %X %s",
                                         MEM, SHORT_LIT, SWAP_ALU3);  break;
    case OP_DEC_RX:          fprintf(outf,"DEC_RX ");  break;
    case OP_DROP:            fprintf(outf,"DROP %s%s", INVERT, SHIFT);  break;
    case OP_DROP_DUP:        fprintf(outf,"DROP DUP %s%s", INVERT, SHIFT);
      break;
    case OP_DROP_LIT:        fprintf(outf,"DROP LIT %d %s", LIT, INVERT);  break;
    case OP_DROP_SHORT_LIT:  fprintf(outf,"DROP %X %s", SHORT_LIT, INVERT);
      break;
    case OP_DSLL:            fprintf(outf,"Double Shift Left ");  break;
    case OP_DSRA:    fprintf(outf,"Double Shift Right Arithmetic ");  break;
    case OP_DSRL:    fprintf(outf,"Double Shift Right Logical ");  break;
    case OP_DUP:             fprintf(outf,"DUP %s%s", INVERT, SHIFT);  break;
    case OP_DUP_FETCH_SWAP:  fprintf(outf,"DUP %s@ SWAP ", MEM);  break;
    case OP_DUP_GSTORE:      fprintf(outf,"DUP %X G! %s", SHORT_LIT, INVERT);
      break;
    case OP_DUP_USTORE:      fprintf(outf,"DUP %X U! %s", SHORT_LIT, INVERT);
      break;
    case OP_FETCH:           fprintf(outf,"%s@ %s", MEM, INVERT);  break;
    case OP_FETCH_LIT:       fprintf(outf,"%s@ %X ", MEM, SHORT_LIT);  break;
    case OP_FETCH_OVER_ALU:  fprintf(outf,"%s@ OVER %s", MEM, ALU);  break;
    case OP_FETCH_SWAP:      fprintf(outf,"%s@ SWAP %s", MEM, INVERT);  break;
    case OP_FETCH_SWAP_ALU:  fprintf(outf,"%s@ %s", MEM, SWAP_ALU);  break;
    case OP_FETCH_WITH_ALU:  fprintf(outf,"%X %s@_%s", SHORT_LIT, MEM, ALU3);
      break;
    case OP_GFETCH:          fprintf(outf,"%X G@ %s", SHORT_LIT, INVERT);
      if (SHORT_LIT == 1 )  fprintf(outf,"(R>) ");
      break;
    case OP_GFETCH_DROP:     fprintf(outf,"%X G@ DROP %s", SHORT_LIT, INVERT);
      if (SHORT_LIT == 1 )  fprintf(outf,"(R> DROP) ");
      break;
    case OP_GFETCH_OVER_ALU: fprintf(outf,"%X G@ OVER %s", SHORT_LIT, ALU);
      break;
    case OP_GFETCH_SWAP_ALU: fprintf(outf,"%X G@ %s", SHORT_LIT, SWAP_ALU);
      break;
    case OP_GSTORE:          fprintf(outf,"%X G! %s", SHORT_LIT, INVERT);
      if (SHORT_LIT == 0 )  fprintf(outf,"(R> DROP >R) ");
      if (SHORT_LIT == 1 )  fprintf(outf,"(>R) ");
      if (SHORT_LIT == 7 &&  (instruction&0x0020))
        fprintf(outf,"(>R;) ");
      break;
    case OP_INC_RX:          fprintf(outf,"INC_RX ");  break;
    case OP_LIT:             fprintf(outf,"LIT %d %s", LIT, INVERT);  break;
    case OP_LIT_OVER_ALU:    fprintf(outf,"LIT %d OVER %s", LIT, ALU);  break;
    case OP_LIT_SWAP:        fprintf(outf,"LIT %d SWAP %s", LIT, INVERT);  break;
    case OP_LIT_SWAP_ALU:    fprintf(outf,"LIT %d %s", LIT, SWAP_ALU);  break;
    case OP_MAC:             fprintf(outf,"Multiply Accumulate ");  break;
    case OP_MIXED_MAC:       fprintf(outf,"Mixed Mode MAC ");  break;
    case OP_MIXED_MULT:      fprintf(outf,"Mixed Mode Multiply ");  break;
    case OP_MULT:            fprintf(outf,"Multiply ");  break;
    case OP_MULT_SUB:        fprintf(outf,"Multiply Subtract ");  break;
    case OP_NEXT:
      fprintf(outf,"NEXT %01X%04X ", page,
              target_addr(instruction,address));
      break;
    case OP_NIP:             fprintf(outf,"NIP %s%s", INVERT, SHIFT);
      break;
    case OP_NIP_DUP:         fprintf(outf,"NIP DUP %s%s", INVERT, SHIFT);
      break;
    case OP_NIP_DUP_FETCH_SWAP:
      fprintf(outf,"NIP DUP %s@ SWAP ", MEM);  break;
    case OP_NIP_FETCH_LIT:   fprintf(outf,"NIP %s@ %X ", MEM, SHORT_LIT);
      break;
    case OP_NIP_FETCH_WITH_ALU: fprintf(outf,"NIP %X %s@_%s",
                                        SHORT_LIT, MEM, ALU3);  break;
    case OP_NORMALIZE:       fprintf(outf,"Normalize ");  break;
    case OP_OVER:            fprintf(outf,"OVER %s%s", INVERT, SHIFT);  break;
    case OP_QDUP_0BRANCH:
      fprintf(outf,"?DUP 0BRANCH %01X%04X ", page,
              target_addr(instruction,address));
      break;
    case OP_RDR:             fprintf(outf,"RDR ");  break;
    case OP_RESERVED: /*       fprintf(outf,"RESERVED (%04X)", instruction); */
      fprintf(outf,"?? ");
      break;
    case OP_RESERVED_STEP_MATH: /* fprintf(outf,"RESERVED (step math %04X) ",
                                   instruction);  break;  */
      fprintf(outf,"?? ");
      break;
    case OP_RTR:             fprintf(outf,"RTR ");  break;
    case OP_R_TICK:          fprintf(outf,"R' ");  break;
    case OP_SELECT_CPR:      fprintf(outf,"Select CPR ");  break;
    case OP_SELECT_DPR:      fprintf(outf,"Select DPR ");  break;
    case OP_SET_SOFTINT:     fprintf(outf,"Set Softint ");  break;
    case OP_SHIFT:
      if ( (instruction&0x0FFF) == 0)    fprintf(outf,"NOP ");
      else              fprintf(outf,"%s%s", INVERT, SHIFT);
      break;
    case OP_SHIFT_MAC_RIGHT: fprintf(outf,"Shift Mac Right ");  break;
    case OP_SHORT_LIT:       fprintf(outf,"%X %s", SHORT_LIT, INVERT);  break;
    case OP_SHORT_LIT_OVER_ALU: fprintf(outf,"%X OVER %s", SHORT_LIT, ALU);
      break;
    case OP_SHORT_LIT_SWAP_ALU: fprintf(outf,"%X %s", SHORT_LIT, SWAP_ALU);
      break;
    case OP_STAR_TICK:       fprintf(outf,"*' ");  break;
    case OP_STAR_TICK_TICK:  fprintf(outf,"*'' ");  break;
    case OP_STORE:           fprintf(outf,"%s! %s", MEM, INVERT);  break;
    case OP_STORE_LIT:       fprintf(outf,"%s! %X ", MEM, SHORT_LIT);  break;
    case OP_STREAM_MAC:      fprintf(outf,"Stream MAC ");  break;
    case OP_SWAP:            fprintf(outf,"SWAP %s%s", INVERT, SHIFT);  break;
    case OP_S_ONE_TICK:      fprintf(outf,"S1' ");  break;
    case OP_S_TICK:          fprintf(outf,"S' ");  break;
    case OP_S_TICK_TICK:     fprintf(outf,"S'' ");  break;
    case OP_TUCK_ALU:        fprintf(outf,"TUCK %s%s", ALU, SHIFT);  break;
    case OP_TUCK_STORE:      fprintf(outf,"TUCK %s! ", MEM);  break;
    case OP_TUCK_STORE_WITH_ALU:  fprintf(outf,"TUCK %s! %X %s",
                                          MEM, SHORT_LIT, ALU3);  break;
    case OP_TWO_STAR_TICK:   fprintf(outf,"2*' ");  break;
    case OP_UFETCH:          fprintf(outf,"%X U@ %s", SHORT_LIT, INVERT);
      break;
    case OP_UFETCH_OVER_ALU: fprintf(outf,"%X U@ OVER %s", SHORT_LIT, ALU);
      break;
    case OP_UFETCH_SWAP:     fprintf(outf,"%X U@ SWAP %s", SHORT_LIT, INVERT);
      break;
    case OP_UFETCH_SWAP_ALU: fprintf(outf,"%X U@ %s", SHORT_LIT, SWAP_ALU);
      break;
    case OP_UMAC:            fprintf(outf,"Unsigned MAC ");  break;
    case OP_UMULT:           fprintf(outf,"Unsigned Mult ");  break;
    case OP_UNDER_ALU:       fprintf(outf,"OVER %s%s", SWAP_ALU, SHIFT);
      break;
    case OP_UNDER_STORE:     fprintf(outf,"UNDER %s! %s", MEM, INVERT);
      break;
    case OP_UNDER_STORE_LIT: fprintf(outf,"UNDER %s! %X ", MEM, SHORT_LIT);
      break;
    case OP_USLASH_ONE_TICK: fprintf(outf,"U/1' ");  break;
    case OP_USLASH_ONE_TICK_TICK: fprintf(outf,"U/1'' ");  break;
    case OP_USLASH_TICK:     fprintf(outf,"U/' ");  break;
    case OP_USLASH_TICK_TICK:fprintf(outf,"U/'' ");  break;
    case OP_USTAR_TICK:      fprintf(outf,"U*' ");  break;
    case OP_USTAR_TICK_TICK: fprintf(outf,"U*'' ");  break;
    case OP_USTORE:          fprintf(outf,"%X U! %s", SHORT_LIT, INVERT);
      break;
    case OP_ZERO_EQUAL:      fprintf(outf,"0= ");  break;
    default: error("illegal case in print_instruction");
    }
  if (  instruction & 0x0020
        && opr != OP_0BRANCH
        && opr != OP_BRANCH
        && opr != OP_CALL
        && opr != OP_QDUP_0BRANCH
        && opr != OP_NEXT
        && opr != OP_RESERVED
        && opr != OP_RESERVED_STEP_MATH)
    { fprintf(outf," EXIT ");  }
}
