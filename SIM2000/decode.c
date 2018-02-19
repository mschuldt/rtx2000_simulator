/* DECODE.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* This file contains routines to crack instructions down
 * into enums at the level of what the major operation and
 * stack effect is.  Alu, shift, invert, and exit bits are not
 * decoded
 */

#include "main.h"
#include "decode.h"

/* ®MDBO¯---------------------------------------------------®MDNM¯ */

/* Compute branch target address
 *   Input: instruction = 16-bit RTX instruction
 *          address = 16-bit RTX address
 *  Output: 16-bit RTX branch target address
 */
int target_addr(int instruction, int address)
{ int next_addr;
  next_addr = ((address + 1) & 0xFC00) | ( (instruction <<1 ) & 0x03FE)  ;
  switch (instruction & 0x0600)
    {
    case 0x0000:  /* same memory block */
      return(next_addr);
    case 0x0200:  /* next memory block */
      return(next_addr + 0x0400);
    case 0x0400:  /* memory block 0 */
      return(next_addr & 0x03FE );
    case 0x0600:  /* previous memory block */
      return(next_addr - 0x0400);
    }
  error("case statement error in target_addr");
}

/* ®MDUL¯_____________________________________________________________®MDNM¯ */


/* Decode given that the instruction is known to be BRANCH class,
 *  returning an enum corresponding to the instruction type */
machine_op decode_branch (register int instruction)
{ /* 100x xbba aaaa aaaa */
  switch( instruction & 0xF800 )  /* which type of branch instruction */
    {
    case 0x8000:  return(OP_QDUP_0BRANCH);
    case 0x8800:  return(OP_0BRANCH);
    case 0x9000:  return(OP_BRANCH);
    case 0x9800:  return(OP_NEXT);
    }
  error("illegal case in decode_branch");
}

/* ®MDBO¯---------------------------------------------------®MDUL¯®MDNM¯ */

/* Decode given that the instruction is known to be ALU class,
 *  returning an enum corresponding to the instruction type */
machine_op decode_alu (register int instruction)
{ /* 1010 xxxx xxx0 xxxx */

  switch( instruction & 0x00C0 )
    {
    case 0x0000:  /* 1010 xxxx 00x0 xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_SHIFT);         /* 1010 000x 00x0 xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_DROP_DUP);      /* 1010 111x 00x0 xxxx */ }
      return(OP_UNDER_ALU);     /* 1010 cccc 00x0 xxxx */

    case 0x0040:  /* 1010 xxxx 01x0 xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_NIP);           /* 1010 000x 01x0 xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_DROP);          /* 1010 111x 01x0 xxxx */ }
      return(OP_ALU);           /* 1010 cccc 01x0 xxxx */

    case 0x0080:  /* 1010 xxxx 10x0 xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_NIP_DUP);       /* 1010 000x 10x0 xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_SWAP);          /* 1010 111x 10x0 xxxx */ }
      return(OP_TUCK_ALU);      /* 1010 cccc 10x0 xxxx */

    case 0x00C0:  /* 1010 xxxx 11x0 xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_DUP);           /* 1010 000x 11x0 xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_OVER);          /* 1010 111x 11x0 xxxx */ }
      return(OP_DDUP_ALU);      /* 1010 cccc 11x0 xxxx */
    }
  error("illegal case in decode_alu");
}

/* ®MDBO¯---------------------------------------------------®MDUL¯®MDNM¯ */

/* Decode given that the instruction is known to be STEP MATH class,
 *  returning an enum corresponding to the instruction type */
machine_op decode_step_math (register int instruction)
{ /* 1010 xxxx xxx1 xxxx */

  switch( instruction & 0xFFDF )
    { /* recognize only fully supported/documented step math operations */
    case 0xA012:  return(OP_TWO_STAR_TICK);        /* 2*' */
    case 0xA096:  return(OP_RTR);                  /* RTR */
    case 0xA09E:  return(OP_RDR);                  /* RDR */
    case 0xA196:  return(OP_R_TICK);               /* R' */
    case 0xA412:  return(OP_BUSLASH_TICK);         /* BU/' */
    case 0xA418:  return(OP_USLASH_ONE_TICK_TICK); /* U/1'' */
    case 0xA41A:  return(OP_USLASH_ONE_TICK);      /* U/1' */
    case 0xA458:  return(OP_USLASH_TICK_TICK);     /* U/'' */
    case 0xA45A:  return(OP_USLASH_TICK);          /* U/' */
    case 0xA494:  return(OP_BUSTAR_TICK_TICK);     /* BU*'' */
    case 0xA49C:  return(OP_USTAR_TICK_TICK);      /* U*'' */
    case 0xA49D:  return(OP_STAR_TICK_TICK);       /* *'' */
    case 0xA512:  return(OP_BS_ONE_TICK);          /* BS1' */
    case 0xA51A:  return(OP_S_ONE_TICK);           /* S1' */
    case 0xA552:  return(OP_BS_TICK);              /* BS' */
    case 0xA558:  return(OP_S_TICK_TICK);          /* S'' */
    case 0xA55A:  return(OP_S_TICK);               /* S' */
    case 0xA894:  return(OP_BUSTAR_TICK);          /* BU*' */
    case 0xA89C:  return(OP_USTAR_TICK);           /* U*' */
    case 0xA89D:  return(OP_STAR_TICK);            /* *' */
    case 0xAADE:  return(OP_C_TICK);               /* C' */
    }
  return(OP_RESERVED_STEP_MATH);
}

/* ®MDBO¯---------------------------------------------------®MDUL¯®MDNM¯ */

/* Check for "special" instructions given that the instruction
 * is known to be GBUS class, returning an enum corresponding
 * to the instruction type if found, otherwise returning 0. */
machine_op decode_special (register int instruction)
{ /* 1011 xxxx xxxx xxxx */

  if (which_chip == RTX2000 )
    { switch( instruction & 0xFFDF )
        {
        case 0xB00D: return(OP_SELECT_CPR);
        case 0xB010: return(OP_CLEAR_SOFTINT);
        case 0xB08D: return(OP_SELECT_DPR);
        case 0xB090: return(OP_SET_SOFTINT);
        case 0xB096: return(OP_UMULT);
        case 0xB097: return(OP_MULT);
        }
      return(NIL);
    }

  if (which_chip == RTX2001A )
    { switch( instruction & 0xFFDF )
        {
        case 0xB00D: return(OP_SELECT_CPR);
        case 0xB010: return(OP_CLEAR_SOFTINT);
        case 0xB08D: return(OP_SELECT_DPR);
        case 0xB090: return(OP_SET_SOFTINT);
        case 0xB016: return(OP_DEC_RX);
        case 0xB096: return(OP_INC_RX);
        }
      return(NIL);
    }

  if (which_chip == RTX2010 )
    { switch( instruction & 0xFFDF )
        {
        case 0xB00D: return(OP_SELECT_CPR);
        case 0xB010: return(OP_CLEAR_SOFTINT);
        case 0xB08D: return(OP_SELECT_DPR);
        case 0xB090: return(OP_SET_SOFTINT);
        case 0xB008: return(OP_ZERO_EQUAL);
        case 0xB009: return(OP_DSRA);
        case 0xB00A: return(OP_DSRL);
        case 0xB00C: return(OP_CLEAR_ACC);
        case 0xB00E: return(OP_DSLL);
        case 0xB00F: return(OP_NORMALIZE);
        case 0xB011: return(OP_SHIFT_MAC_RIGHT);
        case 0xB012: return(OP_STREAM_MAC);
        case 0xB013: return(OP_MIXED_MULT);
        case 0xB014: return(OP_MULT_SUB);
        case 0xB015: return(OP_MIXED_MAC);
        case 0xB016: return(OP_UMAC);
        case 0xB017: return(OP_MAC);
        case 0xB092: return(OP_ASIC_STREAM_MAC);
        case 0xB096: return(OP_UMULT);
        case 0xB097: return(OP_MULT);
        }
      return(NIL);
    }
  return(NIL);
}

/* Decode given that the instruction is known to be GBUS class,
 *  returning an enum corresponding to the instruction type */
machine_op decode_gbus (register int instruction)
{ /* 1011 xxxx xxxx xxxx */
  machine_op temp;
  int middle_flag ;

  if ( (instruction & 0xFF40) == 0xB000 )
    { temp = decode_special(instruction);
      if (temp) return(temp);
    }

  middle_flag = FALSE;
  if (  (instruction & 0x001F) > 0x07
        && (instruction & 0x001F) < 0x18 )  middle_flag = TRUE ;


  switch( instruction & 0x00C0 )
    {
    case 0x0000:  /* 1011 xxxx 00xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  if (middle_flag)  return(OP_RESERVED);
          return(OP_GFETCH_DROP);     /* 1011 000x 00xg gggg */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_GFETCH);          /* 1011 111x 00xg gggg */ }
      if (middle_flag)  return(OP_RESERVED);
      return(OP_GFETCH_OVER_ALU); /* 1011 cccc 00xg gggg */

    case 0x0080:  /* 1011 xxxx 10xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        { if (middle_flag)  return(OP_RESERVED);
          return(OP_DUP_GSTORE);      /* 1011 000x 10xg gggg */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_GSTORE);          /* 1011 111x 10xg gggg */ }
      if (middle_flag && ((instruction & 0x1f) != 0x11)
          && ((instruction & 0x1f) != 0x09))
        return(OP_RESERVED);
      return(OP_GFETCH_SWAP_ALU); /* 1011 cccc 10xg gggg */
    }
  error("illegal case in decode_gbus");
}

/* ®MDBO¯---------------------------------------------------®MDUL¯®MDNM¯ */


/* Decode given that the instruction is known to be SHORT LIT class,
 *  returning an enum corresponding to the instruction type */
machine_op decode_short_lit (register int instruction)
{ /* 1011 xxxx x1xx xxxx  --- short_lit */

  switch( instruction & 0x00C0 )
    {
    case 0x0040:  /* 1011 xxxx 01xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_RESERVED);        /* 1011 000x 01xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_SHORT_LIT);       /* 1011 111x 01xd dddd */ }
      return(OP_SHORT_LIT_OVER_ALU); /* 1011 cccc 01xd dddd */

    case 0x00C0:  /* 1011 xxxx 11xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_RESERVED);        /* 1011 000x 11xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_DROP_SHORT_LIT);     /* 1011 111x 11xd dddd */ }
      return(OP_SHORT_LIT_SWAP_ALU); /* 1011 cccc 11xd dddd */
    }
  error("illegal case in decode_short_lit");
}

/* ®MDBO¯---------------------------------------------------®MDUL¯®MDNM¯ */

/* Decode given that the instruction is known to be USER MEMORY class,
 *  returning an enum corresponding to the instruction type */
machine_op decode_user (register int instruction)
{ /* 1100 xxxx x0xx xxxx  --- U@/U! */

  switch( instruction & 0x00C0 )
    {
    case 0x0000:  /* 1100 xxxx 00xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_UFETCH_SWAP);     /* 1100 000x 00xu uuuu */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_UFETCH);          /* 1100 111x 00xu uuuu */ }
      return(OP_UFETCH_OVER_ALU); /* 1100 cccc 00xu uuuu */

    case 0x0080:  /* 1100 xxxx 10xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_DUP_USTORE);      /* 1100 000x 10xu uuuu */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_USTORE);          /* 1100 111x 10xu uuuu */ }
      return(OP_UFETCH_SWAP_ALU); /* 1100 cccc 10xu uuuu */
    }
  return(OP_RESERVED);
}

/* ®MDBO¯---------------------------------------------------®MDUL¯®MDNM¯ */

/* Decode given that the instruction is known to be LONG LITERAL class,
 *  returning an enum corresponding to the instruction type */
machine_op decode_long_lit (register int instruction)
{ /* 1101 xxxx xxxx xxxx  --- long literal */

  if (instruction & 0x001F) return(OP_RESERVED);   /* 1101 xxxx x1xy yyyy */

  switch( instruction & 0x00C0 )
    {
    case 0x0000:  /* 1101 xxxx 00xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_LIT_SWAP);        /* 1101 000x 00xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_LIT);             /* 1101 111x 00xx xxxx */ }
      return(OP_LIT_OVER_ALU);    /* 1101 cccc 00xx xxxx */

    case 0x0080:  /* 1101 xxxx 10xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_RESERVED);        /* 1101 000x 10xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_DROP_LIT);        /* 1101 111x 10xx xxxx */ }
      return(OP_LIT_SWAP_ALU);    /* 1101 cccc 10xx xxxx */
    }
  return(OP_RESERVED);        /* 1101 xxxx x1xx xxxx */
}

/* ®MDBO¯---------------------------------------------------®MDUL¯®MDNM¯ */

/* Decode given that the instruction is known to be MEMORY class,
 *  returning an enum corresponding to the instruction type */
machine_op decode_memory (register int instruction)
{ /* 111x xxxx xxxx xxxx  --- memory access */

  switch( instruction & 0x01C0 )
    {
    case 0x0000:  /* 111x xxx0 00xx xxxx */
    case 0x0100:  /* 111x xxx1 00xx xxxx */
      if (instruction & 0x001F)  return(OP_RESERVED); /* low 5 bits not 0 */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_FETCH_SWAP);      /* 111x 000x 00xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_FETCH);           /* 111x 111x 00xx xxxx */ }
      return(OP_FETCH_OVER_ALU);  /* 111x cccc 00xx xxxx */

    case 0x0040:  /* 111x xxx0 01xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  if (instruction & 0x001F)
            return(OP_RESERVED); /* low 5 bits not 0 */
          return(OP_NIP_DUP_FETCH_SWAP);     /* 111x 0000 01xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_NIP_FETCH_LIT);          /* 111x 1110 01xd dddd */ }
      /* NIP DUP @ SWAP d SWAP ALU */
      return(OP_NIP_FETCH_WITH_ALU);     /* 111x aaa0 01xx xxxx */

    case 0x0140:  /* 111x xxx1 01xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  if (instruction & 0x001F)
            return(OP_RESERVED); /* low 5 bits not 0 */
          return(OP_DUP_FETCH_SWAP);     /* 111x 0001 01xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_FETCH_LIT);          /* 111x 1111 01xd dddd */ }
      /* DUP @ SWAP d SWAP ALU */
      return(OP_FETCH_WITH_ALU);     /* 111x aaa1 01xx xxxx */

    case 0x0080:  /* 111x xxx0 10xx xxxx */
    case 0x0180:  /* 111x xxx1 10xx xxxx */
      if (instruction & 0x001F)  return(OP_RESERVED); /* low 5 bits not 0 */
      if (  (instruction & 0x0E00) == 0x0000 )
        {  return(OP_UNDER_STORE); /* 111x 000x 10xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_STORE);           /* 111x 111x 10xx xxxx */ }
      return(OP_FETCH_SWAP_ALU);  /* 111x cccc 10xx xxxx */

    case 0x00C0:  /* 111x xxx0 11xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        { if (instruction & 0x001F)
            return(OP_RESERVED); /* low 5 bits not 0 */
          return(OP_DDUP_STORE);           /* 111x 0000 11xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_UNDER_STORE_LIT);      /* 111x 1110 11xd dddd */ }
      /* DDUP ! d SWAP alu */
      return(OP_DDUP_STORE_WITH_ALU);  /* 111x aaa0 11xx xxxx */

    case 0x01C0:  /* 111x xxx1 11xx xxxx */
      if (  (instruction & 0x0E00) == 0x0000 )
        { if (instruction & 0x001F)
            return(OP_RESERVED); /* low 5 bits not 0 */
          return(OP_TUCK_STORE);           /* 111x 0001 11xx xxxx */ }
      if  ( (instruction & 0x0E00) == 0x0E00 )
        {  return(OP_STORE_LIT);            /* 111x 1111 11xd dddd */ }
      /* TUCK ! d SWAP alu */
      return(OP_TUCK_STORE_WITH_ALU);  /* 111x aaa1 11xx xxxx */
    }
  error("illegal case in decode_memory");
}

/* ®MDBO¯---------------------------------------------------®MDUL¯®MDNM¯ */

/* Driver for instruction operation cracking.
 *   Input: 16-bit RTX instruction
 *   Output: enum for operation to be performed (exclusive of alu/shift op).
 */
machine_op decode (register int instruction)
{
  if ( instruction & 0x8000 )
    { /* non-call instructions */
      switch ( instruction & 0xF000 )  /* 3 bits of top 4 */
        {
        case 0x8000:  /* Branch */
        case 0x9000:  /* Branch */
          return(decode_branch(instruction));

        case 0xA000:  /* ALU */
          if ( instruction & 0x0010 )
            { return(decode_step_math(instruction)); }
          return(decode_alu(instruction));

        case 0xB000:  /* G@/G! or short_lit */
          if ( instruction & 0x0040 )
            { return(decode_short_lit(instruction)); }
          return(decode_gbus(instruction));

        case 0xC000:  /* U@/U! */
          return(decode_user(instruction));

        case 0xD000:  /* long literal */
          return(decode_long_lit(instruction));

        case 0xE000:  /* word memory access*/
        case 0xF000:  /* byte memory access*/
          return(decode_memory(instruction));

        default:  error("decode 0-7");
        }
    }
  else
    { /* call instruction */
      return (OP_CALL);
    }
  error("decode fall-through");
}

/* ®MDBO¯---------------------------------------------------®MDUL¯®MDNM¯ */
