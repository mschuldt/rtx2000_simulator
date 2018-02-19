/* EXECUTE.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* This file contains routines to implement the instructions.
 * Each operation enum has its own execution routine.
 */

#include <stdio.h>
#include "main.h"
#include "state.h"
#include "decode.h"
#include "output.h"
#include "execute.h"

#define SHORT_LIT (IR & 0x001F)

/* .MDBO/---------------------------------------------------.MDNM/ */

#define invert()  {if (IR & 0x0100)  TOP = (~TOP) _MASKED_ ;}

/* ??? the below definitions might speed things up,  from Mitch Bradley */
/* #define CARRY(a,b)   ((unsigned)a >= (unsigned)(-b)) */
/* #define BORROW(a,b)  ((unsigned)a <  (unsigned)b)    */
/* 16-bit versions that promote to longs */
int CARRY(int a, int b, int c)
{ LONG sum,temp;   /* sum=a+b+c */
  /* the additions below use high 16 bits of the long to catch carries */
  temp = a ;  sum = temp & 0xFFFF ;
  temp = b ;  sum = sum + (temp & 0xFFFF);
  temp = c ;  sum = sum + (temp & 0xFFFF);
  if (sum & 0x10000)  return(1);
  return(0);
}
#define BORROW(a,b,c)  CARRY(a,~b,c)


int alu(int opa, int opb)
{switch(IR & 0x0F00)
    {
    case 0x0200: return(  (opb & opa)  _MASKED_ );
    case 0x0300: return((~(opb | opa)) _MASKED_ );
    case 0x0400: set_CY( BORROW(opa,opb,1) );
      return((opa -  opb) _MASKED_      );
    case 0x0500: set_CY( BORROW(opa,opb,CY) );
      return((opa + ~opb + CY) _MASKED_ );
    case 0x0600: return(  (opb | opa) _MASKED_ );
    case 0x0700: return((~(opb & opa)) _MASKED_ );
    case 0x0800: set_CY( CARRY(opa,opb,0) );
      return((opa + opb) _MASKED_      );
    case 0x0900: set_CY( CARRY(opa,opb,CY) );
      return((opa + opb + CY) _MASKED_ );
    case 0x0A00: return(  (opb ^ opa) _MASKED_ );
    case 0x0B00: return((~(opb ^ opa)) _MASKED_ );
    case 0x0C00: set_CY( BORROW(opb,opa,1) );
      return((opb -  opa) _MASKED_      );
    case 0x0D00: set_CY( BORROW(opb,opa,CY) );
      return((opb + ~opa + CY) _MASKED_ );
    }
  error("illegal alu function in alu");
}

void shift()
{ int temp, tempa;
  /* compute shift function */
  switch( IR & 0x000F )
    {
    case 0x0000:  /* nop */
      break;

    case 0x0001:  /* 0< */
      if ( TOP & 0x8000 )  { TOP = 0xFFFF ; }
      else                 { TOP = 0x0000 ; }
      break;

    case 0x0002:  /* 2* */
      set_CY( TOP & 0x8000 );
      TOP = (TOP << 1) & 0xFFFE ;
      break;

    case 0x0003:  /* 2*c */
      temp = (TOP << 1) & 0xFFFE ;
      if (CY)  temp = temp | 1;
      set_CY( TOP & 0x8000 );
      TOP = temp ;
      break;

    case 0x0004:  /* cU2/ */
      TOP =  (TOP >> 1) & 0x7FFF ;
      if (CY)  TOP = TOP | 0x8000 ;
      set_CY_0;
      break;

    case 0x0005:  /* c2/ */
      temp =  (TOP >> 1) & 0x7FFF ;
      if (CY)  temp = temp | 0x8000 ;
      set_CY(TOP & 1);
      TOP = temp;
      break;

    case 0x0006:  /* U2/ */
      TOP =  (TOP >> 1) & 0x7FFF ;
      set_CY_0;
      break;

    case 0x0007:  /* 2/ */
      temp =  (TOP >> 1) & 0x7FFF ;
      if (TOP & 0x8000)  temp = temp | 0x8000 ;
      set_CY(TOP & 0x8000);
      TOP = temp;
      break;

    case 0x0008:  /* N2* */
      NEXT = (NEXT <<1) & 0xFFFE ;
      break;

    case 0x0009:  /* N2*c */
      NEXT = (NEXT <<1) & 0xFFFE ;
      if (CY)  NEXT = NEXT | 1 ;
      break;

    case 0x000A:  /* D2* */
      set_CY( TOP & 0x8000 );
      TOP  = (TOP  <<1) & 0xFFFE ;
      if (NEXT & 0x8000 )  TOP = TOP | 1;
      NEXT = (NEXT <<1) & 0xFFFE ;
      break;

    case 0x000B:  /* D2*C */
      temp = NEXT & 0x8000 ;
      tempa = TOP & 0x8000 ;
      NEXT = (NEXT <<1) & 0xFFFE ;
      if (CY)  NEXT = NEXT | 1 ;
      TOP  = (TOP  <<1) & 0xFFFE ;
      if (temp)  TOP = TOP | 1 ;
      set_CY(tempa);
      break;

    case 0x000C:  /* cUD2/ */
      temp = TOP & 0x0001 ;
      TOP  = (TOP >>1) & 0x7FFF ;
      if (CY)  TOP = TOP | 0x8000 ;
      NEXT = (NEXT >>1) & 0x7FFF ;
      if (temp)  NEXT = NEXT | 0x8000 ;
      set_CY_0;
      break;

    case 0x000D:  /* cD2/ */
      temp = TOP & 0x0001 ;
      tempa = NEXT & 0x0001 ;
      TOP  = (TOP >>1) & 0x7FFF ;
      if (CY)  TOP = TOP | 0x8000 ;
      NEXT = (NEXT >>1) & 0x7FFF ;
      if (temp)  NEXT = NEXT | 0x8000 ;
      set_CY(tempa);
      break;

    case 0x000E:  /* UD2/ */
      temp = TOP & 0x0001 ;
      TOP  = (TOP >>1) & 0x7FFF ;
      NEXT = (NEXT >>1) & 0x7FFF ;
      if (temp)  NEXT = NEXT | 0x8000 ;
      set_CY_0;
      break;

    case 0x000F:  /* D2/ */
      temp = TOP & 0x8000 ;
      tempa = TOP & 0x0001 ;
      TOP  = (TOP >>1) & 0x7FFF ;
      TOP = TOP | temp ;
      set_CY(temp);
      NEXT = (NEXT >>1) & 0x7FFF ;
      if (tempa)  NEXT = NEXT | 0x8000 ;
      break;

    default:  error("illegal shift case in shift");
    }
}

/* .MDUL/________________________________________________________________.MDNM/ */


void bad_insn()
{  printf("Unsupported instruction: addr=%01X%04X  insn=%04X ",
          CPR, PC, IR);
  print_instruction(stdout, decode(IR), IR, CPR, PC);
  printf("\n");
}

void D_swap()         /* D swap */
{ push(TOP);
  NEXT = long_fetch(CPR, PC);   INC_PC;
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void fetch_swap()     /* @ swap */
{ int temp;
  temp = NEXT;
  NEXT = fetch(TOP);
  TOP = temp;
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void ufetch_swap()    /* U@ swap */
{ push( ufetch(SHORT_LIT) );
  invert();
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void tuck_alu_2()
{ int temp;
  temp = TOP;
  TOP = alu(TOP, NEXT);
  NEXT = temp;
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#if NEVER
void step_math()
{ unsigned LONG W, alu_out;
  int alu_y_source, YES, arithmetic;

  if ( IR & 0x0100 )  /* test r bit of instruction */
    {  alu_y_source = SQ; }
  else  {  alu_y_source = MD; }

  arithmetic = FALSE;
  switch( IR & 0x0E00 )  /* select ALU function */
    { case 0x0000: alu_out = (unsigned) TOP;  break;
    case 0x0200: alu_out = (unsigned) TOP & (unsigned) alu_y_source ;  break;
    case 0x0400: alu_out = (unsigned) TOP - (unsigned) alu_y_source ;
      arithmetic = TRUE; break;
    case 0x0600: alu_out = (unsigned) TOP | (unsigned) alu_y_source ;  break;
    case 0x0800: alu_out = (unsigned) TOP + (unsigned) alu_y_source ;
      arithmetic = TRUE; break;
    case 0x0A00: alu_out = (unsigned) TOP ^ (unsigned) alu_y_source ;  break;
    case 0x0C00: alu_out = (unsigned) TOP - (unsigned) alu_y_source ;
      arithmetic = TRUE; break;
    case 0x0E00: alu_out =                  (unsigned) alu_y_source ;  break;
    default: error("bad alu case in step_math");
    }

  switch( IR & 0x00C0 )  /* compute value of YES */
    { case 0x0000: if (arithmetic)  { YES =  ( (alu_out & 0x10000) ? 1 : 0 ); }
      else   { YES =  ( (IR      & 0x0001)  ? 1 : 0 ); }
        break;
    case 0x0040: if (arithmetic)  { YES =  ( (alu_out & 0x10000) ? 1 : 0 ); }
        else   { YES =  ( (IR      & 0x0001)  ? 1 : 0 ); }
      YES = YES | CR1 ;
      break;
    case 0x0080: if (IR & 0x0008) { YES = NEXT & 1; }
      else { YES = TOP  & 1; }
      break;
    case 0x00C0: YES = (TOP & 1) ^ (NEXT & 1);
      break;
    default: error("bad YES case in step_math");
    }

  if (YES)
    { switch( IR & 0x0E00 )  /* select ALU function */
        { case 0x0000:   W = (unsigned) TOP;  break;
        case 0x0200:   W = (unsigned) TOP & (unsigned) alu_y_source ;  break;
        case 0x0400:   W = (unsigned) TOP - (unsigned) alu_y_source ;  break;
        case 0x0600:   W = (unsigned) TOP | (unsigned) alu_y_source ;  break;
        case 0x0800:   W = (unsigned) TOP + (unsigned) alu_y_source ;  break;
        case 0x0A00:   W = (unsigned) TOP ^ (unsigned) alu_y_source ;  break;
        case 0x0C00:   W = (unsigned) TOP - (unsigned) alu_y_source ;  break;
        case 0x0E00:   W =                  (unsigned) alu_y_source ;  break;
        default: error("bad case in step_math");
        }
    }
  else
    {  /* not YES */
      W =  ;
    }

  if ( IR & 0x0100 )  /* test r bit of instruction */
    { if (YES) MD = MD | SR;
      SR = (SR >> 1)  & 0x7FFF;
    }

  TEST_EXIT; CLOCKS(1); }

#endif

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_exit()
{ CPR = IPR & 0x0F ;
  set_DPRSEL(IPR & 0x10);
  PC = INDEX;
  rs_pop();
}

void do_call()
{ rs_push(CPR, PC);
  PC = (IR <<1) _MASKED_;
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_0branch()
{ if ( !TOP )  { PC = target_addr(IR, PC); }
  pop();
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_alu()
{ NEXT = alu(TOP, NEXT);
  pop();
  invert(); shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_asic_stream_mac()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_branch()
{ PC = target_addr(IR, PC);
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_bs_one_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_bs_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_buslash_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_bustar_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_bustar_tick_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_clear_acc()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_clear_softint()
{ SOFTINT = FALSE;
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_c_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_ddup_alu()
{ push(NEXT);
  TOP = alu(NEXT, TOP);
  invert(); shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_ddup_store()
{ store(TOP, NEXT);
  TEST_EXIT; CLOCKS(1); second_cycle = TRUE; }

void do_ddup_store_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_ddup_store_with_alu()
{ store(TOP, NEXT);
  TOP = alu(TOP, SHORT_LIT);
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE; }

void do_ddup_store_with_alu_2()
{ CLOCKS(1);}

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_dec_rx()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_drop()
{ pop();
  invert(); shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_drop_dup()
{ TOP = NEXT;
  invert(); shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_drop_lit   D_swap

void do_drop_lit_2()
{ pop();
  invert();  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_drop_short_lit()
{
  TOP = SHORT_LIT;
  invert();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_dsll()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_dsra()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_dsrl()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_dup()
{ push(TOP);
  invert(); shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_dup_fetch_swap()
{ push(TOP);
  NEXT = fetch(TOP);
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE; }

void do_dup_fetch_swap_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_dup_gstore()
{ if (SHORT_LIT != 7) TEST_EXIT;  /* important that TEST_EXIT come before gstore! */
  gstore(SHORT_LIT, TOP);
  invert();
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_dup_ustore()
{ ustore(SHORT_LIT, TOP);
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_dup_ustore_2()
{ invert(); CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_fetch    fetch_swap

void do_fetch_2()  /* SWAP {inv} */
{ int temp;
  temp = TOP;  TOP = NEXT;  NEXT = temp;
  invert(); CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_fetch_lit()
{ push(SHORT_LIT);
  push(fetch(NEXT));
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_fetch_lit_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_fetch_over_alu  fetch_swap

void do_fetch_over_alu_2()  /* TUCK alu */
{ int temp;
  temp = TOP;
  TOP = alu(TOP, NEXT);
  NEXT = temp;
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_fetch_swap      fetch_swap

void do_fetch_swap_2()
{ invert(); CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_fetch_swap_alu   fetch_swap

void do_fetch_swap_alu_2() /* alu */
{ NEXT = alu(TOP, NEXT);
  pop();
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_fetch_with_alu()
{ push(TOP);
  NEXT = fetch(TOP);
  TOP = alu(TOP, SHORT_LIT);
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_fetch_with_alu_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_gfetch()
{ CLOCKS(1);
  if      (SHORT_LIT == 0x16 && which_chip == RTX2000 ) /* MLR special read */
    { NEXT = TOP;    TOP = gfetch(SHORT_LIT);  }
  else if (SHORT_LIT == 0x17 && which_chip == RTX2000 ) /* MHR special read */
    { NEXT = TOP;    TOP = gfetch(SHORT_LIT);  }
  else { push( gfetch(SHORT_LIT) ); }
  invert();
  TEST_EXIT; }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_gfetch_drop()
{ /* ??? what happens for short_lit = 16, short_lit = 17??? */
  gfetch(SHORT_LIT);
  invert();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_gfetch_over_alu()
{ /* ??? what happens for short_lit = 16, short_lit = 17??? */
  push( alu(TOP, gfetch(SHORT_LIT)) );
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_gfetch_swap_alu()
{ /* ??? what happens for short_lit = 16, short_lit = 17??? */
  TOP = alu(TOP, gfetch(SHORT_LIT));
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_gstore()
{ if (SHORT_LIT != 7) TEST_EXIT;  /* important that TEST_EXIT come before gstore! */
  gstore(SHORT_LIT, TOP);
  if (SHORT_LIT != 9) pop();
  invert();
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_inc_rx()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_lit  D_swap

void do_lit_2()  /* swap {inv} */
{ int temp;
  temp = NEXT ;  NEXT = TOP;  TOP = temp;
  invert(); CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_lit_over_alu   D_swap

#define do_lit_over_alu_2  tuck_alu_2

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_lit_swap    D_swap

void do_lit_swap_2()
{ invert(); CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_lit_swap_alu  D_swap

void do_lit_swap_alu_2()
{ NEXT = alu(TOP, NEXT);
  pop();
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_mac()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_mixed_mac()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_mixed_mult()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_mult()
{ /* ??? needs to inhibit interrupts */
  LONG opa, opb, product;
  opa = TOP;  opb = NEXT;
  product = opa * opb;
  MLR = product _MASKED_ ;
  MHR = (product>>16) _MASKED_ ;
  invert(); TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_mult_sub()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_next()
{
  if (INDEX)
    { /* loop */
      PC = target_addr(IR, PC);
      INDEX = (INDEX - 1) _MASKED_ ;
    }
  else
    { /* fall through */
      rs_pop();
    }
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_nip()
{ NEXT = TOP;
  pop();
  invert();  shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_nip_dup()
{ NEXT = TOP;
  invert();  shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_nip_dup_fetch_swap()
{ NEXT = TOP;
  push(TOP);
  NEXT = fetch(TOP);
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_nip_dup_fetch_swap_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_nip_fetch_lit()
{ NEXT = fetch(TOP);
  TOP = SHORT_LIT;
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_nip_fetch_lit_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_nip_fetch_with_alu()
{ NEXT = fetch(TOP);
  TOP = alu(TOP, SHORT_LIT);
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_nip_fetch_with_alu_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_normalize()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_over()
{ push(NEXT);
  invert();  shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_qdup_0branch()
{
  if (!TOP)
    { PC = target_addr(IR, PC);
      pop();
    }
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_rdr()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_reserved()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_reserved_step_math()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_rtr()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_r_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_select_cpr()
{ set_DPRSEL_0;
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_select_dpr()
{ set_DPRSEL_1;
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_set_softint()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_shift()
{ invert();  shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_shift_mac_right()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_short_lit()
{ push(SHORT_LIT);
  invert();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_short_lit_over_alu()
{ push( alu(TOP, SHORT_LIT));
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_short_lit_swap_alu()
{ TOP = alu(TOP, SHORT_LIT);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_star_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_star_tick_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_store()
{ store(TOP, NEXT);
  pop();
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_store_2()
{ pop();
  invert();  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_store_lit()
{ store(TOP, NEXT);
  pop();
  TOP = SHORT_LIT;
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_store_lit_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_stream_mac()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_swap()
{ int temp;
  temp = TOP;
  TOP = NEXT ;
  NEXT = temp;
  invert();  shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_s_one_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_s_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_s_tick_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_tuck_alu()
{ int temp;
  temp = NEXT;
  NEXT = TOP;
  TOP = alu(TOP, temp);
  shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_tuck_store()
{ store(TOP, NEXT);
  NEXT = TOP;
  pop();
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_tuck_store_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_tuck_store_with_alu()
{ store(TOP, NEXT);
  NEXT = TOP;  pop();
  TOP = alu(TOP, SHORT_LIT);
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_tuck_store_with_alu_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_two_star_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_ufetch    ufetch_swap

void do_ufetch_2()
{ int temp;
  temp = TOP;  TOP = NEXT;  NEXT = temp;
  invert();  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_ufetch_over_alu   ufetch_swap

#define do_ufetch_over_alu_2  tuck_alu_2

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_ufetch_swap    ufetch_swap

void do_ufetch_swap_2()
{ invert(); CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

#define do_ufetch_swap_alu   ufetch_swap

void do_ufetch_swap_alu_2()
{ NEXT = alu(TOP, NEXT);
  pop();
  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_umac()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_umult()
{ /* ??? needs to inhibit interrupts */
  unsigned LONG opa, opb, product;
  opa = (unsigned) TOP;  opb = (unsigned) NEXT;
  product = opa * opb;
  MLR = product _MASKED_ ;
  MHR = (product>>16) _MASKED_ ;
  invert(); TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_under_alu()
{ TOP = alu(TOP, NEXT);
  shift();
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_under_store()
{ store(TOP, NEXT);
  pop();
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_under_store_2()
{ invert();  CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_under_store_lit()
{ store(TOP, NEXT);
  TOP = SHORT_LIT;
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_under_store_lit_2()
{ CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_uslash_one_tick()
{
  /* really sleazy definition to make things work for now ???? */
  /* undoes the preceeding d2* */
  NEXT = (NEXT >> 1) & 0x7FFF ;
  if (TOP & 1)  NEXT = NEXT | 0x8000;
  TOP =  (TOP >> 1)  & 0x7FFF ;
  if (CY)  TOP = TOP | 0x8000 ;
}

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_uslash_one_tick_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_uslash_tick()
{
  /* really sleazy definition to make things work for now ???? */
  /* do nothing -- save the division up for the end! */
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_uslash_tick_tick()
{
  /* really sleazy definition to make things work for now ???? */
  /* do all the division at once */
  unsigned LONG dividend;
  unsigned int divisor, quotient, remainder;
  dividend = ((unsigned LONG) TOP << 16) & 0xFFFF0000;
  dividend = dividend | (unsigned LONG) (NEXT & 0xFFFF);
  divisor = MD;
  quotient = dividend / divisor ;
  dividend = dividend - ((unsigned LONG) quotient * (unsigned LONG) divisor);
  remainder = dividend;
  TOP = remainder;
  NEXT = quotient;
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_ustar_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_ustar_tick_tick()
{  bad_insn(IR);
  TEST_EXIT; CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_ustore()
{ ustore(SHORT_LIT, TOP);
  TEST_EXIT; CLOCKS(1);  second_cycle = TRUE;  }

void do_ustore_2()
{ pop();
  invert(); CLOCKS(1); }

/* .MDBO/---------------------------------------------------.MDNM/ */

void do_zero_equal()
{ if (TOP)  { TOP = 0; }  else  { TOP = 0xFFFF; }
  TEST_EXIT; CLOCKS(1); }

/* .MDUL/___________________________________________________________________.MDNM/ */

/* Note: it is *very important* that this list be in correct order!!!
 *  We could use a statically initialized data structure in C, but sure
 *  enough someone will add an operation without properly updating the
 *  list, leading to a *real* nasty bug.  So instead, we'll do a
 *  run-time initialization.
 *  Also, the 128 constant is intentionally used to generate a
 *  compile time error if NUMBER_OF_ROUTINES is changed without
 *  updating this initialization structure. */

/* dispatch table for 1st clock cycle */
void (*dispatch_vector_1[128]) () =
  { bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 16 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 32 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 48 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 64 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 80 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 96 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 112 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn   /* 128 */
  };

/* dispatch table for 2nd clock cycle of 2-cycle instructions */
void (*dispatch_vector_2[128]) () =
  { bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 16 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 32 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 48 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 64 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 80 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 96 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,  /* 112 */
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn,
    bad_insn, bad_insn, bad_insn, bad_insn   /* 128 */
  };

void execute(register int inst)
{ IR= inst;
  dispatch_vector_1[decode(inst)]();
}

void execute_2(register int inst)
{ IR= inst;
  dispatch_vector_2[decode(inst)]();
  second_cycle = FALSE;
}

/* .MDBO/---------------------------------------------------.MDNM/ */

void init_dispatch()
{
#if DUMMY_LAST >= NUMBER_OF_ROUTINES
  Compilation error: Increase NUMBER_OF_ROUTINES & array initialization!!
#endif
    /* ---------------- first clock routines */
    dispatch_vector_1[(int) OP_CALL] = do_call;
  dispatch_vector_1[(int) OP_0BRANCH] = do_0branch;
  dispatch_vector_1[(int) OP_ALU] = do_alu;
  dispatch_vector_1[(int) OP_ASIC_STREAM_MAC] = do_asic_stream_mac;
  dispatch_vector_1[(int) OP_BRANCH] = do_branch;
  dispatch_vector_1[(int) OP_BS_ONE_TICK] = do_bs_one_tick;
  dispatch_vector_1[(int) OP_BS_TICK] = do_bs_tick;
  dispatch_vector_1[(int) OP_BUSLASH_TICK] = do_buslash_tick;
  dispatch_vector_1[(int) OP_BUSTAR_TICK] = do_bustar_tick;
  dispatch_vector_1[(int) OP_BUSTAR_TICK_TICK] = do_bustar_tick_tick;
  dispatch_vector_1[(int) OP_CLEAR_ACC] = do_clear_acc;
  dispatch_vector_1[(int) OP_CLEAR_SOFTINT] = do_clear_softint;
  dispatch_vector_1[(int) OP_C_TICK] = do_c_tick;
  dispatch_vector_1[(int) OP_DDUP_ALU] = do_ddup_alu;
  dispatch_vector_1[(int) OP_DDUP_STORE] = do_ddup_store;
  dispatch_vector_1[(int) OP_DDUP_STORE_WITH_ALU] = do_ddup_store_with_alu;
  dispatch_vector_1[(int) OP_DEC_RX] = do_dec_rx;
  dispatch_vector_1[(int) OP_DROP] = do_drop;
  dispatch_vector_1[(int) OP_DROP_DUP] = do_drop_dup;
  dispatch_vector_1[(int) OP_DROP_LIT] = do_drop_lit;
  dispatch_vector_1[(int) OP_DROP_SHORT_LIT] = do_drop_short_lit;
  dispatch_vector_1[(int) OP_DSLL] = do_dsll;
  dispatch_vector_1[(int) OP_DSRA] = do_dsra;
  dispatch_vector_1[(int) OP_DSRL] = do_dsrl;
  dispatch_vector_1[(int) OP_DUP] = do_dup;
  dispatch_vector_1[(int) OP_DUP_FETCH_SWAP] = do_dup_fetch_swap;
  dispatch_vector_1[(int) OP_DUP_GSTORE] = do_dup_gstore;
  dispatch_vector_1[(int) OP_DUP_USTORE] = do_dup_ustore;
  dispatch_vector_1[(int) OP_FETCH] = do_fetch;
  dispatch_vector_1[(int) OP_FETCH_LIT] = do_fetch_lit;
  dispatch_vector_1[(int) OP_FETCH_OVER_ALU] = do_fetch_over_alu;
  dispatch_vector_1[(int) OP_FETCH_SWAP] = do_fetch_swap;
  dispatch_vector_1[(int) OP_FETCH_SWAP_ALU] = do_fetch_swap_alu;
  dispatch_vector_1[(int) OP_FETCH_WITH_ALU] = do_fetch_with_alu;
  dispatch_vector_1[(int) OP_GFETCH] = do_gfetch;
  dispatch_vector_1[(int) OP_GFETCH_DROP] = do_gfetch_drop;
  dispatch_vector_1[(int) OP_GFETCH_OVER_ALU] = do_gfetch_over_alu;
  dispatch_vector_1[(int) OP_GFETCH_SWAP_ALU] = do_gfetch_swap_alu;
  dispatch_vector_1[(int) OP_GSTORE] = do_gstore;
  dispatch_vector_1[(int) OP_INC_RX] = do_inc_rx;
  dispatch_vector_1[(int) OP_LIT] = do_lit;
  dispatch_vector_1[(int) OP_LIT_OVER_ALU] = do_lit_over_alu;
  dispatch_vector_1[(int) OP_LIT_SWAP] = do_lit_swap;
  dispatch_vector_1[(int) OP_LIT_SWAP_ALU] = do_lit_swap_alu;
  dispatch_vector_1[(int) OP_MAC] = do_mac;
  dispatch_vector_1[(int) OP_MIXED_MAC] = do_mixed_mac;
  dispatch_vector_1[(int) OP_MIXED_MULT] = do_mixed_mult;
  dispatch_vector_1[(int) OP_MULT] = do_mult;
  dispatch_vector_1[(int) OP_MULT_SUB] = do_mult_sub;
  dispatch_vector_1[(int) OP_NEXT] = do_next;
  dispatch_vector_1[(int) OP_NIP] = do_nip;
  dispatch_vector_1[(int) OP_NIP_DUP] = do_nip_dup;
  dispatch_vector_1[(int) OP_NIP_DUP_FETCH_SWAP] = do_nip_dup_fetch_swap;
  dispatch_vector_1[(int) OP_NIP_FETCH_LIT] = do_nip_fetch_lit;
  dispatch_vector_1[(int) OP_NIP_FETCH_WITH_ALU] = do_nip_fetch_with_alu;
  dispatch_vector_1[(int) OP_NORMALIZE] = do_normalize;
  dispatch_vector_1[(int) OP_OVER] = do_over;
  dispatch_vector_1[(int) OP_QDUP_0BRANCH] = do_qdup_0branch;
  dispatch_vector_1[(int) OP_RDR] = do_rdr;
  dispatch_vector_1[(int) OP_RESERVED] = do_reserved;
  dispatch_vector_1[(int) OP_RESERVED_STEP_MATH] = do_reserved_step_math;
  dispatch_vector_1[(int) OP_RTR] = do_rtr;
  dispatch_vector_1[(int) OP_R_TICK] = do_r_tick;
  dispatch_vector_1[(int) OP_SELECT_CPR] = do_select_cpr;
  dispatch_vector_1[(int) OP_SELECT_DPR] = do_select_dpr;
  dispatch_vector_1[(int) OP_SET_SOFTINT] = do_set_softint;
  dispatch_vector_1[(int) OP_SHIFT] = do_shift;
  dispatch_vector_1[(int) OP_SHIFT_MAC_RIGHT] = do_shift_mac_right;
  dispatch_vector_1[(int) OP_SHORT_LIT] = do_short_lit;
  dispatch_vector_1[(int) OP_SHORT_LIT_OVER_ALU] = do_short_lit_over_alu;
  dispatch_vector_1[(int) OP_SHORT_LIT_SWAP_ALU] = do_short_lit_swap_alu;
  dispatch_vector_1[(int) OP_STAR_TICK] = do_star_tick;
  dispatch_vector_1[(int) OP_STAR_TICK_TICK] = do_star_tick_tick;
  dispatch_vector_1[(int) OP_STORE] = do_store;
  dispatch_vector_1[(int) OP_STORE_LIT] = do_store_lit;
  dispatch_vector_1[(int) OP_STREAM_MAC] = do_stream_mac;
  dispatch_vector_1[(int) OP_SWAP] = do_swap;
  dispatch_vector_1[(int) OP_S_ONE_TICK] = do_s_one_tick;
  dispatch_vector_1[(int) OP_S_TICK] = do_s_tick;
  dispatch_vector_1[(int) OP_S_TICK_TICK] = do_s_tick_tick;
  dispatch_vector_1[(int) OP_TUCK_ALU] = do_tuck_alu;
  dispatch_vector_1[(int) OP_TUCK_STORE] = do_tuck_store;
  dispatch_vector_1[(int) OP_TUCK_STORE_WITH_ALU] = do_tuck_store_with_alu;
  dispatch_vector_1[(int) OP_TWO_STAR_TICK] = do_two_star_tick;
  dispatch_vector_1[(int) OP_UFETCH] = do_ufetch;
  dispatch_vector_1[(int) OP_UFETCH_OVER_ALU] = do_ufetch_over_alu;
  dispatch_vector_1[(int) OP_UFETCH_SWAP] = do_ufetch_swap;
  dispatch_vector_1[(int) OP_UFETCH_SWAP_ALU] = do_ufetch_swap_alu;
  dispatch_vector_1[(int) OP_UMAC] = do_umac;
  dispatch_vector_1[(int) OP_UMULT] = do_umult;
  dispatch_vector_1[(int) OP_UNDER_ALU] = do_under_alu;
  dispatch_vector_1[(int) OP_UNDER_STORE] = do_under_store;
  dispatch_vector_1[(int) OP_UNDER_STORE_LIT] = do_under_store_lit;
  dispatch_vector_1[(int) OP_USLASH_ONE_TICK] = do_uslash_one_tick;
  dispatch_vector_1[(int) OP_USLASH_ONE_TICK_TICK] = do_uslash_one_tick_tick;
  dispatch_vector_1[(int) OP_USLASH_TICK] = do_uslash_tick;
  dispatch_vector_1[(int) OP_USLASH_TICK_TICK] = do_uslash_tick_tick;
  dispatch_vector_1[(int) OP_USTAR_TICK] = do_ustar_tick;
  dispatch_vector_1[(int) OP_USTAR_TICK_TICK] = do_ustar_tick_tick;
  dispatch_vector_1[(int) OP_USTORE] = do_ustore;
  dispatch_vector_1[(int) OP_ZERO_EQUAL] = do_zero_equal;

  /* ---------------- second clock routines */
  dispatch_vector_2[(int) OP_DDUP_STORE] = do_ddup_store_2;
  dispatch_vector_2[(int) OP_DDUP_STORE_WITH_ALU] = do_ddup_store_with_alu_2;
  dispatch_vector_2[(int) OP_DROP_LIT] = do_drop_lit_2;
  dispatch_vector_2[(int) OP_DUP_FETCH_SWAP] = do_dup_fetch_swap_2;
  dispatch_vector_2[(int) OP_DUP_USTORE] = do_dup_ustore_2;
  dispatch_vector_2[(int) OP_FETCH] = do_fetch_2;
  dispatch_vector_2[(int) OP_FETCH_LIT] = do_fetch_lit_2;
  dispatch_vector_2[(int) OP_FETCH_OVER_ALU] = do_fetch_over_alu_2;
  dispatch_vector_2[(int) OP_FETCH_SWAP] = do_fetch_swap_2;
  dispatch_vector_2[(int) OP_FETCH_SWAP_ALU] = do_fetch_swap_alu_2;
  dispatch_vector_2[(int) OP_FETCH_WITH_ALU] = do_fetch_with_alu_2;
  dispatch_vector_2[(int) OP_LIT] = do_lit_2;
  dispatch_vector_2[(int) OP_LIT_OVER_ALU] = do_lit_over_alu_2;
  dispatch_vector_2[(int) OP_LIT_SWAP] = do_lit_swap_2;
  dispatch_vector_2[(int) OP_LIT_SWAP_ALU] = do_lit_swap_alu_2;
  dispatch_vector_2[(int) OP_NIP_DUP_FETCH_SWAP] = do_nip_dup_fetch_swap_2;
  dispatch_vector_2[(int) OP_NIP_FETCH_LIT] = do_nip_fetch_lit_2;
  dispatch_vector_2[(int) OP_NIP_FETCH_WITH_ALU] = do_nip_fetch_with_alu_2;
  dispatch_vector_2[(int) OP_STORE] = do_store_2;
  dispatch_vector_2[(int) OP_STORE_LIT] = do_store_lit_2;
  dispatch_vector_2[(int) OP_TUCK_STORE] = do_tuck_store_2;
  dispatch_vector_2[(int) OP_TUCK_STORE_WITH_ALU] = do_tuck_store_with_alu_2;
  dispatch_vector_2[(int) OP_UFETCH] = do_ufetch_2;
  dispatch_vector_2[(int) OP_UFETCH_OVER_ALU] = do_ufetch_over_alu_2;
  dispatch_vector_2[(int) OP_UFETCH_SWAP] = do_ufetch_swap_2;
  dispatch_vector_2[(int) OP_UFETCH_SWAP_ALU] = do_ufetch_swap_alu_2;
  dispatch_vector_2[(int) OP_UNDER_STORE] = do_under_store_2;
  dispatch_vector_2[(int) OP_UNDER_STORE_LIT] = do_under_store_lit_2;
  dispatch_vector_2[(int) OP_USTORE] = do_ustore_2;

}
