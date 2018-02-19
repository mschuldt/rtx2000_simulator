/* STATE.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* This file contains variable declarations, initializations,
 *   and routines to print the state of the simulator.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>

#include "main.h"
#include "state.h"
#include "execute.h"
#include "host.h"

/* --------------------------------------------------- */

int TOP;
int NEXT;
int IR;
int SOFTINT;
int STREAM;
int second_cycle;
int SVR;
int IVR;
int TP0, TP1, TP2;
int internal_regs[32];

LONG clocks;

int data_stack[MAX_STACK+1];
int dp;
double_stack return_stack[MAX_STACK+1];
int rp;

int ROM[ROM_SIZE];
int RAM[RAM_SIZE];

void init_state()
{
  TOP = 0;
  NEXT = 0xFFFF;
  IR = 0;
  INDEX = 0xFFFF;
  CR = 0x4008;
  MD = 0xFFFF;
  SR = 0 ;
  PC = 0;
  IMR = 0;
  dp = 0;  rp = 0;  /* SPR = 0; */
  SUR = 0x0707;
  SVR = 0xFFFF;
  IVR = 0x0200;
  IPR = 0;
  DPR = 0;
  UPR = 0;
  CPR = 0;
  IBC = 0;
  UBR = 0;
  MXR = 0;
  TC0 = 0; TP0 = 0;
  TC1 = 0; TP1 = 0;
  TC2 = 0; TP2 = 0;
  MLR = 0xFF00 ;
  MHR = 0xFFFF ;
  SOFTINT = 0;
  STREAM = FALSE;
  second_cycle = FALSE;
  /* set_DPRSEL(0);  /* default is tiny model, data from CPR page */
  clocks = 0;
}

/* -------------------------------------------- */

#ifndef TURBOC

/* UNIX-based key routines */
/* borrowed from Mitch Bradley's Forth */
int kbhit()  /* used to be long c_keyques */
{
  int nchars = 0;
  /*   fflush(stdout); */
  /*     if( (nchars = stdin->_cnt) == 0) ioctl(0, FIONREAD, &nchars); */
  /* printf(" %d", nchars); */
  nchars = 1;
  return (nchars);
}


int getch()   /* used to be long c_key() */
{
  register int c;
  fflush(stdout);
  c = getc(stdin);
  if (c == '\n')  c = 13 ;
  return(c);
}

#endif


/* --------------------------------------------------- */

/* data stack pop, return element popped */
void pop()
{ TOP = NEXT;
  NEXT = data_stack[dp] ;
  dp = (--dp) & STACK_MASK;
}

/* data stack push */
void push(register int data)
{ dp = (++dp) & STACK_MASK;
  data_stack[dp] = NEXT;
  NEXT = TOP;
  TOP = data;
}

#define set_IPR(page)                           \
  { IPR = page & 0xF;                           \
    if (DPRSEL) IPR = IPR | 0x10 ;              \
  }

/* return stack pop, return element popped */
void rs_pop()
{ IPR = (return_stack[rp].high)  & 0x1F ;   /* don't use set_IPR; bit 4 comes from RS */
  INDEX = return_stack[rp].low _MASKED_;
  rp = (--rp) & STACK_MASK;
}

/* return stack push with 2 inputs */
void rs_push(int page, register int data)
{ rp = (++rp) & STACK_MASK;
  return_stack[rp].high = IPR ;
  return_stack[rp].low  = INDEX;
  INDEX = data ;
  set_IPR(page) ;
  if (DPRSEL)  IPR = IPR & 0x10;
}

void display_ds()
{ int *ptr;
  printf(" top>> ");
  if (dp == 0 )
    { printf("empty ");  return; }

  printf("%d ",TOP);
  if (dp == 1 )  return;
  printf("%d ",NEXT);

  ptr = &data_stack[dp];
  while (ptr != &data_stack[2] )
    { printf("%d ",*ptr);
      ptr-- ;
    }
}

void display_rs()
{ double_stack *ptr;
  ptr = &return_stack[rp];
  printf(" rs>> ");
  if (rp == 0 )
    { printf("empty ");  return; }

  printf("%01X%04X ",IPR, INDEX);

  while (ptr != &return_stack[1] )
    { printf("%01X:%04X",(*ptr).high, (*ptr).low);
      ptr-- ;
    }
}

void display_short_rs()
{  if (rp == 0 )
    { printf("rs(0)>> empty ");  return; }

  printf(" rs(%d)>> ", rp-1);

  printf("%01X%04X ",IPR, INDEX);
  if (rp != MAX_STACK )  printf(" ...");
}

#define SHORT_LIT (instruction & 0x001F)

/* --------------------------------------------------- */


void long_store(int seg, int address, int data)
{ LONG addr;
  if (seg)  { addr = ((long) seg << 16) & 0xF0000 ;
    addr = addr |  ( (long) address & 0xFFFF ); }
  else  { addr = address & 0xFFFF ; }
  addr = addr - RAM_ORG ;
  if (addr >= RAM_BYTES)
    {  warning_3("store operation outside of RAM bounds", seg, address);
      return;
    }
  RAM[ addr>>1 ] = data _MASKED_ ;
}

int long_fetch(int seg, int address)
{ LONG addr, addrx;
  if (seg)  { addr = ((long) seg << 16) & 0xF0000 ;
    addr = addr |  ( (long) address & 0xFFFF ); }
  else  { addr = address & 0xFFFF  ; }
  addrx = addr - ROM_ORG ;
  if (  addrx >= 0
        && addrx <  ROM_BYTES)
    { return( ROM[ addrx>>1 ] );  }
  addrx = addr - RAM_ORG ;
  if (  addrx >= 0
        && addrx <  RAM_BYTES )
    { return( RAM[ addrx>>1 ] );  }
  warning_3("fetch operation outside of RAM/ROM bounds", seg, address);
  return(-1);
}

void byte_store(int address, int data)
{ register int temp;
  temp = long_fetch(DATA_PAGE, address);  /* watch out for doubled wait states!!! */
  if ( (address & 1) ^ BYTE )
    { long_store(DATA_PAGE, address, (  data     & 0x00FF) | (temp & 0xFF00) ) ; }
  else
    { long_store(DATA_PAGE, address, ( (data<<8) & 0xFF00) | (temp & 0x00FF) ) ; }
}

int byte_fetch(register int address)
{ if ( (address & 1) ^ BYTE )
    { return(   long_fetch(DATA_PAGE,address)       & 0x00FF) ; }
  else { return(  (long_fetch(DATA_PAGE,address) >> 8) & 0x00FF) ; }
}

int fetch(int address)
{ register int tempa, tempb;
  if (IR & 0x1000 )
    { return(byte_fetch(address)); }
  else
    { if ((address & 1) ^ BYTE)
        { tempa = word_fetch(address);
          tempb = (tempa>>8) & 0xFF;
          tempa = (tempa<<8) & 0xFF00;
          return(tempa | tempb);
        }
      else { return(word_fetch(address)); }
    }
}

int store(int address, int data)
{ register int tempa, tempb;
  if (IR & 0x1000 )
    { byte_store(address,data); }
  else
    { if ((address & 1) ^ BYTE)
        { tempb = (data>>8) & 0xFF;
          tempa = (data<<8) & 0xFF00;
          word_store(address, (tempa | tempb) );
        }
      else { word_store(address,data); }
    }
}

void ustore(int offset, int data)
{ register int addr;
  addr = ( UBR & 0xFFFE ) | ( (offset<<1) & 0x003E );
  long_store(UPR, addr, data);
}

int ufetch(int offset)
{ register int addr;
  addr = ( UBR & 0xFFFE ) | ( (offset<<1) & 0x003E );
  return( long_fetch(UPR, addr)) ;
}

/* --------------------------------------------------- */

int gdata[0x20] =
  { 0x0000, 0x0101, 0x0202, 0x0303,  0x0404, 0x0505, 0x0606, 0x0707,
    0x0808, 0x0909, 0x0A0A, 0x0B0B,  0x0C0C, 0x0D0D, 0x0E0E, 0x0F0F,
    0x1010, 0x1111, 0x1212, 0x1313,  0x1414, 0x1515, 0x1616, 0x1717,
    0x1818, 0x1919, 0x1A1A, 0x1B1B,  0x1C1C, 0x1D1D, 0x1E1E, 0x1F1F };



void gstore(register int offset, int data)
{
  /* implementation note: gstore is always invoked after TEST_EXIT */

  if (offset == 0x00 )
    { if ( EXIT )   { rs_push(CPR, data); }
      else          { set_IPR(CPR);  INDEX = data; }
      return;
    }

  if (offset == 0x01 )  { rs_push(CPR, data);    return; }

  if (offset == 0x02 )
    { rs_push(CPR, data);
      STREAM = TRUE;
      return;
    }
  if (offset == 0x03 && which_chip==RTX2010) { CR = data & 0xE81F; return;}
  if (offset == 0x03 && which_chip!=RTX2010) { CR = data & 0xC01F; return;}

  if (offset == 5 )  { MD = (data << 8 ) _MASKED_ ; return; }

  if (offset == 7 )
    { if ( EXIT )   /* push value onto RS, then perform exit */
        { PC = data;
          return;
        }
      else                /* perform a subroutine call */
        { rs_push(CPR, PC);
          PC = data;
          return;
        }
    }

  if ( offset == 0x08 ) { internal_regs[offset] = data & 0x3FFE;  return; }

  if ( offset == 0x09 )
    { if (which_chip == RTX2001A )
        { rp = (data >> 8) & 0x7F;    dp = data & 0x7F;  }
      else { rp = (data >> 8) & 0xFF;    dp = data & 0xFF;  }
      return;
    }

  if (offset == 0x0A )
    { if (which_chip == RTX2000 )
        {  error("0A G! not supported on RTX 2000");     return; }
    }

  if (  offset == 0x0C ) { internal_regs[offset] = data & 0x1F;  return; }

  if (  offset >= 0x0D
        && offset <= 0x0F ) { internal_regs[offset] = data & 0x0F;  return; }

  if ( offset == 0x10 )
    { if (which_chip == RTX2000 )   { IBC = data & 0xFFE0;  return; }
      if (which_chip == RTX2001A )  { IBC = data & 0xFFBF;  return; }
      /* RTX2010 */  error("rtx 2010 needs stack flags for IBC G@");
    }

  if (offset == 0x12 )
    { if (which_chip != RTX2010 )
        {  error("12 G! supported only on RTX 2010");     return; }
    }

  if ( offset == 0x13 )  { TP0 = data;  return; }  /* timer side effect? */
  if ( offset == 0x14 )  { TP1 = data;  return; }
  if ( offset == 0x15 )  { TP2 = data;  return; }

  if ( offset <= 0x17 )  { internal_regs[offset] = data;         return; }

  /* off-chip gstores */
  if ( offset == 0x19 )   {
    if (hostmode)  /* invoke fsm for serving host mode */
      { host_write(data);
        return;   }
    if(data)  { putchar(data); }
    else      { hostmode = -1; }
    return;  }

  if (  offset >= 0x18
        && offset <= 0x1F )
    { gdata[offset] = data;
      return; }

  error("illegal G! offset value");
}

/* --------------------------------------------------- */

int gfetch(register int offset)
{ int temp;
  /* implementation note: gfetch is always invoked before TEST_EXIT */

  if (offset == 0x01)
    { temp = INDEX;
      if ( !EXIT )  rs_pop();
      return(temp); }
  if (offset == 0x02) return( (INDEX<<1) _MASKED_ );
  if (offset == 0x05) return(SQ);   /* pseudo register */
  if (offset == 0x09) return(SPR);  /* pseudo register */
  if (offset == 0x0A && which_chip == RTX2000) error("SUR not defined on RTX2000");
  if (offset <= 0x17) return( internal_regs[offset] );

  /* this gets I/O to work for now */
  if (offset == 0x19)
    { if (hostmode)  /* invoke fsm for serving host mode */
        { return(host_read());  }
      temp = getch() _MASKED_ ;
      /* printf("\n(%d) = '%c'", temp, temp); */
      return(temp);
    }
  if (offset == 0x1A) return(kbhit() _MASKED_ );

  if (  offset >= 0x18
        && offset <= 0x1F )  return(gdata[offset]);
  error("illegal G@ offset value");
  return 0;
}

/* --------------------------------------------------- */
