/* STATE.H  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* machine registers */
extern int TOP;
extern int NEXT;
extern int IR;
extern int SOFTINT;
extern int STREAM;
extern int second_cycle;
extern int SVR;
extern int IVR;
extern int TP0, TP1, TP2;
extern int internal_regs[32];

#define INDEX internal_regs[0]
#define CR    internal_regs[3]
#define MD    internal_regs[4]
#define SR    internal_regs[6]
#define PC    internal_regs[7]
#define IMR   internal_regs[8]
#define SUR   internal_regs[10]
#define IPR   internal_regs[12]  /* 5 significant bits */
#define DPR   internal_regs[13]  /* 4 significant bits */
#define UPR   internal_regs[14]  /* 4 significant bits */
#define CPR   internal_regs[15]  /* 4 significant bits */
#define IBC   internal_regs[16]
#define UBR   internal_regs[17]
#define MXR   internal_regs[18]
#define TC0   internal_regs[19]
#define TC1   internal_regs[20]
#define TC2   internal_regs[21]
#define MLR   internal_regs[22]
#define MHR   internal_regs[23]

#define INC_PC  { PC = (PC + 2) & 0xFFFF; }

/* definitions for pseudo-registers */
#define SQ   ( ((MD<<1) & 0xFFFF) | SR )   /* read only, pseudo reg #5 */
#define SPR  ( (((rp+1) << 8) & 0xFF00) | ((dp+1) & 0x00FF) )   /* read only */

/* definitions for configuration register bits: see Table 4.5 */
#define IL      ( (CR & 0x8000) ? 1 : 0 )
#define RID     ( (CR & 0x4000) ? 1 : 0 )
#define ARCE    ( (CR & 0x2000) ? 1 : 0 )  /* 2010 only */
#define NMIM    ( (CR & 0x0800) ? 1 : 0 )  /* 2010 only */
#define BOOT    ( (CR & 0x0008) ? 1 : 0 )
#define BYTE    ( (CR & 0x0004) ? 1 : 0 )
#define CCY     ( (CR & 0x0002) ? 1 : 0 )
#define CY      ( (CR & 0x0001) ? 1 : 0 )
#define set_IL(value)    { if(value) { CR = CR | 0x8000; }      \
    else { CR = CR & 0x7FFF; }  }
#define set_ARCE(value)  { if(value) { CR = CR | 0x2000; }  /* 2010 only */ \
    else { CR = CR & 0xDFFF; }  }
#define set_NMIM(value)  { if(value) { CR = CR | 0x0800; }  /* 2010 only */ \
    else { CR = CR & 0xF7FF; }  }
/* note: set_SID actually sets bit 14 so RID will work */
#define set_SID(value)   { if(value) { CR = CR | 0x4000; }      \
    else { CR = CR & 0xBFFF; }  }
#define set_BOOT(value)  { if(value) { CR = CR | 0x0008; }      \
    else { CR = CR & 0xFFF7; }  }
#define set_BYTE(value)  { if(value) { CR = CR | 0x0004; }      \
    else { CR = CR & 0xFFFB; }  }
#define set_CCY(value)   { if(value) { CR = CR | 0x0002; }      \
    else { CR = CR & 0xFFFD; }  }
#define set_CY(value)    { if(value) { CR = CR | 0x0001; }      \
    else { CR = CR & 0xFFFE; }  }
#define set_CY_0  { CR = CR & 0xFFFE; } /* avoids unreachable code warning */
#define set_CY_1  { CR = CR | 0x0001; } /* avoids unreachable code warning */

/* aliases for carry bits used in RTX documentation */
#define CR_0 CY
#define CR_1 CCY
#define set_CR0(value) set_CY(value)
#define set_CR1(value) set_CCY(value)

/* definitions for interrupt mask register bits: see Table 4.7 */
/* all defines are read-only */
#define SWI   ( (IMR & 0x2000) ? 1 : 0 )
#define EI5   ( (IMR & 0x1000) ? 1 : 0 )
#define EI4   ( (IMR & 0x0800) ? 1 : 0 )
#define EI3   ( (IMR & 0x0400) ? 1 : 0 )
#define T2    ( (IMR & 0x0200) ? 1 : 0 )
#define T1    ( (IMR & 0x0100) ? 1 : 0 )
#define T0    ( (IMR & 0x0080) ? 1 : 0 )
#define EI2   ( (IMR & 0x0040) ? 1 : 0 )
#define RSV   ( (IMR & 0x0020) ? 1 : 0 )
#define PSV   ( (IMR & 0x0010) ? 1 : 0 )
#define RSU   ( (IMR & 0x0008) ? 1 : 0 )
#define PSU   ( (IMR & 0x0004) ? 1 : 0 )
#define EI1   ( (IMR & 0x0002) ? 1 : 0 )

/* definitions for interrupt/base control register bits: see Table 4.8 */
#define IB     (  IBC & 0xFC00)        /* returns bits 10-15 */
#define TB     ( (IBC & 0x0300) >>8 )  /* returns bits 0-1 */
#define CYCEXT ( (IBC & 0x0080) ? 1 : 0 )
#define ROUND  ( (IBC & 0x0040) ? 1 : 0 )
#define DPRSEL ( (IBC & 0x0020) ? 1 : 0 )
#define set_DPRSEL_0  { IBC = IBC & 0xFFDF; } /* avoids unreachable code warning */
#define set_DPRSEL_1  { IBC = IBC | 0x0020; } /* avoids unreachable code warning */
#define set_DPRSEL(value)  { if(value) { IBC = IBC | 0x0020; }  \
    else { IBC = IBC & 0xFFDF; }  }

extern LONG clocks;

#define CLOCKS(arg)  clocks += arg

#define MAX_STACK 255
#define STACK_MASK 0x00FF   /* mask for stack pointer wrapping */
typedef struct double_stack
{  int low;
  int high;
} double_stack ;

extern int data_stack[MAX_STACK+1];
extern int dp ;  /* data stack pointer */
extern double_stack return_stack[MAX_STACK+1];
extern int rp ;  /* return stack pointer */

#define ROM_SIZE   0x2000  /* in words */
#define ROM_BYTES  0x4000
#define ROM_ORG    0x0000

#define RAM_SIZE   0x4000  /* in words */
#define RAM_BYTES  0x8000
#define RAM_ORG    0x8000

extern int ROM[ROM_SIZE];
extern int RAM[RAM_SIZE];

#define DATA_PAGE  ( DPRSEL ? DPR : CPR )  /* correct page for access */
#define word_store(address, data)  long_store(DATA_PAGE, address, data)
#define word_fetch(address)        long_fetch(DATA_PAGE, address)
#define code_word_store(address, data)  long_store(CPR, address, data)
#define code_word_fetch(address)        long_fetch(CPR, address)

extern int fetch(int address);
extern int byte_fetch(int address);
extern int gfetch(int offset);
extern int long_fetch(int seg, int address);
extern int long_fetch(int seg, int address);;
extern int store(int address, int data);
extern int ufetch(int offset);
extern void display_ds();
extern void display_short_rs();
extern void display_rs();
extern void gstore(int offset, int data);
extern void init_state();
extern void long_store(int seg, int address, int data);
extern void pop();
extern void push(int data);
extern void rs_pop();
extern void rs_push(int page, int data);
extern void set_carry();
extern void ustore(int offset, int data);
