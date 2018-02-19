/* MAIN.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* Driving routine.
 */

#include <stdlib.h>
#include <stdio.h>
#include "main.h"
#include "state.h"
#include "decode.h"
#include "output.h"
#include "hexin.h"
#include "execute.h"

#define TRACE    0
#define INFINITE 1

LONG count;
#define START_COUNT 18100   /* minimum count to activate trace */
#define END_COUNT   50000   /* maximum number of instructions to run */

/* .MDBO/---------------------------------------------------.MDUL/.MDNM/ */

#if COMMENT
#ifndef TURBOC
/* keyboard I/O initialization lifted from Mitch's Forth */

#include <sgtty.h>
struct sgttyb ostate;
struct sgttyb lstate;
struct sgttyb kstate;
#define TCSETA TIOCSETN

#define M_ORIG 0
#define M_KEY  1
#define M_LINE 2
static lmode = M_ORIG;

initline() {
  if (lmode != M_ORIG)
    { printf("error in initline\n");   return; }

  ioctl(0, TIOCGETP, &ostate);            /* save old state
                                           */

  ioctl(0, TIOCGETP, &lstate);            /* base of line state
                                           */
  lstate.sg_flags &= ~(CBREAK|RAW);       /* Line editing on
                                           */
  lstate.sg_flags |= ECHO;                /* Echo
                                           */
  lstate.sg_flags |= CRMOD;               /* Accept CR for LF
                                           */

  ioctl(0, TIOCGETP, &kstate);            /* base of key state
                                           */
  kstate.sg_flags |= CBREAK;              /* Wake up on each char
                                           */
  kstate.sg_flags &= ~ECHO;               /* Don't echo
                                           */
  kstate.sg_flags |= CRMOD;               /* Accept CR for LF
                                           */
}

linemode()
{
  initline();
  if (lmode != M_LINE) {
    ioctl(0, TCSETA, &lstate);
    lmode = M_LINE;
  }
}

keymode()
{
  initline();
  if (lmode != M_KEY) {
    ioctl(0, TCSETA, &kstate);
    lmode = M_KEY;
  }
}


restoremode()
{
  initline();
  if (lmode != M_ORIG) {
    ioctl(0, TCSETA, &ostate);
    lmode = M_ORIG;
  }
}



#endif
#endif

/*------------------------------------------------------*/
int which_chip ;

void error(char *s)
{
  fprintf(/*stderr*/stdout,"\nERROR: %s \n",s);
  exit(-1);
}

void error_1(char *s, int n)
{
  fprintf(/*stderr*/stdout,"\nERROR: %s %d\n", s, n);
  exit(-1);
}

void warning(char *s)
{  fprintf(/*stderr*/stdout,"  WARNING: %s ",s);
}

void warning_3(char *s, int page, int offset)
{  fprintf(/*stderr*/stdout,"  WARNING: %s addr=%01X:%04X\n", s, page, offset);
}

#define preamble                                                        \
  if (count > START_COUNT)                                              \
    { printf("%04X  insn=%04X ",address,instruction);                   \
      print_instruction(stdout, decode(instruction), instruction, CPR, PC); \
    }

#define epilog                                  \
  if (count > START_COUNT)                      \
    { printf("  ");                             \
      display_ds();  display_short_rs();        \
      printf("\n");                             \
    }


void trace(int instruction, int address)
{
#if TRACE
  preamble
#else
    address++;  /* eliminates unused parameter warning */
#endif
  execute(instruction);
#if TRACE
  epilog
#endif
    }


void trace_2(int instruction, int address)
{
#if TRACE
  if (count > START_COUNT) printf("         ");
#endif
  address++;  /* eliminates unused parameter warning */
  execute_2(instruction);
#if TRACE
  epilog
#endif
    }

void main(int argc, char *argv[])
{ FILE *rom_file;
  int inst, temp_pc, stream_mode;
  int *ram_ptr;

  printf("\nRTX 2000 Simulator  ver.0.0  9/x/90   By Phil Koopman Jr.");
  printf("\n(C) Copyright 1990, All Rights Reserved, Harris Semiconductor\n");
  if ( argc < 1 )  /* no input arguments */
    { printf("\nusage:   SIM2000  rom-file\n");
      return;
    }

  printf("ROM file: %s\n",argv[1]);
  rom_file = fopen(argv[1],"r") ;
  hex_input(rom_file);

  /* wipe RAM to ensure a real cold start */
  for (ram_ptr = &RAM[0] ; ram_ptr < &RAM[RAM_SIZE] ;  ram_ptr++ )
    {  *ram_ptr = 0xA5A5 ; }

  init_dispatch();  init_state();
  which_chip = RTX2000;
  PC = 0x0000;
  CPR = 0;
  DPR = 0;
  stream_mode = FALSE;
  display_ds();  printf("\n");

#if COMMENT
#ifndef TURBOC
  keymode();
#endif
#endif

  inst = long_fetch(CPR, PC);
  temp_pc = PC;
  INC_PC ;

  for (count = 0 ; count < END_COUNT; count++ )
    {
      if (!STREAM && second_cycle)
        { trace_2( inst, temp_pc ); }
      else
        { trace( inst, temp_pc ); }

      if(stream_mode)
        { if(INDEX == 0)
            { STREAM = FALSE;
              stream_mode = FALSE;
              rs_pop();
              if (!second_cycle)
                { inst = long_fetch(CPR, PC);
                  temp_pc = PC;
                  INC_PC;
                }
            }
          else
            { INDEX = (INDEX - 1) _MASKED_ ; }
        }
      else
        { if (STREAM)
            { /* enter streamed mode for following instruction */
              stream_mode = TRUE;
            }
          if(!second_cycle)
            { inst = long_fetch(CPR, PC);
              temp_pc = PC;
              INC_PC ;
            }
        }
#if INFINITE
      count--;
#endif
    }
#if COMMENT
#ifndef TURBOC
  restoremode();
#endif
#endif
}

#if NEVER
\ TODO LIST:
\ Interrupts (remember also specific instructions disable interrupts
\     -- pg. 154, 155, ...  also set SOFTINT
\ NMI interrupts streamed instructions.
\ wait states for ASIC bus
\ wait states for memory bus
\ Support for IVR/SVR/SLR (ASIC addres 0x0B) pg. 53.
\ Cycle extend bits (pg. 67, 68)
\ Chapter 6: stacks, interrupts
\ SLR not implemented (pg. 88)
\ far calls (pg. 127) & 71.
\ Division is stubbed out -- need to implement full step math
\ Does I bit really work on "special" instructions?  2 categories.
\        perhaps it only works on 1 of them.
\ Support for counter/timers

\ NEEDS TO BE OPTIMIZED FOR SPEED!
\ eliminate run-time decoding:  byte/word for memory access
\                               special cases of gbus operations
\ order case statements to account for execution frequency

--------------------------------------------------------------

\ RTX 2001A todo list:
\ ====================
\ different stack size
\ advanced stack controller
\ NMI interrupts streamed instructions. & glitch filter circuit pg. 31
\ Initialization pg. 37
\ Implementation of SUR

-------------------------------------------------------------

\ RTX 2010 todo list:
\ ====================
\ advanced stack controller
\ NMI interrupt modes. & glitch filter circuit pg. 32
\ Initialization pg. 38



#endif
