/* TEXEC.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* test code file for instruction execution */

#include "main.h"
#include "execute.h"
#include "decode.h"
#include "state.h"
#include <stdio.h>

int which_chip ;

void error(char *s)
{
  printf("\nERROR: %s ",s);
}

void trace(int instruction, int address)
{
  execute(instruction);
  printf("%X  insn=%X ",address,instruction);
  print_instruction(stdout, decode(instruction), instruction, CPR, PC);
  display_ds();
  printf("\n");
}

void main()
{
  init_dispatch();
  PC = 0x9000;
  CPR = 0;
  DPR = 0;
  display_ds();  printf("\n");
  trace(0xBE43, PC++);  /* lit 3 */
  trace(0xBE45, PC++);  /* lit 5 */
  trace(0xBE47, PC++);  /* lit 7 */
  trace(0xa840, PC++);  /* + */
  trace(0xaC40, PC++);  /* - */
  printf("\n%ld clocks\n",clocks);

}
