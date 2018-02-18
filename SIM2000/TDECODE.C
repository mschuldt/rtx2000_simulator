/* TDECODE.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* test code file for instruction cracking */

#include "output.h"

int which_chip ;

void error(char *s)
{
  fprintf(/*stderr*/stdout,"\nERROR: %s ",s);
}

void main()
{
  unsigned int i;
  int j;

  which_chip = RTX2001A;
  for (i = 0x7FFF; i <= 0xFFFE; i++)
  { j = i;
    printf("%05X ",j);
    print_instruction(stdout, decode(j), j, 0, 0x9000);
    printf("\n");
  }
   j = 0xFFFF;
    printf("%05X ",j);
    print_instruction(stdout, decode(j), j, 0, 0x9000);
    printf("\n");
}
