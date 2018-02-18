/* TESTHEX.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

#include "hexin.h"
int which_chip ;

void error(char *s)
{
  fprintf(/*stderr*/stdout,"\nERROR: %s ",s);
}

main()
{
  printf("\nloading .HEX file...");
  hex_input(stdin);
  printf("done\n");
}
