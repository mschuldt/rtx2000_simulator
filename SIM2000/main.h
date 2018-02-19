/* MAIN.H  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */


extern void error(char *s);
extern void error_1(char *s, int n);
extern void warning(char *s);
extern void warning_3(char *s, int page, int offset);

extern int which_chip;
#define RTX2000  1
#define RTX2001A 2
#define RTX2010  3

#define FALSE 0
#define TRUE  1

#ifdef TURBOC
#define LONG long
#else
#define LONG int  /* 32-bit systems */
#endif

#ifdef TURBOC
#define _MASKED_  /* nop */
#else
#define _MASKED_  & 0xFFFF  /* mask to 16 bits */
#endif
