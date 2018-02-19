/* EXECUTE.H  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */
       
#define NUMBER_OF_ROUTINES 128
/* Note: you *MUST* change the initialization statement for the array
 * if NUMBER_OF_ROUTINES is changed !!!!! */
extern void (*dispatch_vector[NUMBER_OF_ROUTINES]) ();

extern void execute(int instruction);
extern void execute_2(int instruction);

void init_dispatch();  /* initialize the function dispatch vector */

#define EXIT       (IR & 0x0020)
#define TEST_EXIT  { if (EXIT)  do_exit(); }
extern void do_exit();
