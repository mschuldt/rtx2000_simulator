/* OUTPUT.H  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* print a disassembled instruction to a designated output */
/* inputs: *outf = output file (e.g., stdout)
 *      opr = decoded instruction token
 *     instruction = 16-bit instruction value
 *     address = address of instruction (for branch computations
 */

#ifndef OUTPUT_H
#define OUTPUT_H

void print_instruction(FILE *outf, machine_op opr,
                       int instruction, int page, int address);

#endif 
