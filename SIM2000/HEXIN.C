/* HEXIN.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* This file reads a .HEX file in Intel Hex format and stores
 * it into simulator memory.  It is pretty dumb, and does not
 * know about the entire Intel format -- just enough to do its job.
 * It ignores error codes in the format because the disk drives do
 * their own error checking anyway.
 */

#include <stdio.h>
#include "main.h"
#include "state.h"
#include "hexin.h"
#include "execute.h"

/*  Hex input format recognized by this program:
 *  item   bytes meaning
 *  ----   ----- --------------------
 *  :        1   generate error if missing (not a hex file if missing)
 *  count    2   hex count of data bytes
 *  addr     4   hex memory start address
 *  code     1   some sort of type code -- ignored
 *  data    20   10 bytes of data in hex expected (except for 0 count)
 *  check    2   some sort of checksum -- ignored
 *  nl       1   new line character
 */

/* ®MDBO¯---------------------------------------------------®MDNM¯ */


void init_store(int address, int data)
{ register unsigned int addr;      /* all initializations to page 0 memory */
  addr = address - ROM_ORG ;
  if (  addr >= 0
     && addr < ROM_BYTES)
   { ROM[ addr>>1 ]  = data;
     return; }
  addr = address - RAM_ORG ;
  if (  addr >= 0
     && addr < RAM_BYTES)
   { RAM[ addr>>1 ]  = data;
     return; }
  error("memory initialization address out of RAM/ROM bounds");
}

int convert_char(register int ascii_char)
{
  if (  ascii_char >= '0'
     && ascii_char <= '9')
  {  return( ascii_char - (int) '0');  }
  if (  ascii_char >= 'A'
     && ascii_char <= 'F')
  {  return( ascii_char + 10 - (int) 'A');  }
  if (  ascii_char >= 'a'
     && ascii_char <= 'f')
  {  return( ascii_char + 10 - (int) 'a');  }
  error("invalid hex character in input file");
}

#define INP (convert_char(fgetc(in_file)))

void hex_input(FILE *in_file)
{ int count, addr;
  register int data;
  char chr;

  count = -1 ;
  while ( !feof(in_file) )
  {
    chr = fgetc(in_file);
    if (feof(in_file)) break;

    if( chr != ':')
    { error("input file not in proper .HEX format");
      return;
    }
    count = INP<<4 ;  count += INP;  /* assume even number of input bytes */

#if DEBUG_HEX
    printf("\ncount=%02X ", count);
#endif

    addr = INP;
    addr = (addr<<4) + INP;
    addr = (addr<<4) + INP;
    addr = (addr<<4) + INP;

#if DEBUG_HEX
    printf("addr=%04X ", addr);
#endif

    /* throw away code */  fgetc(in_file);   fgetc(in_file);
    while ( count > 0 )
    {
      data = INP;
      data = (data<<4) + INP;
      data = (data<<4) + INP;
      data = (data<<4) + INP;
#if DEBUG_HEX
      printf("%02X %02X ", (data>>8) & 0xFF, data & 0xFF);
#endif
      init_store(addr, data);
      addr += 2;
      count -= 2;
    }
    /* throw away checksum & \n */
    fgetc(in_file);  fgetc(in_file);  fgetc(in_file);
  }
}

#undef INP
