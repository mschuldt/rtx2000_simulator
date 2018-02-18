/* HOST.C  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

/* This file contains FSM implementations to implement host
 *   service routines.
 */

#include <stdio.h>

#ifdef TURBOC
#include <stdlib.h>
#endif

#include "host.h"
#include "main.h"


int hostmode = FALSE;  /* when true, in process of talking to the host */
   /*  0 = not in command mode */
   /* -1 = processed escape character */
   /* other = command character */

int FSM = 0 ;  /* FSM state variable: 0 is rest state */

#define PAD_SIZE 80
char PAD[PAD_SIZE+1];  /* input/output buffer area */


void erase_pad()
{ int i;
  for ( i = 0; i < PAD_SIZE; i++)
   { PAD[i] = 0;}
}

#define first_count data
#define second_count (((count<<8) & 0xFF00) | (data & 0xFF))

#if NEVER
static int count = 0;
static FILE *file;
#endif

int X_OPEN_FILE(int data)
{

#if NEVER
 int result;
 result = 0;
  switch(FSM)
  { case 0: erase_pad();    break;
    /* receive 2 byte count */
    case 1: count = first_count;                             break;
    case 2: count = second_count;                             break;
    default:
     if(count < 0 ) { error_1("bad FSM in x_open_file",FSM);  break ;}
     if(count = 0 )
     { file = fopen(PAD,"r");
       result = fileno(fopen(PAD,"r")); }
     else
      {
      }

  } FSM++;
: X-OPEN-FILE
   PAD 80 ERASE
   PAD number_RCV DDUP RCVS
   OPEN-FILE  XMT ;
 #endif
 error_1("command not supported",data);
 return(0);
}

int X_CLOSE_FILE(int data)
{ error_1("command not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x_close_file",FSM);
  } FSM++;
*/
 return(0);
}

int X_READ_FILE(int data)
{ error_1("command not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x__file",FSM);
  } FSM++;
*/
 return(0);
}

int X_WRITE_FILE(int data)
{ error_1("command not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x__file",FSM);
  } FSM++;
*/
 return(0);
}

int X_SEEK(int data)
{ error_1("command not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x__file",FSM);
  } FSM++;
*/
 return(0);
}

int X_CREATE(int data)
{ error_1("command not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x__file",FSM);
  } FSM++;
*/
 return(0);
}

int X_DELETE(int data)
{ error_1("command not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x__file",FSM);
  } FSM++;
*/
 return(0);
}

int X_LIST_FILE(int data)
{ error_1("command not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x__file",FSM);
  } FSM++;
*/
 return(0);
}

int X_LIST(int data)
{ error_1("command not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x__file",FSM);
  } FSM++;
*/
 return(0);
}

int X_READ_LINE(int data)
{ error_1("command not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x__file",FSM);
  } FSM++;
*/
 return(0);
}

int X_COMMAND(int data)
{
  error_1("dos commands not supported",data);
/*  switch(FSM)
  { case 0:
    default:  error_1("bad FSM in x__file",FSM);
  } FSM++;
*/
  return(0);
}

int BYE(int data)
{ exit(0);
  return(data);
}



int host_read()
{ int data;
  data = 0;
  
  switch(hostmode) {
   case 0x20: /* openfile */ return(X_OPEN_FILE(data));
   case 0x21: /* close    */ return(X_CLOSE_FILE(data));    
   case 0x22: /* read     */ return(X_READ_FILE(data));    
   case 0x23: /* write    */ return(X_WRITE_FILE(data));    
   case 0x24: /* seek     */ return(X_SEEK(data));    
   case 0x25: /* create   */ return(X_CREATE(data));    
   case 0x26: /* delete   */ return(X_DELETE(data));    
   case 0x2D: /* listfile */ return(X_LIST_FILE(data));    
   case 0x2E: /* list     */ return(X_LIST(data));    
   case 0x2F: /* rdline   */ return(X_READ_LINE(data));    
   case 0x30: /* dos cmd  */ return(X_COMMAND(data));
   case 0xFF: /* exit     */ return(BYE(data));
   default:
      error_1("unknown function in host program read",data);
  }
  return(0);
}

void host_write(int data)
{
  if (data == 0xFF)   {  exit(0); }
  hostmode = FALSE;
  if (hostmode <= 0)  { hostmode = data;}

  switch(hostmode) {
   case 0x20: /* openfile */ X_OPEN_FILE(data);    break;
   case 0x21: /* close    */ X_CLOSE_FILE(data);    break;
   case 0x22: /* read     */ X_READ_FILE(data);    break;
   case 0x23: /* write    */ X_WRITE_FILE(data);    break;
   case 0x24: /* seek     */ X_SEEK(data);    break;
   case 0x25: /* create   */ X_CREATE(data);    break;
   case 0x26: /* delete   */ X_DELETE(data);    break;
   case 0x2D: /* listfile */ X_LIST_FILE(data);    break;
   case 0x2E: /* list     */ X_LIST(data);    break;
   case 0x2F: /* rdline   */ X_READ_LINE(data);    break;
   case 0x30: /* dos cmd  */ X_COMMAND(data);    break;
   case 0xFF: /* exit     */ BYE(data);    break;
   default:
      error_1("unknown function in host program write",data);
  }

}


#if NEVER

: KEYBOARD
   KEY? IF
      KEY
      ?DUP IF XMT EXIT THEN
      KEY DUP 113 = IF DROP BYE            THEN
          DUP 103 = IF DROP SUSPEND        THEN
          DUP  59 = IF DROP HELP EXIT      THEN
          DUP  60 = IF DROP BUGGY @ 0= BUGGY !
                                      EXIT THEN
      0 XMT XMT
   THEN ;

: number_XMT ( n)
   DUP XMT 256 / XMT ;
: XMTS ( a #)
   BOUNDS ?DO  I C@ XMT  LOOP ;

: RCVS ( a #)
   BOUNDS ?DO  RCV I C!  LOOP ;

: X-CLOSE-FILE
   RCV CLOSE-FILE ;

: X-READ-FILE
   RCV  PAD number_RCV  ROT READ-FILE ( #read)
   DUP number_XMT PAD SWAP XMTS ;

: X-WRITE-FILE
   RCV number_RCV
   PAD OVER RCVS
   PAD SWAP ROT WRITE-FILE
   number_XMT ;

: X-SEEK
   RCV ( handle) >R
   RCV ( dir)    >R
   number_RCV ( hi pos)  16 LSLN
   number_RCV ( lo pos)  OR
   R> R>
   SEEK-FILE
   DUP number_XMT 16 LSRN number_XMT ;

: X-CREATE
   PAD 80 ERASE
   PAD number_RCV DDUP RCVS
   CREATE-FILE  XMT ;

: X-DELETE     ;

\ ---------- simple file lister

ONLY FORTH ALSO DEFINITIONS  SYSTEM ALSO  DOS2 ALSO

: X-OPEN-LIST ( - f)
   LNAME FOPEN DUP HANDLE !  0<> DUP XMT ;

: X-CLOSE-LIST
   HANDLE @  DUP CLOSE-FILE  0<> XMT ;

: X-L16 ( list the next 16 lines)
   X-OPEN-LIST IF
      0 0 AT  DARK
      LINE# @ >LINE
      16 0 DO
         PAD 80 HANDLE @ READ-LINE IF PAD SWAP TYPE ELSE DROP THEN
         CR
      LOOP
      .LNAME
      X-CLOSE-LIST
   THEN ;

: X-LIST
   number_RCV  0 MAX  LAST-LINE @ MIN  LINE# !  X-L16  LINE# @ number_XMT ;

\ ----------

: X-SETUP-VIEW-FILE
   X-OPEN-LIST IF
      HANDLE @ FILESIZE  0 0 HANDLE @ SEEK-FILE DROP
      PAD S:O_ALIGN DUP ROT HANDLE @ BIG-READ-FILE ( pad #)
      OVER S:O>LINEAR + LINEAR>S:O ( from to)  LINES S:O_ALIGN
      SCAN-LINES LAST-LINE !
      X-CLOSE-LIST
   THEN ;

ONLY FORTH ALSO DEFINITIONS SYSTEM ALSO DOS2 ALSO

: X-LIST-FILE
   PAD 80 ERASE  PAD number_RCV DDUP RCVS ( a #)
   LNAME SWAP  DDUP >R >R  CMOVE  0 R> R> + C!
   X-SETUP-VIEW-FILE ;

\ ----------

: X-READ-LINE
   SPIN
   RCV ( handle) number_RCV ( count) SWAP ( # handle)
   >R  BUFF  OVER 1+ 128 MIN  R@  READ-FILE  ( # #read)
   DUP 0= IF ( end of file)
      DDROP  R> DROP  0 number_XMT 0 XMT  EXIT
   THEN
   ( # #read)
   BUFF OVER #EOL SCAN  NIP ( # #read #toeol)
   ?DUP IF    EOL# OVER - >R -
        ELSE  DDUP U< >R
        THEN
   MIN R> ( u4 #seek)
   ?DUP IF  1 R@ SEEK-FILE DROP  THEN
   BUFF OVER #EOF SCAN  NIP -   ( remove if no control-Zs)
   R> DROP
   BUFF W@ $205C = IF  DROP 2  THEN
   DUP ( #line) number_XMT -1 XMT
   BUFF SWAP
   BUGGY @ IF  CR DDUP TYPE THEN
   XMTS ;


\ ----- COMMAND.COM INTERFACE

: X-COMMAND
   number_RCV HERE C!  HERE COUNT RCVS
   HERE  [COMMAND]
   0 XMT ;


#endif