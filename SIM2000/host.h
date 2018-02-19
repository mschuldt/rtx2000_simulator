/* HOST.H  --  RTX 2000 Instruction Set Simulator */
/* (C) Copyright 1990 Harris Semiconductor, all rights reserved */
/* By: Phil Koopman Jr.     9/x/90 */

#ifndef HOST_H
#define HOST_H

extern int host_read();
extern void host_write(int data);

extern int hostmode;  /* when true, in process of talking to the host */

#endif
