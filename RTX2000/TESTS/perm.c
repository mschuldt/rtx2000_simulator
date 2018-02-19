
/* perm.c */
/* from Stanford benchmark suite */
/* modified for reasonable 16-bit operation */

#include <stdio.h>
/* #include "stdlib.h" */

    /* Perm */
#define    permrange 10

    /* Perm */
int    permarray[permrange+1];
int    pctr;

static void do_error() {
	printf(" Error in Perm.\n"); }

    /* Permutation program, heavily recursive, written by Denny Brown. */

static void    Swap_el ( a,b )
	int *a, *b;
	{
	int t;
	t = *a;  *a = *b;  *b = t;
	};

static void    Initialize ()
	{
	int i;
	for ( i = 1; i <= 7; i++ ) {
	    permarray[i]=i-1;
	    };
	};

static void    Permute (n)
	int n;
	{   /* permute */
	int k;
	pctr = pctr + 1;
	if ( n!=1 )  {
	    Permute(n-1);
	    for ( k = n-1; k >= 1; k-- ) {
		Swap_el(&permarray[n],&permarray[k]);
		Permute(n-1);
		Swap_el(&permarray[n],&permarray[k]);
		};
	    };
	}     /* permute */;

void main ()    {   /* Perm */
    int i;
    for ( i = 1; i <= 250 ; i++ ) {
        pctr = 0;
	Initialize();
	Permute(7);
        if ( pctr != 8660 )  do_error();
	};
    }     /* Perm */;

