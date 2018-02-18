
/* quick.c */
/* from Stanford benchmark suite */
/* modified for reasonable 16-bit operation */

#include <stdio.h>
/* #include <stdlib.h> */

    /* Bubble, Quick */
#define sortelements 	 5000

    /* Bubble, Quick */
int    sortlist[sortelements+1],
    biggest, littlest,      top;

static void do_error()
  { printf ( " Error in Quick.\n"); }

int seed ;

static void Initrand ()
    {
    seed = 9219;
    };

static int Rand ()
    {
    seed = (seed * 1309 + 13849) ;
    return( seed );
    };

    /* Sorts an array using quicksort */

static void    Initarr()
	{
	int i, temp;
	Initrand();
	biggest = 0; littlest = 0;
	for ( i = 1; i <= sortelements; i++ )
	    {
	    temp = Rand();
	    sortlist[i] = temp - 32668 ;
	    if ( sortlist[i] > biggest ) biggest = sortlist[i];
	    else if ( sortlist[i] < littlest ) littlest = sortlist[i];
	    };
	};

static void    Quicksort( a,l,r) int a[], l, r;
	/* quicksort the array A from start to finish */
	{
	int i,j,x,w;

	i=l; j=r;
	x=a[(l+r) / 2];
	do {
	    while ( a[i]<x ) i = i+1;
	    while ( x<a[j] ) j = j-1;
	    if ( i<=j ) {
		w = a[i];
		a[i] = a[j];
		a[j] = w;
		i = i+1;    j= j-1;
		}
	} while ( i<=j );
	if ( l <j ) Quicksort(a,l,j);
	if ( i<r ) Quicksort(a,i,r);
	};


int main ()
    {
    int i;
    for (i = 0 ; i < 50  ; i++ )
   {
    Initarr();
    Quicksort(sortlist,1,sortelements);
    if ( (sortlist[1] != littlest) || (sortlist[sortelements] != biggest) )
	do_error();
  }
    };

