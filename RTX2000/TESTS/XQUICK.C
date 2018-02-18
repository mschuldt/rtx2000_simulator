/* quick.c */
/* from Stanford benchmark suite */
/* modified for reasonable 16-bit operation */
/* Hand-optimized for best C performance by Phil Koopman */

#include <stdio.h>

    /* Bubble, Quick */
#define sortelements 	 5000

    /* Bubble, Quick */
int    sortlist[sortelements+1],
    biggest, littlest,      top;

static void do_error()
  { puts ( " Error in Quick.\n"); }

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
	int *i, temp;
	Initrand();
	biggest = 0; littlest = 0;
	for ( i = &sortlist[1]; i <= &sortlist[sortelements]; i++ )
	    {
	    temp = Rand();
	    temp = temp - 32668 ;
            *i = temp;
	    if ( temp > biggest ) biggest = temp;
	    else if ( temp < littlest ) littlest = temp;
	    };
	};

static void    Quicksort( a,l,r) int a[], *l, *r;
	/* quicksort the array A from start to finish */
	{
	int *i,*j,x,w;

	i=l; j=r;
	x= *(int *)( (  ((int)l >>1) + ((int)r >>1) ) & 0xFFFE ) ;
	do {
	    while ( *i<x ) i = i+1;
	    while ( x<*j ) j = j-1;
	    if ( i<=j ) {
		w = *i;
		*i = *j;
		*j = w;
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
    Quicksort(sortlist,&sortlist[1],&sortlist[sortelements]);
    if ( (sortlist[1] != littlest) || (sortlist[sortelements] != biggest) )
	do_error();
  }
 };

