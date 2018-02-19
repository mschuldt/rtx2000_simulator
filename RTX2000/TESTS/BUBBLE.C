
/* bubble.c */
/* from Stanford benchmark suite */
/* modified for reasonable 16-bit operation */

#include <stdio.h>
/* #include "stdlib.h" */

/* Bubble, Quick */
#define sortelements 	 5000
#define srtelements 	 500

#define false 0
#define true  1

/* Bubble, Quick */
int    sortlist[sortelements+1],
    biggest, littlest, top;

static void do_error()
  { printf ( "Error3 in Bubble.\n"); }


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

    /* Sorts an array using bubblesort */

static void bInitarr()
	{
	int i, temp;
	Initrand();
	biggest = 0; littlest = 0;
	for ( i = 1; i <= srtelements; i++ )
	    {
	    temp = Rand();
	    sortlist[i] = temp - 32767 ;
	    if ( sortlist[i] > biggest ) biggest = sortlist[i];
	    else if ( sortlist[i] < littlest ) littlest = sortlist[i];
	    };
	};

void main()
    {
    int i, j, iter;

   for (iter = 1 ; iter < 30 ; iter++ )
   {
    bInitarr();
    top=srtelements;
    while ( top>1 ) {

	i=1;
	while ( i<top ) {

	    if ( sortlist[i] > sortlist[i+1] ) {
		j = sortlist[i];
		sortlist[i] = sortlist[i+1];
		sortlist[i+1] = j;
		};
	    i=i+1;
	    };

	top=top-1;
	};
    if ( (sortlist[1] != littlest) || (sortlist[srtelements] != biggest) )
	do_error();
   }
 };
