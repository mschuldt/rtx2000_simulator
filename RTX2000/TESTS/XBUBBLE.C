/* bubble.c */
/* from Stanford benchmark suite */
/* modified for reasonable 16-bit operation */
/* Hand-optimized for best C performance by Phil Koopman */

#include <stdio.h>

/* Bubble, Quick */
#define sortelements 	 5000
#define srtelements 	 500

#define false 0
#define true  1

/* Bubble, Quick */
int    sortlist[sortelements+1],
    biggest, littlest;

static void do_error()
  { puts ( "Error3 in Bubble.\n"); }

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
	int temp, list;
        int *i;
	Initrand();
	biggest = 0; littlest = 0;
	for ( i = &sortlist[1]; i <= &sortlist[srtelements]; i++ )
	    {
	    temp = Rand();
	    list = temp - 32767 ;
            *i = list;
	    if ( list > biggest ) biggest = list;
	    else if ( list < littlest ) littlest = list;
	    };
	};

int main()
    {
    int j, iter;
    int *i, *top;

   for (iter = 1 ; iter < 30 ; iter++ )
   {
/*    printf("\niter = %d ",iter); */
    bInitarr();
    top = &sortlist[srtelements];
    while ( top > &sortlist[1] ) {

	i=&sortlist[1];
	while ( i < top ) {

	    if ( *i > *(i+1) ) {
		j = *i;
		*i = *(i+1);
		*(i+1) = j;
		};
	    i=i+1;
	    };

	top=top-1;
	};
    if ( (sortlist[1] != littlest) || (sortlist[srtelements] != biggest) )
	do_error();
   }
 return(0);
 };
