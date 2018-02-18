/* Eratosthenes Siee Prime Number Program in C from Byte January 1983 */

#include <stdio.h>

#define true 1
#define false 0
#define size 8190

char flags [size+1];

static void do_error() {
	printf(" Error in Sieve.\n"); }

void main()
  {
    int prime, count, iter;
    register i,k;
/*    getchar(); */
    for(iter = 1; iter < 350 ; iter++)
      {
      /* putchar ('I');  */
      count = 0 ;  /* prime counter */
      for (i = 0; i <= size; i++)  /* set all flags true */
         flags[i] = true ;
      for (i = 0; i <= size; i++)
         {
           if (flags[i])     /* found a prime */
           {
             prime = i + i + 3 ;
             for (k = i+prime; k<=size; k+= prime)
                     flags[k] = false;
             count++;
           }
         }
      }
/*    printf(" %d primes found\n",count); */
    if (count != 1899) do_error();
  }

