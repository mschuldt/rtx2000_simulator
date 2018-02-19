/* Eratosthenes Sieve Prime Number Program in C from Byte January 1983 */
/* Hand-optimized for best C performance by Phil Koopman */

#include <stdio.h>

#define true 1
#define false 0
#define size 8190

char flags [size+1];

static void do_error() {
	puts(" Error in Sieve.\n"); }

void main()
  {
    int count, iter, j, k, prime;
    char *i;
    for(iter = 1; iter < 350 ; iter++)
      {
      count = 0 ;  /* prime counter */
      for (i = &flags[0]; i <= &flags[size]; i++)  /* set all flags true */
         *i = true ;
      for (j = 0; j <= size; j++)
         {
           if (flags[j])     /* found a prime */
           {
             prime = j + j + 3 ;
             for (k = j+prime; k<=size; k+= prime)
                     { flags[k] = false; }
             count++;
           }
         }
      if (count != 1899) do_error();
      }
  }

