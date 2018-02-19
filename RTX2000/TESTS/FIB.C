/* fib.c */
/* Phil Koopman Jr. */

#include <stdio.h>
/* #include <stdlib.h> */

static int fib(n)
  int n;
 {  int result ;
   if ( n < 3 )
     {  return(1) ; }
   result = fib(n-1) + fib(n-2) ;
   return(result);
 }


void main()
 { int input;
  input = 2 ;
  while (input > 0 )
  {
    printf("\nFibonacci.  Input?");
    scanf("%d",&input);
    printf("  = %d",fib(input));
  }
 }
