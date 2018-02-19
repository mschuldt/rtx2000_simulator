
/* matmul.c */
/* from Stanford benchmark suite */
/* modified for reasonable 16-bit operation */
 
#include <stdio.h>
/* #include <stdlib.h> */
 
#define true -1
#define false 0
 
    /* Intmm, Mm */
#define ROWSIZE 40
 
    /* Intmm, Mm */
int ima[ROWSIZE+1][ROWSIZE+1],
    imb[ROWSIZE+1][ROWSIZE+1],
    imr[ROWSIZE+1][ROWSIZE+1];

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

    /* Multiplies two integer matrices. */

static void Initmatrix ( m ) int m[ROWSIZE+1][ROWSIZE+1];
        {
        int temp, i, j;
        for ( i = 1; i <= ROWSIZE; i++ )
            for ( j = 1; j <= ROWSIZE; j++ )
            {   temp = Rand();
                m[i][j] = temp - (temp/120)*120 - 60;
            };
        };
 
static void Innerproduct( result,a,b, row,column)
        int *result,
            a[ROWSIZE+1][ROWSIZE+1],
            b[ROWSIZE+1][ROWSIZE+1],
            row,column;
        /* computes the inner product of A[row,*] and B[*,column] */
        {
        int i;
        *result = 0;
        for(i = 1; i <= ROWSIZE; i++ )*result = *result+a[row][i]*b[i][column];
        };
 
void main ()
    {
    int i, j, iter;
   for (iter = 1 ; iter < 25; iter++ )
   {
    Initrand();
    Initmatrix (ima);
    Initmatrix (imb);
    for ( i = 1; i <= ROWSIZE; i++ )
        for ( j = 1; j <= ROWSIZE; j++ ) Innerproduct(&imr[i][j],ima,imb,i,j);
   }
  };
 
