/* queens.c */
/* from Stanford benchmark suite */
/* modified for reasonable 16-bit operation */
/* Hand-optimized for best C performance by Phil Koopman */
   /* net result: no changes made */

#include <stdio.h>

#define true -1
#define false 0

static void do_error()
  { puts (" Error in Queens.\n"); }

int tries ;
 
    /* The eight queens problem, solved 50 times. */
/*
        type
            doubleboard =   2..16;
            doublenorm  =   -7..7;
            boardrange  =   1..8;
            aarray      =   array [boardrange] of boolean;
            barray      =   array [doubleboard] of boolean;
            carray      =   array [doublenorm] of boolean;
            xarray      =   array [boardrange] of boardrange;
*/

static void Try(i, q, a, b, c, x) int i, *q, a[], b[], c[], x[];
            {
            int     j;
            tries += 1 ;
            j = 0;
            *q = false;
            while ( (! *q) && (j != 8) )
                { j = j + 1;
                *q = false;
                if ( b[j] && a[i+j] && c[i-j+7] )
                    {
                    x[i] = j;
                    b[j] = false;
                    a[i+j] = false;
                    c[i-j+7] = false;
                    if ( i < 8 )
                        {
                          Try(i+1,q,a,b,c,x);
                        if ( ! *q )
                            { b[j] = true;
                            a[i+j] = true;
                            c[i-j+7] = true;
                            }
                        }
                    else { *q = true;  
                         }
                    }
                }
            };

static void Doit ()
        {
        int i,q;
        int a[9], b[17], c[15], x[9];
        i = 0 - 7;
        while ( i <= 16 )
            { if ( (i >= 1) && (i <= 8) ) a[i] = true;
            if ( i >= 2 ) b[i] = true;
            if ( i <= 7 ) c[i+7] = true;
            i = i + 1;
            };

        Try(1, &q, b, a, c, x);
        if ( ! q )
         { do_error();
            tries += 1000 ;
         }
        };

void main ()
    {
    int i;
    for ( i = 1; i <= 2500; i++ )
     {
        tries = 0 ;
        Doit();
        if (tries != 113 )  do_error();
     }
    };

