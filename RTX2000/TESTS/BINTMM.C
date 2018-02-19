/******************** INTMM ******************/

#define ROWSIZE 40

int   ima [ROWSIZE+1][ROWSIZE+1], imb [ROWSIZE+1][ROWSIZE+1];
int   imr [ROWSIZE+1][ROWSIZE+1];
int   seed;

void initrand ()  {    
    seed = 74755;
}

int rand ()  {   
    seed = (seed * 1309 + 13849) & 65535;
    return (seed);
}


void initmatrix (m)
int m[ROWSIZE+1][ROWSIZE+1];
{
    int temp, i, j;

    for (i=1; i<=ROWSIZE; i++)   {
        for (j=1; j<=ROWSIZE; j++)  {               
            temp = rand ();
            m[i][j] = temp - (temp/120)*120 - 60;
        }
    }
}

/* computes the inner product of a X b */
void innerproduct (results, a, b, row, column)
int *results, a[ROWSIZE+1][ROWSIZE+1], b[ROWSIZE+1][ROWSIZE+1], row, column;
{
    int i;
    *results = 0;
    for (i=1; i<= ROWSIZE; i++)   {         
        *results = *results + a[row][i] * b[i][column];
    }
}


void main ()  {
    int i, j;
  
    initrand ();
    initmatrix (ima);
    initmatrix (imb);
    for (i=1; i<=ROWSIZE; i++)  {
        for (j=1; j<=ROWSIZE; j++)  {
            innerproduct (&imr[i][j], ima, imb, i, j);
        }
    }
}
