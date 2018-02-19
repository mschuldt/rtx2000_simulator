/******************** QUEEN ******************/

#define FALSE  0
#define TRUE   1

void try (i, q, a, b, c, x)
int i, *q, a[], b[], c[], x[];
{
    int j = 0;

   *q = FALSE;
    while ((! *q) && (j != 8))  {
        j += 1;
        *q = FALSE;

        if (b[j] && a[j+i] && c[j-i+7])  {
            x[i] = j;
            b[j] = FALSE;
            a[j+i] = FALSE;
            c[j-i+7] = FALSE;

            if (i < 8)  {
                try (i+1, q, a, b, c, x);
            }
            else  {
                *q = TRUE;
            }

            if (! *q)  {
                b[j] = TRUE;
                a[j+i] = TRUE;
                c[j-i+7] = TRUE;
            }
        }
    }
}

void doit ()  {
    int i, q;
    int a[9], b[17], c[15], x[9];

    i = 0 - 7;
    while (i <= 16)  {
        if ((i >= 1) && (i <= 8))  {
            a[i] = TRUE;
        }
        if (i >= 2)  {
            b[i] = TRUE;
        }
        if (i <= 7)  {
            c[i+7] = TRUE;
        }
        i += 1;
    }
    try (1, &q, b, a, c, x);

}

void main ()  {
    int i;

   for (i=1; i <= 50; i++)  {
        doit ();
    }

}
