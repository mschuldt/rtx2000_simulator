/******************** QUICK  ********************/

#define SORTELEMENTS     5000

int   sortlist[SORTELEMENTS+1], biggest, littlest, top;
int   seed;

void initrand ()  {    
    seed = 9219;
}

int rand ()  {   
    seed = (seed * 1309 + 13849) & 65535;
    return (seed);
}


void initarr ()
{
    int i, temp;

    initrand ();
    biggest = 0;
    littlest = 0;
    for (i=1; i <= SORTELEMENTS; i++)   {
        temp = rand ();
        sortlist[i] = temp - (temp / 10000) * 10000 - 5000;
        if (sortlist[i] > biggest)  {
            biggest = sortlist[i];
        } else  {
            if (sortlist[i] < littlest)  {
                littlest = sortlist[i];
            }
        }
    }
}

void quicksort (a, l, r)
int a[], l, r;
{
    int i, j, x, w;

    i = l; j = r;
    x = a[(l+r) / 2];
    do  {
        while (a[i] < x)  {
            i = i + 1;
        }
        while (x < a[j])  {
            j = j - 1;
        }
        if (i <= j)  {
            w = a[i];
            a[i] = a[j];
            a[j] = w;
            i = i + 1;  j = j - 1;
        }
    } while (i <= j);
    if (l < j)   {
        quicksort (a, l, j);
    }
    if (i < r)  {
        quicksort (a, i, r);
    }
}

void main ()
{
    initarr ();
    quicksort (sortlist, 1, SORTELEMENTS);
}
