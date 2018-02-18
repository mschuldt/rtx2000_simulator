/******************** PERM ************************/

#define PERMRANGE  10

int   permarray [PERMRANGE+1];
int   pctr;

void swap (a, b) 
int *a;
int *b;
{
    int t;
    t = *a; *a = *b; *b = t;
}

void initialize ()  {
    int i;
    for (i=1; i<=7; i++)  {
        permarray [i] = i - 1;
    }
}

void permute (n)
int n;
{
    int k;

    pctr = pctr + 1;
    if (n != 1)  {
        permute (n-1);
        for (k=n-1; k>=1; k--)  {
            swap (&permarray[n], &permarray[k]);
            permute (n-1);
            swap (&permarray[n], &permarray[k]);
        }
    }
}

void main ()  {
    int i;

    pctr = 0;
    for (i=1; i<=5; i++)  {
        initialize ();
        permute (7);
     }
 }

