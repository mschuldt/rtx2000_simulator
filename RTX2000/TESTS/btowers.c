/********************  TOWERS  ********************/

#define FALSE       0
#define TRUE        1
#define MAXCELLS   18
#define STACKRANGE  3

int             stack [STACKRANGE+1];
struct element  {
    int discsize;
    int next;
} cellspace [MAXCELLS+1];
int             freelist, movesdone;

void error (emsg)
char *emsg;
{
}

void makenull (s)
int s;
{
    stack[s] = 0;
}

int getelement()   {
    int temp;

    if (freelist > 0)   {
        temp = freelist;
        freelist = cellspace[freelist].next;
    } else
        error ("Out of space    ");
    return (temp);
}

void push (i, s)
int i, s;
{
    int errorfound, localel;

    errorfound = FALSE;
    if (stack[s] > 0)  {
        if (cellspace[stack[s]].discsize <= i)  {
            errorfound = TRUE;
            error ("Disc size error   ");
        }
    }
    if (! errorfound)  {
        localel = getelement ();
        cellspace[localel].next = stack[s];
        stack[s] = localel;
        cellspace[localel].discsize = i;
    }
}

void init (s, n)
int s, n;
{
    int discctr;

    makenull (s);
    for (discctr=n; discctr >= 1; discctr--)  {
        push (discctr, s);
    }
}

int pop (s)
int s;
{
    int temp, temp1;

    if (stack[s] > 0)  {
        temp1 = cellspace[stack[s]].discsize;
        temp = cellspace[stack[s]].next;
        cellspace[stack[s]].next = freelist;
        freelist = stack[s];
        stack[s] = temp;
        return (temp1);
    } else
        error ("Nothing to pop ");
}

void move (s1, s2)
int s1, s2;
{
    push (pop (s1), s2);
    movesdone = movesdone + 1;
}

void tower (i, j, k)
int i, j, k;
{
    int other;

    if (k == 1)  {
        move (i, j);
    }else  {
        other = 6 - i - j;
        tower (i, other, k-1);
        move (i, j);
        tower (other, j, k-1);
    }
}

void main ()   {
    int i;

    for (i=1; i<= MAXCELLS; i++)  {
        cellspace[i].next = i-1;
    }
    freelist = MAXCELLS;
    init (1, 14);
    makenull (2);
    makenull (3);
    movesdone = 0;
    tower (1, 2, 14);
}
