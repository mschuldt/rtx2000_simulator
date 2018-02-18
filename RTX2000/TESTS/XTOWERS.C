/* towers.c */
/* from Stanford benchmark suite */
/* modified for reasonable 16-bit operation */
/* Hand-optimized for best C performance by Phil Koopman */

#include <stdio.h>

    /* Towers */
#define maxcells 	 18

#define    stackrange	3
/*    cellcursor = 0..maxcells; */
struct    element {
	    int discsize;
	    int next;
	};
#define true  1
#define false 0

    /* Towers */
 int   stack[stackrange+1];
struct element    cellspace[maxcells+1];
int    freelist,  movesdone;

    /*  Program to Solve the Towers of Hanoi */

static void do_error()
  { puts (" Error in Towers.\n"); }

static void Error (emsg) char *emsg;
	{
	printf(" Error in Towers: %s\n",emsg);
	};

void Makenull (int *s)
	{
	*s=0;
	};

int Getelement ()
	{
	int temp;
	if ( freelist>0 )
	    {
	    temp = freelist;
	    freelist = cellspace[freelist].next;
            return(temp);
	    }
	else
	    Error("out of space   ");
	return (temp);
	};

static void Push(i,s) int i, *s;
	{
        int errorfound, localel;
        int s_val;
	errorfound=false;

        s_val = *s;
	if ( s_val > 0 )
	    if ( cellspace[s_val].discsize<=i )
		{
		errorfound=true;
		Error("disc size error");
		};
	if ( ! errorfound )
	    {
	    localel=Getelement();
	    cellspace[localel].next=s_val;
	    *s=localel;
	    cellspace[localel].discsize=i;
	    }
	};

static void Init (s,n) int *s, n;
	{
	int discctr;
	Makenull(s);
	for ( discctr = n; discctr >= 1; discctr-- )
	    Push(discctr,s);
	};

static int Pop (s) int *s;
	{
	 int temp, temp1;
	if ( *s > 0 )
	    {
	    temp1 = cellspace[*s].discsize;
	    temp = cellspace[*s].next;
	    cellspace[*s].next=freelist;
	    freelist=*s;
	    *s=temp;
	    return (temp1);
	    }
	else
	   { Error("nothing to pop ");
             return(0);
           }
	};

static void Move (s1,s2) int *s1, *s2;
	{
/*        printf("Move %d -> %d\n",s1,s2); */
	Push(Pop(s1),s2);
	movesdone=movesdone+1;
	};

static void tower(i,j,k) int i,j,k;
	{
	int other;
	if ( k==1 )
	    Move(&stack[i],&stack[j]);
	else
	    {
	    other=6-i-j;
	    tower(i,other,k-1);
	    Move(&stack[i],&stack[j]);
	    tower(other,j,k-1);
	    }
	};


void main ()    { /* Towers */
    int i, iter;
    for (iter = 0 ; iter < 35 ; iter++ )
   {
    for ( i=1; i <= maxcells; i++ )
	cellspace[i].next=i-1;
    freelist=maxcells;
    Init(&stack[1],14);
    Makenull(&stack[2]);
    Makenull(&stack[3]);
    movesdone=0;
    tower(1,2,14);
    if ( movesdone != 16383 )
	do_error();
   }
    }; /* Towers */
