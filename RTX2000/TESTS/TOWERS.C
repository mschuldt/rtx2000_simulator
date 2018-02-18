
/* towers.c */
/* from Stanford benchmark suite */
/* modified for reasonable 16-bit operation */

#include <stdio.h>
/* #include <stdlib.h> */
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
  { printf (" Error in Towers.\n"); }

static void Error (emsg) char *emsg;
	{
	printf(" Error in Towers: %s\n",emsg);
	};

static void Makenull (s)
	{
	stack[s]=0;
	};

static int Getelement ()
	{
	int temp;
	if ( freelist>0 )
	    {
	    temp = freelist;
	    freelist = cellspace[freelist].next;
	    }
	else
	    Error("out of space   ");
	return (temp);
	};

static void Push(i,s) int i, s;
	{
        int errorfound, localel;
	errorfound=false;
	if ( stack[s] > 0 )
	    if ( cellspace[stack[s]].discsize<=i )
		{
		errorfound=true;
		Error("disc size error");
		};
	if ( ! errorfound )
	    {
	    localel=Getelement();
	    cellspace[localel].next=stack[s];
	    stack[s]=localel;
	    cellspace[localel].discsize=i;
	    }
	};

static void Init (s,n) int s, n;
	{
	int discctr;
	Makenull(s);
	for ( discctr = n; discctr >= 1; discctr-- )
	    Push(discctr,s);
	};

static int Pop (s) int s;
	{
	 int temp, temp1;
	if ( stack[s] > 0 )
	    {
	    temp1 = cellspace[stack[s]].discsize;
	    temp = cellspace[stack[s]].next;
	    cellspace[stack[s]].next=freelist;
	    freelist=stack[s];
	    stack[s]=temp;
	    return (temp1);
	    }
	else
	    Error("nothing to pop ");
	};

static void Move (s1,s2) int s1, s2;
	{
/*        printf("Move %d -> %d\n",s1,s2); */
	Push(Pop(s1),s2);
	movesdone=movesdone+1;
	};

static void tower(i,j,k) int i,j,k;
	{
	int other;
	if ( k==1 )
	    Move(i,j);
	else
	    {
	    other=6-i-j;
	    tower(i,other,k-1);
	    Move(i,j);
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
    Init(1,14);
    Makenull(2);
    Makenull(3);
    movesdone=0;
    tower(1,2,14);
    if ( movesdone != 16383 )
	do_error();
   }
    }; /* Towers */
