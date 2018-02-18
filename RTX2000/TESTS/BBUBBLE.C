/******************** BUBBLE ********************/
#define SRTELEMENTS   500

int   sortlist[SRTELEMENTS+1], biggest, littlest, top;
int   seed;

void initrand ()  {    
    seed = 9219;
}

int rand ()  {   
    seed = (seed * 1309 + 13849) & 65535;
    return (seed);
}


void binitarr ()  {
    int i, temp;

    initrand ();
    biggest = 0; littlest = 0;
    for (i=1; i<= SRTELEMENTS; i++)  {
          temp = rand ();
          sortlist[i] = temp - (temp / 10000) * 10000 - 5000;
          if (sortlist[i] > biggest)  {
            biggest = sortlist[i];
          }else  {
            if (sortlist[i] < littlest)  {
                littlest = sortlist[i];
            }
          }
    }
}

void main ()  {
    int i, j;

    binitarr ();
    top = SRTELEMENTS;

    while (top > 1)   {
        i = 1;
        while (i < top)  {
            if (sortlist[i] > sortlist[i+1])  {
                j = sortlist[i];
                sortlist[i] = sortlist[i+1];
                sortlist[i+1] = j;
            }
            i = i + 1;
        }
        top = top - 1;
    }
  
}
