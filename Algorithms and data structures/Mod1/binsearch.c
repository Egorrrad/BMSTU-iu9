#include <math.h>
#include <stdio.h>
#include <stdlib.h>

unsigned long binsearch(unsigned long nel, int (*compare)(unsigned long i))
{
   unsigned long left=0; 
   unsigned long right=nel-1;
   unsigned long in=nel;
   unsigned long m=0;
    while(left<=right){
        m=(left+right)/2;
        if (compare(m)==0){
            in=m;
            break;
        }
        if (compare(m)==1){
            right=m-1;
        }
        if (compare(m)==-1){
            left=m+1;
        }
    }
    return in;
}
int main(){
    printf("%ld", binsearch(8, compare));
}
//++++++++