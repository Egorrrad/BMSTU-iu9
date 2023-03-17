#include <math.h>
#include <stdio.h>

int maxarray(void *base, size_t nel, size_t width,
        int (*compare)(void *a, void *b)){
             char *maxi = base, *now = base;
             for(int i = 0; i < nel; ++i){
                   if(compare(maxi, now) < 0)
                         maxi = now;
                   now += width;
             }
       return (maxi - (char *)base)/width;
 }

