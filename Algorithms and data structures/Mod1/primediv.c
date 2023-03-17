#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main(void){
    long long x;
    scanf("%lld",&x);
    if (x<0){
        x=-x;
    }
    unsigned long res=x;
    int flag=0;
    unsigned long array[31]={2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648};
    if (x%2==0){
        for (int i=0; i<31; i++){
            if (array[i]==x){
                res=2;
                flag=2;
                i=31;
            }
        }
    }
    if (x>2000000000){
        flag=1;
    }
    if (flag==1){
        unsigned long top, t;
        char *prost1;
        top=sqrt(x)+1;
        t=top+1;
        prost1=calloc(t, sizeof(char));
        for (int i=2; i*i<top; i++){
            for (int k=i*i; k<top; k+=i){
                prost1[k]='t';
            }
        }
        for (unsigned long k=2; k<top; k++){
            if (x%k==0){
                unsigned long perem=x/k;
                if (perem<top){
                    for (unsigned long j=perem+1; j>1; j--){
                        if ((prost1[j]!='t') && (j==perem)){
                            res=perem;
                            j=1;
                        }
                    }
                }
                
            }
        }
        free(prost1);
    }
    if (flag==0){
        unsigned long top, t;
        char *prost1;
        top=x+1;
        t=top+1;
        prost1=calloc(t, sizeof(char));
        for (int i=2; i*i<top; i++){
            for (int k=i*i; k<top; k+=i){
                prost1[k]='t';
            }
        }
        for (unsigned long k=top-2; k>1; k--){
            if ((prost1[k]!='t') && (x%k==0)){
                res=k;
                k=1;
            }
        }
        free(prost1);
    }
    printf("%ld\n",res);
    return 0;
}

//2147483647
//2147483647 not work
//2147483640 work
//61442

//-2 147 483 648 < n < 2 147 483 648

/*
if (flag==1){
        for (unsigned long i=top-2; i>1; i--){
            if ((prost1[i]!='t') && (x%i==0)){
                res=i;
                i=1;
            }
        }
    }
*/


//можно делить и уменьшать число потепенно