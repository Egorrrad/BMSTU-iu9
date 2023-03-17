#include <stdio.h>
#include <math.h>


int main(void)
{
    long long n, x0;
    long long t, res=0, prois=0;
    scanf("%lld%lld", &n,&x0);
    for (int i=0; i<n+1; i++)
    {
        scanf("%lld", &t);
        if (i==n){
            res=res+t;
        }
        else{
            res=(res+t)*x0;
        }
        if (i<n-1){
            prois=(prois+(n-i)*t)*x0;
        }
        else{
            prois+=t*(n-i);
        }
    }
    printf("%lld\t%lld",res, prois);

    return 0;
}