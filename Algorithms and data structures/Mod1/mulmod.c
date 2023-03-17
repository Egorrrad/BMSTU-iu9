#include <stdio.h>
#include <math.h>


int main(void){
    unsigned long long a, b, m;
    unsigned long long os;
    //scanf("%lld%lld%lld", &a, &b, &m);
    a=4; 
    b=5;
    m=2;
    /*long del=100000000000000000;
    if (a%del==0){
        a=a/del;
    }
    if (b%del==0){
        b=b/del;
    }
    if (m%del==0){
        m=m/del;
    }
    if (a<pow(2,31)){
        os=(a*b)%m;
    }
    else{
        */
        unsigned long long x=a;
        unsigned long long y=b;
        unsigned long long ost;
        int n=0;
        int arr1[400]={0};
        while (x>0){
            ost=x%2;
            arr1[n]=ost;
            x=x/2;
            n+=1;
        }
        int arr2[400]={0};
        int c=0;
        while (y>0){
            ost=y%2;
            arr2[c]=ost;
            y=y/2;
            c+=1;
        }
        unsigned long long  array[400]={0};
        for (int i=0; i<400; i++){
            array[i]=arr1[i]*arr2[i];
        }
        
    printf("%lld", os);
    return 0;
}


/*a=2000000000000000000;
b=3000000000000000000;
m=5000000000000000000; */
//2000000000000000000 3000000000000000000 5000000000000000000 os 0 ------

//3105701647597925722 4999738698666759910 6485832086901542432 ostatok 5217490816175180598 ----

//3105701647597925722 4999738698666759910 6485832086901542432 ostatok 191057499226008380

//567349182 1625349953 1906350982 os 1895551636 +++++

//5525794756834742467 406006243781858320 735058053146009638 os 1403732759736241161

