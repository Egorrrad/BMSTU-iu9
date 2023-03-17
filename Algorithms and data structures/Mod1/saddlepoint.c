#include <math.h>
#include <stdio.h>

int main(void){
    int str, col;
    scanf("%d", &str);
    scanf("%d", &col);
    int max=-10000000;
    int maxx[10]={-10000000, -10000000, -10000000, -10000000, -10000000, -10000000, -10000000, -10000000, -10000000, -10000000};
    int array[10][10]={{0}};
    int mi=10000000;
    int minn[10]={10000000,10000000,10000000,10000000,10000000,10000000,10000000,10000000,10000000,10000000};
    for (int i=0; i<str; i++){
        for (int k=0; k<col;k++){
            scanf("%d",&array[i][k]);
            if (array[i][k]>max){
                max=array[i][k];
            }
            if (k==col-1){
                maxx[i]=max;
                max=-10000000;
            }
            if (array[i][k]<minn[k]){
                minn[k]=array[i][k];
            }
       }
    }
    int i=0;
    int min;
    int m1, m2;
    m1=m2=-1;
    for (int i=0;i<str;i++){
        for (int k=0; k<col;k++){
            if ((array[i][k]==maxx[i]) && (array[i][k]==minn[k])){
                m1=i;
                m2=k;
            }
        }
    }
    //if ((str=1) && (col==1)){
    //       m1=m2=0;
     //   }
    if ((m1==-1) && (m2==-1)){
        printf("none");
    }
    else{
        printf("%d %d", m1, m2);
    }
}

//work++++
