#include <stdio.h>
#include <stdlib.h>

 void swap(int *p, int i, int j){
       int perem;
       perem=p[i];
       p[i]=p[j];
       p[j]=perem;
 }

void selectsort(int *p, int n, int h){
    int k;
    for(int i = n; i < h; i++){
        k=i;
        for(int j = i+1; j <=h; j++){
            if(p[j] < p[k]){
                k=j;
            }
        }
        swap(p, i, k);
    }
}

 int partition(int *p, int low, int high){
    int i, j;
    i=low;
    j=low;
    while (j < high) {
        if(p[j] < p[high]){
            swap(p, i, j);
            j+=1;
        }
        j+=1;
    }
    swap(p, i, high);
    return i;
 }

 void quicksortrec(int *p, int low, int high, int m){
       if(low < high){
            if(low-high<=m){
                selectsort(p, low, high);
            } 
            else{
                int q;
                q=partition(p, low, high);
                quicksortrec(p, low, q-1, m);
                quicksortrec(p, q+1, high, m);
            }
       }
 }
 
 void quicksort(int *p, int n, int m){
    quicksortrec(p, 0, n-1, m);
 }

 int main(){
    int n, m;
    scanf("%d", &n);
    scanf("%d", &m);
    int *array;
    array=malloc(sizeof(int)*n);
    for(int i=0; i<n; i++){
        scanf("%d", array+i);
    }
    quicksort (array, n, m);
    for (int i=0; i<n; i++){
        printf("%d ", array[i]);
    }
    //free(array);
    return 0;
 }
/*
 int array[10]={-232  -346   603  -645  -722   643  -549  -176   668    94};
   n=10;
   m=9;

*/

/*
    while (j>0){
        k=j;
        i=j-1;
        while(i>=0){
            if (p[k]< p[i]){
                k=i;
            }
            i-=1;
        }
    swap (p, j, k);
    }
    */