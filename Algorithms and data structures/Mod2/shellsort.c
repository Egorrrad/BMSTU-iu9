
#include <stdio.h> 
  

int array[100]={1, 4, 3, 5, 2};

int compare(unsigned long i, unsigned long j)
 {
         if (i < j) {
            return -1;
         } 
         if (i>j) {
            return 1;
         }
         return 0;
 }

 void swap(unsigned long i, unsigned long j)
 {
         int t = array[i];
         array[i] = array[j];
         array[j] = t;
 }

/* Пример из книги Герберта Шилдта */
void shellsort(unsigned long nel, int (*compare)(unsigned long i, unsigned long j), 
    void (*swap)(unsigned long i, unsigned long j)){
        int fibr[40]={1,1};
        int index=0;
        for (int i=2; i<nel+1; i++){
            fibr[i]=fibr[i-2]+fibr[i-1];
            if (fibr[i]>nel){
                index=i-1;
                break;
            }
        }
        for (int i=fibr[index]; index>=1; index--, i=fibr[index]){
            for (int k=0; k+i<nel; k++){
                if (compare(k, k+i)==1){
                    swap(k, k+1);
                }
                for (int j=k-i; j>=0; j-=i){
                    if (compare(j, j+1)==1){
                        swap(j, j+i);
                    }
                    else{
                        j=0;
                    }
                }
            }
        }
}

  int main(){
    //shellSort(array, 5);
    int n=5;
    shellsort(n, compare, swap);
    for (int i=0; i<5; i++){
        printf("%d", array[i]);
    }
  }