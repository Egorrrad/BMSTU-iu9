#include <math.h>
#include <stdio.h>


int main(void){
    int arr[1000000];
    int x, size, top, k, s;
    long maxsum=-1000000000000000000;
    scanf("%d",&size);
    for (int i=0; i<size;i++){
        scanf("%d",&x);
        arr[i]=x;
    }
    scanf("%d", &k);
    if (k==1){
        maxsum=arr[0];
        for (int i=0; i<size-k+1;i++){
            if (arr[i]>maxsum){
                maxsum=arr[i];
            }
        }
    }
    for (int i=0; i<size-k+1;i++){
        int sum=arr[i];
        int s=1;
        int j=i+1;
        while ((s<k) && (j<size)){
            sum+=arr[j];
            s+=1;
            j+=1;
            if (sum>maxsum){
                maxsum=sum;
            }
        }
    }
    printf("%ld",maxsum);
    return 0;
}
//"7
//-193738147 383551304 -415130124 318853847 940376817 920056485 981062438 2
//1901118923 right pizdec sovpalo kryto
//1860433302 me


//1933213652 right
//1665432550 me

//"6
//-780860612 322734993 -682623151 182871636 293119823 -250830677 1
