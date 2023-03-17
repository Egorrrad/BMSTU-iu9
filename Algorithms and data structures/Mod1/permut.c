#include <stdio.h>
#include <math.h>

int main(void)
{
    int arr1[8];
    int arr2[8];
    int lab=1;
    for (int i=0;i<8; i++){
        scanf("%d", &arr1[i]);
    }
    for (int i=0;i<8; i++){
        scanf("%d", &arr2[i]);
    }
    for (int i=0; i<8; i++){
        if (lab==0){
           break;
        }
        for (int k=0; k<8; k++){
            if (arr1[i]==arr2[k]){
                lab=1;
                k=8;
            }
            else{
                lab=0;
            }
        }
    }
    if (lab==1){
        printf("yes\n");
    }
    else{
        printf("no\n");
    }
    return 0;
}
