#include <stdlib.h>
#include <stdio.h>
#include <math.h>
union Int32
{
    int x;
    unsigned char bytes[4];
};

typedef union Int32 int32;

void sort(int32 *arr, int32 *arv, int n, int k)
{
    int num, i, c, count;
    count=0;
    c=0;
    i=0;
    while (count < 256)
    {
        for (i = 0; i < n; i++)
        {
            num =arr[i].bytes[k];
            if (num == count)
            {
                arv[c] = arr[i];
                c += 1;
            }
        }
        count+=1;
    }
}
void sort2(int32 *arr, int32 *arv, int n, int k)
{
    int num, i, c, count;
    count=128;
    c=0;
    i=0;
    while (count < 256)
    {
        for (i = 0; i < n; i++)
        {
            num =arr[i].bytes[k];
            if (num == count)
            {
                arv[c] = arr[i];
                c += 1;
            }
        }
        count+=1;
    }
    count=0;
    while (count < 128)
    {
        for (i = 0; i < n; i++)
        {
            num =arr[i].bytes[k];
            if (num == count)
            {
                arv[c] = arr[i];
                c += 1;
            }
        }
        count+=1;
    }
}
int main()
{
    int n;
    scanf("%d", &n);
    union Int32 *array;
    array = malloc(sizeof(int32) * n);
    for (int i = 0; i < n; i++)
    {
        scanf("%d", &array[i].x);
    }
    int32 *arr3=malloc(sizeof(int32)*n);
    sort(array, arr3, n, 0);
    int32 *arr2=malloc(sizeof(int32)*n);
    sort(arr3, arr2, n, 1);
    int32 *arr1=malloc(sizeof(int32)*n);
    sort(arr2, arr1, n, 2);
    int32 *arr0=malloc(sizeof(int32)*n);
    sort2(arr1, arr0, n, 3);
    free(array);
    free(arr3);
    free(arr2);
    free(arr1);
    for (int i = 0; i < n; i++)
    {
        printf("%d  ", arr0[i].x);
    }
    free(arr0);
    return 0;
}

/*
int32* dsort(int32 *s, int n, int in){
    int count[256]={0}, j, k, i;
    j=0;
    while(j<n){
        k=s[j].bytes[in];
        count[k]+=1;
        j++;
    }
    i=1;
    while(i<256){
        count[i]+=+count[i-1];
        i++;
    }
    int32 *d;
    d=malloc(sizeof(int32)*n);
    j=n-1;
    while(j>=0){
        k=s[j].bytes[in];
        i=count[k]-1;
        count[k]=i;
        d[i]=s[j];
        j--;
    }
    return d;
}

void radsort(int32 *s, int n){
    int i;
    for(i=3; i>=0; i--){
        s=dsort(s, n, i);
    }
}
*/

//+++++