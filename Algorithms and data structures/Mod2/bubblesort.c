#include <math.h>
#include <stdio.h>
#include <stdlib.h>
int *array;


int compare(unsigned long i, unsigned long j){
	if (array[i]==array[j]){
		return 0;
	}
	if (array[i]<array[j]){
		return -1;
	}
	return 1;
}
void swap(unsigned long i, unsigned long j){
	int t=array[i];
	array[i]=array[j];
	array[j]=t;
}


void bubblesort(unsigned long nel,
        int (*compare)(unsigned long i, unsigned long j),
        void (*swap)(unsigned long i, unsigned long j))
{
	int i, j, t, k;
	i=0;
	if(nel==1){
		return;
	}
	while(i<nel){
		j=i/2;
		t=nel-1 - i/2;
		while(j<t){
			if (compare(j,j+1)==1){
				swap(j+1, j);
			}
			j+=1;
		}
		k=t;
		while(k>i/2){
			if (compare(k-1,k)==1){
				swap(k, k-1);
			}
			k-=1;
		}
		i+=2;
	}
}

 int main()
 {
	int n=10;
 	array=malloc(sizeof(int)*n);
	for (int i=0; i<n; i++){
		//scanf("%d", array+i);
		array[i]=100-i;
	}
	bubblesort(n, compare, swap);
	for (int i=0; i<n; i++){
		printf("%d\t", array[i]);
	}
	free(array);
	return 0;
 }



