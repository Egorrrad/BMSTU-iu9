#include <math.h>
#include <stdio.h>


int main(){
	int k;
	scanf("%d", &k);
	long long a=0;
	long long n;
	for (int i=0; i<k; i++){
		scanf("%lld", &n);
		a+=pow(2, n);
	}
	int j;
	scanf("%d", &j);
	long long b=0;
	for (int i=0; i<j; i++){
		scanf("%lld", &n);
		b+=pow(2, n);
	}
	a=a&b;
	for (int i=0; i<32; ++i){
		if (a%2==1){
			printf("%d ", i);
		}
		a=a/2;
	}
 	return 0;
 }
//21 0 2 4 5 8 10 11 12 13 14 15 17 18 21 22 23 24 25 28 30 31
//28 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 21 22 23 24 25 28 29 30 31

//2 4 5 8 10 11 12 13 14 15 18 21 22 23 24 25 28 30 31


//3 1 2 3 
//4 1 2 5 6

//1 2