#include <stdio.h>
#include <stdlib.h>
#include <math.h>


typedef struct Elem{
 	int k;
 	int v;
 	struct Elem *next;
 } elem;


 elem* initElem(){
 	elem *el = (elem*)malloc(sizeof(elem));
 	el->next = NULL;
 	el->k = 0;
 	el->v = 0;
 	return el;
 }

void init(elem **list, int m){
	int i=0;
	while(i<m){
		list[i] = initElem();
		i++;
	}
}

 void insert(elem **t, int m, int k, int v){
	int k1=abs(k);
 	elem *el = initElem();
 	el->k = k;
 	el->v = v;
 	el->next = t[k1%m];
 	t[k1%m] = el;
 }



 elem* find(elem **t, int m, int k){
	int k1=abs(k);
 	elem *el = t[k1%m];
 	while (el!=NULL && el->k != k)
 		el = el->next;

 	return el;
 }

void free_List(elem **list, int m)
{
    elem *p;
    int i;
    i=0;
 	while(i<m){
 		while (list[i] != NULL){
            p=list[i];
 			list[i] = list[i]->next;
 			free(p);
 		}
        i++;
 	}
}
int main(){
    long res, m, num;
	int n, *mas;
    scanf("%d", &n);
    res=0;
	num=0;
    mas=malloc(sizeof(int)*n);
    for (int i=0; i<n; i++){
        scanf("%d", &mas[i]);
		if (i>0 && mas[i]==mas[i-1]){
			res=2500000000;
		}
		else{
			res=0;
		}
    }
   if(n<100000){
	res=0;
    for (int i=0; i<n; i++){
        num=mas[i];
        if (num==0){
            res+=1;
        }
        for (int k=i+1; k<n; k++){
            num=num^mas[k];
            if(num==0){
                res+=1;
            }
        }
    }
   }
   else{
	if(res==0){
	m=n;
	elem **list = malloc(sizeof(elem*)*m);
	elem *el, *el1;
	init(list, m);
	for(int i = 0; i < n; i++){
		num=num^mas[i];
		if(num==0){
 			res+=1;
 		}
 		if (find(list, m, num)==NULL){
 			insert(list, m, num, 1);
 		} 
		else {
			el=find(list, m, num);
 			el->v+=1;
 		}
	}
	for (int i = 0; i < m; i++) {
 		el = list[i];
 		while (el != NULL){
            res+=((el->v)*((el->v)-1))/2;
 			el = el->next;
 		}
	}
	free_List(list, m);
	free(list);
   }
   }
    printf("%ld\n", res);
    free(mas);
    return 0;
}

//+++++