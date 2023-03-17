#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct Elem
{
    int k;
    int v;
    struct Elem *next;
}elem;

elem *initElem()
{
    elem *el = malloc(sizeof(elem));
    el->next = NULL;
    el->k = 0;
    el->v = 0;
    return el;
}

void initList(elem **list, int m)
{
    int i;
    i = 0;
    while (i < m)
    {
        list[i] = initElem();
        i++;
    }
}
void insert(elem **list, int m, int k, int v)
{
        elem *el = initElem();
        el->k = k;
        el->v = v;
        el->next = list[k % m];
        list[k % m] = el;
}

int find(elem **list, int m, int k)
{
    elem *el = list[k % m];
    while (el != NULL && el->k != k)
        el = el->next;

    if (el != NULL)
        return el->v;
    else
        return 0;
}

elem **delElem(elem **list, int m, int k){
    elem *temp=list[k % m];
    while (temp->next->k != k && temp->next!=NULL){
        temp = temp->next;
    }
    elem *p=temp->next;
    temp->next = temp->next->next; // переставляем указатель
    free(p); // освобождаем память удаляемого узла
   return(list);
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


int main()
{
    int m, i, v,x;
    int mas[100];
    int c=0;
    scanf("%d", &m);
    elem **list;
    list=malloc(sizeof(elem*)*m);
    initList(list, m);
    char name[10];
    while (1)
    {
        scanf("%s", name);
        if (strcmp(name, "END") == 0)
        {
            break;
        }
        if (strcmp(name, "ASSIGN") == 0)
        {
            scanf("%d %d", &i, &v);
            if(v==0){
                list=delElem(list,m,i);
            }
            else{
                insert(list, m, i, v);
            }
        }
        if (strcmp(name, "AT") == 0)
        {
            scanf("%d", &i);
            //printf("%d\n", find(list, m, i));
            x=find(list, m, i);
            mas[c]=x;
            c++;
        }
    }
    free_List(list, m);
    free(list);
    printf("\n");
    for (int i=0; i<c;i++){
        printf("%d\n", mas[i]);
    }
    return 0;
}


