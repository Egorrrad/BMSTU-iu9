#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct Elem
{
    struct Elem *prev, *next;
    int v;
} elem;

elem *initDoubleLinkedList(int num)
{
    elem *l;
    l = malloc(sizeof(elem));
    l->prev = l;
    l->next = l;
    l->v = num;
    return l;
}

int empty(elem *l)
{
    if (l->next == l)
    {
        return 1;
    }
    return 0;
}

int length(elem *l)
{
    int len;
    elem *x;
    len = 0;
    x = l;
    while (x->next != l)
    {
        len += 1;
        x = x->next;
    }
    return len;
}

elem *search(elem *l, int v)
{
    elem *x;
    x = l->next;
    while (x != l && x->v != v)
    {
        x = x->next;
    }
    return x;
}

void insertAfter(elem *x, int num)
{
    elem *z;
    elem *y;
    y = malloc(sizeof(elem));
    y->v = num;
    z = x->next;
    x->next = y;
    y->prev = x;
    y->next = z;
    z->prev = y;
}

void listprint(elem *lst)
{
    elem *p;
    p = lst;
    while (p->next!=lst)
    {
        printf("%d ", p->v);
        p = p->next;
    }
    printf("%d", p->v);
    printf("\n");
}
void delete(elem *l){
    elem *p, *lst;
    lst=l;
    while(lst->next!=l){
        lst=lst->next;
        free(lst->prev);
    }
    free(lst);
}
int main()
{
    int n, num;
    scanf("%d", &n);
    elem *list, *p;
    scanf("%d", &num);
    list = initDoubleLinkedList(num);
    for (int i=1; i<n; i++){
        scanf("%d", &num);
        insertAfter(list, num);
    }
    for (int i = 0; i < n; i++)
    {
        p = list;
        while (p->next != list)
        {
            if (p->v > p->next->v)
            {
                int t = p->v;
                p->v = p->next->v;
                p->next->v = t;
            }
            p = p->next;
        }
    }
    listprint(list);
    delete(list);
    return 0;
}

//+++