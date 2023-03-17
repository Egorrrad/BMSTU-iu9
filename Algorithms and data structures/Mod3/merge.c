#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void swap(long *base, long a, long b)
{
    long per;
    per = base[a];
    base[a] = base[b];
    base[b] = per;
}

struct PriorityQueue
{
    long *heap;
    long cap;
    long count;
};
typedef struct PriorityQueue qu;
long maxi(qu *q)
{
    return (q->heap[0]);
}

int empty(qu *q)
{
    if (q->count == 0)
    {
        return 1;
    }
    return 0;
}

void insert(qu *q, long ptr)
{
    long i;
    i = q->count;
    q->count = i + 1;
    q->heap[i] = ptr;
    while (i > 0 && q->heap[(i - 1) / 2] > q->heap[i])
    {
        swap(q->heap, (i - 1) / 2, i);
        i = (i - 1) / 2;
    }
}

void heapify(int i, int nel, long *base)
{
    int l, r, j;
    int b = 0;
    while (1)
    {
        l = 2 * i + 1;
        r = l + 1;
        j = i;
        if (l < nel && base[i] > base[l])
            i = l;
        if (r < nel && base[i] > base[r])
            i = r;
        if (i == j)
        {
            break;
        }
        swap(base, i, j);
    }
}

long Max(qu *q)
{
    long ptr;
    ptr = q->heap[0];
    q->count -= 1;
    if (q->count > 0)
    {
        q->heap[0] = q->heap[q->count];
        heapify(0, q->count, q->heap);
    }
    return ptr;
}
void initQu(qu *q, int len){
    q->heap = malloc(sizeof(long) * len);
    q->cap = len;
    q->count = 0;
}
void del(qu *q){
    free(q->heap);
}
void printQ(qu *q){
    while (q->count > 0)
    {
        printf("%ld ", Max(q));
    }
}
struct array{
    long *sizes;
    long *start;
    long **mas;
};
typedef struct array ar;

int main()
{
    long n, i, k, len, per;
    qu *q;
    q=malloc(sizeof(qu));
    scanf("%ld", &n);
    initQu(q, n);
    ar arr;
    arr.sizes=malloc(sizeof(long)*n);
    arr.start=malloc(sizeof(long)*n);
    arr.mas=malloc(sizeof(long*)*n);
    len = 0;
    for (i = 0; i < n; i++)
    {
        scanf("%ld ", &per);
        arr.sizes[i]=per;
        arr.start[i]=0;
        arr.mas[i]=malloc(sizeof(long)*per);
        len += per;
    }
    initQu(q, len);
    for (i=0; i<n; i++){
        for(k=0; k<arr.sizes[i]; k++){
            scanf("%ld", &arr.mas[i][k]);
        }
    }
    for (i=1; i<len+1; i++){
        for (k=0; k<n; k++){
            if (arr.start[k]<arr.sizes[k]){
                insert(q, arr.mas[k][arr.start[k]]);
                arr.start[k]+=1;
            }
        }
        printf("%ld ", Max(q)); 
    }
    printf("\n");
    for (i = 0; i < n; i++)
    {
        free(arr.mas[i]);
    }
    del(q);
    free(arr.mas);
    free(arr.sizes);
    free(arr.start);
    free(q);
    return 0;
}