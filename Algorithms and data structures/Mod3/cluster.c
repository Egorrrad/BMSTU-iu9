#include <stdio.h>
#include <stdlib.h>

void swap(int *base, int a, int b)
{
    long per;
    per = base[a];
    base[a] = base[b];
    base[b] = per;
}
int max(int a, int b){
    if (a > b){
        return a;
    }
    return b;
}
struct PriorityQueue
{
    int *heap;
    int cap;
    int count;
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
    int i;
    i = q->count;
    q->count = i + 1;
    q->heap[i] = ptr;
    while (i > 0 && q->heap[(i - 1) / 2] > q->heap[i])
    {
        swap(q->heap, (i - 1) / 2, i);
        i = (i - 1) / 2;
    }
}

void heapify(int i, int nel, int *base)
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

int ExtractMax(qu *q)
{
    int ptr;
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
    q->heap = malloc(sizeof(int) * len);
    q->cap = len;
    q->count = 0;
}
void del(qu *q){
    free(q->heap);
}
int main(){
    int n, m, t1, t2, t, res;
    scanf("%d", &n);
    qu *q;
    q=malloc(sizeof(qu));
    scanf("%d", &m);
    initQu(q, n);
    for (int i=0; i<m; i++){
        scanf("%d %d", &t1, &t2);
        if (i>=n){
            t = max(t1, ExtractMax(q));
        }
        else{
            t=t1;
        }
        insert(q, t+t2);
    }
    res=0;
    for (int i=0; i<n; i++){
        res=max(res, q->heap[i]);
    }
    printf("%d\n",res);
    del(q);
    free(q);
    return 0;
}

