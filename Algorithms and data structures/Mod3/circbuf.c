#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct initQueue
{
    long *data;
    long cap;
    long count;
    long head;
    long tail;
} q;

char *empty()
{
    if (q.count == 0)
    {
        return "true";
    }
    return "false";
}

void enqueue(long x)
{
    int i;
    if (q.count == q.cap)
    {
        long *buf = q.data;
        long bufl = q.cap;
        q.cap =q.cap*2;
        q.data = malloc(sizeof(long)*q.cap);
        if (q.head < q.tail)
        {
            for (i = 0; i < bufl; i++)
                q.data[i] = buf[i];
        }
        else
        {
            for (i = 0; i < q.tail; i++)
                q.data[i] = buf[i];
            for (i = q.head; i < bufl; i++)
                q.data[i + bufl] = buf[i];
            q.head += bufl;
        }
        free(buf);
    }
    q.data[q.tail] = x;
    q.tail += 1;
    if (q.tail == q.cap)
    {
        q.tail = 0;
    }
    q.count += 1;
}

long dequeue()
{
    long x;
    x = q.data[q.head];
    q.head += 1;
    if (q.head == q.cap)
    {
        q.head = 0;
    }
    q.count -= 1;
    return x;
}
int main()
{
    long x;
    q.data = malloc(sizeof(long) * 4);
    q.cap = 4;
    q.count = 0;
    q.head = 0;
    q.tail = 0;
    char name[6];
    while (1)
    {
        scanf("%s", name);
        if (strcmp(name, "END") == 0)
        {
            break;
        }
        if (strcmp(name, "ENQ") == 0)
        {
            scanf("%ld", &x);
            enqueue(x);
        }
        if (strcmp(name, "DEQ") == 0)
        {
            printf("%ld\n", dequeue());
        }
        if (strcmp(name, "EMPTY") == 0)
        {
            printf("%s\n", empty());
        }
    }
    free(q.data);
    return 0;
}

//+++++
