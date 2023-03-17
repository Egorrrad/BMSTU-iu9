#include <stdio.h>
#include <stdlib.h>
#include <string.h>

long long *tree;
long long *array;
long n;

long long max(long long a, long long b)
{
    if (a > b)
    {
        return a;
    }
    return b;
}

void build_tree_max(long long v, long long l, long long r)
{
    long long m;
    if (l == r - 1)
    {
        tree[v] = array[l];
        return;
    }
    m = (l + r) / 2;
    build_tree_max(2 * v + 1, l, m);
    build_tree_max(2 * v + 2, m, r);
    tree[v] = max(tree[2 * v + 1], tree[2 * v + 2]);
}
long long max_tree(long long v, long long l, long long r, long long a, long long b)
{
    long long m, maxim, e1, e2;
    if ((b <= l) || (r <= a))
    {
        return -1e18;
    }
    if ((a <= l) && (r <= b))
    {
        return tree[v];
    }
    m = (l + r) / 2;
    e1 = max_tree(2 * v + 1, l, m, a, b);
    e2 = max_tree(2 * v + 2, m, r, a, b);
    maxim = max(e1, e2);
    return maxim;
}
void update(long long v, long long l, long long r, long long i, long long x)
{
    if (l == r - 1)
    {
        tree[v] = x;
        return;
    }
    long long m;
    m = (l + r) / 2;
    if (i < m)
    {
        update(2 * v + 1, l, m, i, x);
    }
    else
    {
        update(2 * v + 2, m, r, i, x);
    }
    tree[v] = max(tree[2 * v + 1], tree[2 * v + 2]);
}

int main()
{
    long long l, r;
    long k;
    char name[5];
    long long *res;
    scanf("%ld", &n);
    array = malloc(sizeof(long long) * n);
    tree = malloc(sizeof(long long) * (4 * n));
    for (k = 0; k < n; k++)
    {
        scanf("%lld ", &array[k]);
    }
    build_tree_max(0, 0, n);
    while (1)
    {
        // actions
        scanf("%s", name);
        if (strcmp(name, "END") == 0)
        {
            break;
        }
        scanf("%lld %lld", &l, &r);
        if (strcmp(name, "MAX") == 0)
        {
            // actions for max
            printf("%lld\n", max_tree(0, 0, n, l, r + 1));
        }
        if (strcmp(name, "UPD") == 0)
        {
            // actions for upd
            update(0, 0, n, l, r);
        }
    }
    free(array);
    free(tree);
    return 0;
}


//+++++
