#include <stdio.h>
#include <stdlib.h>
#include <string.h>

long gcd (long a, long b) {
    if (b == 0)
        return a;
    else
        return gcd (b, a % b);
}

void build_tree_gcd(long *array, long *tree, long v, long l, long r)
{
    long m;
    if (l == r - 1) {
        tree[v] = array[l];
        return;
    }
    m = (l + r) / 2;
    build_tree_gcd(array, tree, 2 * v + 1, l, m);
    build_tree_gcd(array, tree, 2 * v + 2, m, r);
    tree[v] = gcd(tree[2 * v + 1], tree[2 * v + 2]);
}
long gcd_tree(long *tree, long v, long l, long r, long a, long b)
{
    if ((b <= l) || (r <= a))
    {
        return 0;
    }
    if ((a <= l) && (r <= b))
    {
        return tree[v];
    }
    long m = (l + r) / 2;
    return gcd(gcd_tree(tree, 2 * v + 1, l, m, a, b), gcd_tree(tree, 2 * v + 2, m, r, a, b));
}

int main()
{
    long *tree;
    long *array;
    long n, m;
    long l, r, res;
    long k;
    scanf("%ld", &n);
    array = malloc(sizeof(long) * n);
    tree = malloc(sizeof(long) * (4 * n));
    for (k = 0; k < n; k++)
    {
        scanf("%ld ", &array[k]);
        if (array[k]<0){
            array[k]=-array[k];
        }
    }
    scanf("%ld", &m);
    build_tree_gcd(array, tree, 0, 0, n);
    for (k = 0; k < m; k++)
    {
        scanf("%ld %ld", &l, &r);
        printf("%ld\n", gcd_tree(tree,0, 0, n, l, r + 1));

    }
    free(array);
    free(tree);
    return 0;
}

//++++++