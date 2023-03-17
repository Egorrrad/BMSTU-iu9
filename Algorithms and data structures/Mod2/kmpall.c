#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int* prefix(char *s)
{
    int t, i, len;
    int *pi;
    len=strlen(s);
    pi = malloc(sizeof(int) * len);
    pi[0]=0;
    t = 0;
    i = 1;
    while (i < len)
    {
        while ((t > 0) && (s[t] != s[i]))
        {
            t = pi[t - 1];
        }
        if (s[t] == s[i])
        {
            t = t + 1;
        }
        pi[i] = t;
        i = i + 1;
    }
    return pi;
}

int* KMPSubst(char *s, char *t)
{
    int q, k, lent, lens, c;
    lent = strlen(t);
    lens = strlen(s);
    int *pi = prefix(s);
    q = 0;
    k = 0;
    c=1;
    int *mas;
    mas=malloc(sizeof(int)*(lent+1));
    for (int i=0; i<lent+1; i++){
        mas[i]=0;
    }
    while (k < lent)
    {
        while ((q > 0) && (s[q] != t[k]))
        {
            q = pi[q - 1];
        }
        if (s[q] == t[k])
        {
            q = q + 1;
        }
        if (q == lens)
        {
            k = k - lens + 1;
            mas[c]=k;
            mas[0]=c;
            c+=1;
            q=0;
        }
        k = k + 1;
    }
    free(pi);
    return mas;
}
int main(int arg, char **argv){
    char *s, *t;
    int *res;
    s = argv[1];
    t = argv[2];
    char *str;
    res = KMPSubst(s, t);
    int len;
    len=res[0];
    for (int i=1; i<=len; i++){
        printf("%d ", res[i]);
    }
    free(res);
    return 0;
}


//++++++