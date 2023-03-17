#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int* prefix(char *s)
{
    int t, i, len;
    int *pi;
    len = strlen(s);
    pi = malloc(sizeof(int) * len);
    pi[0] = t = 0;
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
int main(int argc, char **argv)
{
    char *s, *t;
    s = argv[1];
    t = argv[2];
    int lens, lent, len, res;
    lens = strlen(s);
    lent = strlen(t);
    len = lens + lent;
    char *str = calloc(len+1, sizeof(char));
    res=1;
    strcpy(str, s);
    int c=0;
    for (int i=lens; i<len; i++){
        str[i]=t[c];
        c++;
    }
    int *pi;
    pi = prefix(str);
    for (int i = lens; i < len; i++)
    {
        if (pi[i] == 0)
        {
            res=0;
            break;
        }
    }
    if (res==0){
        printf("%s\n", "no");
    }
    else{
        printf("%s\n", "yes");
    }
    free(str);
    free(pi);
    return 0;
}


//+++++