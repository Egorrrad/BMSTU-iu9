#include <stdio.h>
#include <string.h>
#include <stdlib.h>


char* concat(char **s, int n)
{
    char *st, *so;
    int len, lens, c;
    len=1;
    c=0;
    for (int i=0; i<n; i++){
        len+=strlen(s[i]);
    }
    st=malloc(sizeof(char)*len);
    for (int i=0; i<n; i++){
        lens=strlen(s[i]);
        for (int k=0; k<lens; k++){
            st[c]=s[i][k];
            c+=1;
        }
    }
    st[len-1]='\0';
    return st;
}
int main(){
    int n, le, i;
    scanf("%d", &n);
    char **array;
    array=malloc(sizeof(char*)*(n+1));
    for (i=0; i<n; i++){
        array[i]=malloc(sizeof(char)*1001);
        scanf("%s", array[i]);
    }
    char *result=concat(array, n);
    printf("%s\n", result);
    free(result);
    for (i=0; i<n; i++){
        free(array[i]);
    }
    free(array);
    return 0;
}



//+++++
