#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *fibstr(int n){
    char **ar;
    int l1, l2;
    char *res=NULL;
    ar=malloc(sizeof(char*)*(n+1));
    for (int i=1; i<n+1; i++){
        if (i==1){
            ar[i]=malloc(sizeof(char)*2);
            sprintf(ar[i], "%s", "a");
        }
        if (i==2){
            ar[i]=malloc(sizeof(char)*2);
            sprintf(ar[i], "%s", "b");
        }
        if (i>2){
            l1=strlen(ar[i-2]);
            l2=strlen(ar[i-1]);
            ar[i]=malloc(sizeof(char)*(l1+l2+1));
            sprintf(ar[i], "%s%s", ar[i-2], ar[i-1]);
        }
    }
    res=ar[n];
    for (int i=1; i<n; i++){
        free(ar[i]);
    }
    free(ar);
    return res;
}
int main(){
    int n;
    //n=2;
    scanf("%d", &n);
    char *result=fibstr(n);
    printf("%s\n", result);
    free(result);
    return 0;
}

//+++++++
