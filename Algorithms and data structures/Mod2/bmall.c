#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int maxx(int a, int b){
    if (a>b){
        return a;
    }
    return b;
}
int* suffix(char *s){
    int *q;
    int lens, t,i;
    lens=strlen(s);
    q=malloc(sizeof(int)*(lens+1));
    q[lens-1]=t=lens-1;
    i=lens-2;
    while (i>=0){
        while((t<lens-1) && (s[t]!=s[i])){
            t=q[t+1];
        }
        if (s[t]==s[i]){
            t=t-1;
        }
        q[i]=t;
        i=i-1;
    }
    return q;
}
int* delta1(char *s, int size){
    int *b;
    b=malloc(sizeof(int)*(size+1));
    int a, j, lens;
    lens=strlen(s);
    a=0;
    while (a<size){
        b[a]=lens;
        a=a+1;
    }
    j=0;
    while(j<lens){
        b[s[j]]=lens-j-1;
        j=j+1;
    }
    return b;
}

int* delta2(char *s){
    int *b;
    int lens, i, t;
    lens=strlen(s);
    b=malloc(sizeof(int)*(lens+1));
    int *q=suffix(s);
    i=0;
    t=q[0];
    while(i<lens){
        while(t<i){
            t=q[t+1];
        }
        b[i]=-i+t+lens;
        i=i+1;
    }
    i=0;
    while(i<lens-1){
        t=i;
        while(t<lens-1){
            t=q[t+1];
            if(s[i]!=s[t]){
                b[t]=-(i+1)+lens;
            }
        }
        i=i+1;
    }
    free(q);
    return b;
}

int* bmsubst(char *s, int size, char *t){
    int *b1, *b2, *mas;
    int lens, lent, i, k, c;
    lens=strlen(s);
    lent=strlen(t);
    b1=delta1(s, size);
    b2=delta2(s);
    mas=malloc(sizeof(int)*(lent+1));
    for (int j=0; j<lent+1; j++){
        mas[j]=0;
    }
    c = 1;
    k=lens-1;
    while(k<lent){
        i=lens-1;
        while(t[k]==s[i]){
            if (i==0){
                mas[c]=k;
                mas[0]=c;
                c+=1;
                break;
            }
            i=i-1;
            k=k-1;
        }
        k=k+maxx(b1[t[k]], b2[i]);
    }
    free(b1);
    free(b2);
    return mas;
}

int main(int argc, char **argv){
    char *s, *t;
    s=argv[1];
    t=argv[2];
    int *res;
    res=bmsubst(s,100000, t);
    int len;
    len=res[0];
    for (int i=1; i<=len; i++){
        printf("%d ", res[i]);
    }
    free(res);
    return 0;
}



//++++++
//./bmall jjjGjw3jjj jjjCCjjjCjjjujCjjjGjw3jjjjiju8jjjGjjjGkkjjkjjjGjw3j8jjjxj8xj9jjj2jjjGkjj9Ojxj8jjjGjw3jjjGkSjj2Ajjj