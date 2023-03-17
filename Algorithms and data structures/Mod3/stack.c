#include <stdio.h>
#include <string.h>
#include <stdlib.h>

long max(long a, long b){
    if (a>b){
        return a;
    }
    return b;
}

long min(long a, long b){
    if (a<b){
        return a;
    }
    return b;
}
struct stak{
    long *data;
    long top;
} stack;

void push (long x){
    stack.data[stack.top]=x;
    stack.top+=1;
}

long pop (){
    long x;
    stack.top-=1;
    x=stack.data[stack.top];
    return x;
}


int main(){
    int n;
    n=100000;
    long x, a, b;
    char name[6];
    stack.data=malloc(sizeof(long)*n);
    stack.top=0;
    while(1){
        scanf("%s", name);
        //printf("%s", name);
        if(strcmp(name, "END")==0){
            break;
        }
        if (strcmp(name, "CONST")==0){
            scanf("%ld", &x);
            push(x);
        }
        if (strcmp(name, "ADD")==0){
            a=pop();
            b=pop();
            push(a+b);
        }
        if (strcmp(name, "SUB")==0){
            a=pop();
            b=pop();
            push(a-b);
        }
        if (strcmp(name, "MUL")==0){
            a=pop();
            b=pop();
            push(a*b);
        }
        if (strcmp(name, "DIV")==0){
            a=pop();
            b=pop();
            push(a/b);
        }
        if (strcmp(name, "MAX")==0){
            a=pop();
            b=pop();
            push(max(a,b));
        }
        if (strcmp(name, "MIN")==0){
            a=pop();
            b=pop();
            push(min(a,b));
        }
        if (strcmp(name, "NEG")==0){
            a=pop();
            push(-a);
        }
        if (strcmp(name, "DUP")==0){
            a=pop();
            push(a);
            push(a);
        }
        if (strcmp(name, "SWAP")==0){
            a=pop();
            b=pop();
            push(a);
            push(b);
        }
    }
    printf("%ld\n", pop());
    free(stack.data);
    return 0;
}

//+++++
