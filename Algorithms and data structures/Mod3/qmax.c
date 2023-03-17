#include <stdio.h>
#include <string.h>
#include <stdlib.h>

long max(long a, long b){
    if (a>b){
        return a;
    }
    return b;
}

struct InitDoubleStack
{
   long *data;
   long *max;
   long cap;
   long top1;
   long top2;
}ds;

int empty1()
{
    if (ds.top1 == 0)
    {
        return 1;
    }
    return 0;
}

int empty2()
{
    if (ds.top2 == ds.cap - 1)
    {
        return 1;
    }
    return 0;
}
char* empty(){
    if (empty1()==empty2() && empty1()==1){
        return "true";
    }
    return "false";
}
void push1(long x){
    ds.data[ds.top1]=x;
    if(ds.top1 == 0){
        ds.max[ds.top1] = x;
    } 
    else {
        ds.max[ds.top1] = max(x, ds.max[ds.top1-1]);
    }
    ds.top1+=1;
}

void push2(long x){
    ds.data[ds.top2]=x;
    if(ds.top2 == ds.cap-1){
             ds.max[ds.top2] = x;
       } else {
             ds.max[ds.top2] = max(x, ds.max[ds.top2+1]);
       }
    ds.top2-=1;
}

long pop1(){
    long x;
    ds.top1-=1;
    x=ds.data[ds.top1];
    return x;
}

long pop2(){
    long x;
    ds.top2+=1;
    x=ds.data[ds.top2];
    return x;
}
void enqueue(long x){
    push1(x);
}
long dequeue(){
    long x;
    if (empty2()==1){
        while(empty1()==0){
            push2(pop1());
        }
    }
    x=pop2();
    return x;
}
long qmax(){
  if(ds.top1 == 0){ 
    return ds.max[ds.top2 + 1]; 
}
  if(ds.top2 == ds.cap - 1){ 
    return ds.max[ds.top1 - 1]; 
}
  if(ds.max[ds.top1 - 1] > ds.max[ds.top2 + 1]){
    return ds.max[ds.top1 - 1];
  }
  return ds.max[ds.top2 + 1];
}
int main(){
    char name[6];
    long x, n, len;
    n=10000000;
    ds.data=malloc(sizeof(long)*n);
    ds.max=malloc(sizeof(long)*n);
    ds.cap=n;
    ds.top1=0;
    ds.top2=n-1;
    while(1){
        scanf("%s", name);
        if (strcmp(name, "END")==0){
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
        if (strcmp(name, "MAX") == 0)
        {
            printf("%ld\n", qmax());
        }
    }
    free(ds.data);
    return 0;
}

//++++


