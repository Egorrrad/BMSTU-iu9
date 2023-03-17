#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct Elem {
    struct Elem *next;
    char *word;
}elem;

elem* create(char *data){
    elem *el=malloc(sizeof(elem));
    el->word=data;
    el->next=NULL;
    return el;
}
elem* add_st(char *data, elem *list){
    elem *el=malloc(sizeof(elem));
    el->word=data;
    el->next=list;
    return el;
}
void add_en(char *data, elem *list){
    elem *el=malloc(sizeof(elem));
    el->word=data;
    el->next=NULL;
    elem *p=list;
    while(p->next!=NULL){
        p=p->next;
    }
    p->next=el;
}

int srav(char *s1, char *s2){
	int l1, l2;
	l1=strlen(s1);
	l2=strlen(s2);
	return (l1-l2);
}
elem *bsort(elem *list){
    if(list!=NULL){
    elem *p;
    elem *l;
    int l1, l2, i, count;
    char *per;
    i=0;
    l=list;
    while(l){
        p=list;
        while(p->next!=NULL){
            l1=l2=0;
            if(p->word ==NULL || p->next->word==NULL){
                break;
            }
            if (srav(p->word,p->next->word)>0)
            {
                per=p->word;
                p->word=p->next->word;
                p->next->word= per;
            }
            p = p->next;
        }
        l=l->next;
    }
    }
    return list;
}

int main(){
    char *strin, *str, **strings;
    int i, j, k, count, len;
    elem *list, *p;
    strin=calloc(1000, sizeof(char));
    fgets(strin,1000, stdin);
    len=strlen(strin);
    strin[len-1]='\0';
    strings=malloc(sizeof(char*)*1000);
    str=calloc(1001, sizeof(char));
    str[0]=' ';
    for(int r=0; r<len; r++){
        str[r+1]=strin[r];
    }
    str[len]='\0';
    len=strlen(str);
    k=0;
    j=0;
    for (i=0; i<1000; i++){
        strings[i]=calloc(1000, sizeof(char));
    }
    i=1;
    count=0;
    while (i<len){
        if (str[i-1]==' ' && str[i]!=' ' ){
            j=0;
            k=count;
            while (str[i]!=' ' && i<len){
                strings[k][j]=str[i];
                j+=1;
                i+=1;
            }
            strings[k][j] = '\0';
            if (k==0){
                list=create(strings[k]);
            }
            else{
                add_en(strings[k], list);
            }
            count+=1;
        }
        i+=1;
    }
    list=bsort(list);
    while (list!= NULL){
        printf("%s ", list->word);
        p=list;
        list = list->next;
        free(p);
    }
	printf("\n");
    free(str);
    free(strin);
    for (i=0; i<1000; i++){
        free(strings[i]);
    }
    free(strings);
    return 0;
}

//+++++
  
