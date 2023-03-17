#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main(){
    long mas[26]={0};
    char str[1000001];
    scanf("%s", str);
    int len=strlen(str);
    int n;
    for (int i=0; i<len; i++){
        n=str[i];
        mas[n-97]+=1;
    }
    char string[1000000];
    for (int i=0; i<26; i++){
        if (mas[i]!=0){
            for (int k=0; k<mas[i]; k++){
            printf("%c", 97+i);
            }
        }
    }
    return 0;
}

//Aa Bb Cc Dd Ee Ff Gg Hh Ii Jj Kk Ll Mm Nn Oo Pp Qq Rr Ss Tt Uu Vv Ww Xx Yy Zz
/*int main(){
    char str[1000001];
    scanf("%s", str);
    int len=strlen(str);
    char p;
    for (int i=1; i<len; i++){
        int k=i;
        while (k>0){
            if (str[k]<str[k-1]){
                p=str[k];
                str[k]=str[k-1];
                str[k-1]=p;
                k-=1;
            }
            else{
                k-=1;
            }
        }
        
    }
    printf("%s\n", str);

}
*/

//encyclopedia

//accdeeilnopy 
