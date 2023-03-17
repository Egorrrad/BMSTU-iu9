#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

int proverka(char words[10][15], int i, int j, int len1, int len2)
{
    int k = 0;
    int in = 0;
    if ((words[i][len1] != ' ' && words[j][0] != ' ') || (words[i][0] != ' ' && words[j][len2] != ' '))
    {
        while (1)
        {
            if ((in > len1) || (k > len2))
            {
                // if (in==1 && k==1 && len1>1){
                //     k=0;
                // }
                // if (len1==1 && k==1 && len2>1){
                //   k=0;
                //}
                break;
            }
            if (words[i][in] == words[j][k] && words[i][in] != ' ')
            {
                k += 1;
            }
            else
            {
                if (k != 0)
                {
                    in -= 1;
                }
                k = 0;
            }
            in += 1;
        }
    }
    return k;
}

int main(){
    int n;
    int sum=0;
    scanf ("%d", &n);
    if (n>9){
    int f1, f2, k1, k2;
    // n = 5;
    char words[10][15]; 
    for (int i = 0; i < n; i++)
    {
        scanf("%s", words[i]);
    }

    int len, len1, len2, res, k, i, j;
    len = len1 = len2 = res = k = 0;
    for (i = 0; i < n - 1; i++)
    {
        f1 = f2 = 0;
        k1 = k2 = 0;
        len1 = strlen(words[i]) - 1;
        for (j = i + 1; j < n; j++)
        {
            len2 = strlen(words[j]) - 1;
            k1 = proverka(words, i, j, len1, len2);
            
                k2 = proverka(words, j, i, len2, len1);
            
            if (k1 > k2)
            {
                if (f1!=1){
                for (int c = 0; c < k1; c++)
                {
                    words[j][c] = ' ';
                }
                f1 = 1;
                }
            }
            else
            {
                if ((k2 != 0) && (f2!=1))
                {
                    for (int c = 0; c < k2; c++)
                    {
                        words[j][len2 - c] = ' ';
                    }
                    f2 = 1;
                }
            }
        }
    }
            

    if (n > 2)
    {
        i = 0;
        j = n - 1;
        len1 = strlen(words[i]) - 1;
        len2 = strlen(words[j]) - 1;
        if (words[i][len1] != ' ' && words[j][0] != ' ')
        {
            k = proverka(words, i, j, len1, len2);
            for (int c = 0; c < k; c++)
            {
                words[i][len1 - c] = ' ';
            }
        }
        else
        {
            if (words[i][0] != ' ' && words[j][len2] != ' ')
            {
                k = proverka(words, j, i, len2, len1);
            }
            for (int c = 0; c < k; c++)
            {
                words[i][c] = ' ';
            }
        }
    }
    for (int i = 0; i < n; i++)
    {
        //printf("%s\t", words[i]);
        for (int c = 0; c < strlen(words[i]); c++)
        {
            if (words[i][c] != ' ')
            {
                res += 1;
            }
        }
    }
    printf("%d\n", res);
}

    else{
    int len1, len2, len, k, f, i, j, c, d, c1, c2, f1, f2, d1, d2;
    char str[10][15];
    char *e1, *e2;
    for (int i=0; i<n; i++){
        scanf("%s", str[i]);
    }
    for (i=0; i<n-1; i++){
        e1=str[i];
        len1=strlen(str[i]);
        f=f1=f2=c=c1=c2=k=0;
        for (j=i+1; j<n; j++){
            if ((f==2) || (c==2) || (f==1 && c==1)){
                j=n;
            }
            e2=str[j];
            len2=strlen(str[j]);
        if (len1<len2){
            len=len1;
        }
        else{
            len=len2;
        }
        if (len==1){
            if (len1==1){
                if(e1[0]==e2[0]){
                    e2[0]=' ';
                    c1=1;
                }
                else{
                    if(e1[0]==e2[len2-1]){
                        e2[0]=' ';
                        c2=1;
                    }
                }
            }
            else{
                if(e1[0]==e2[0]){
                    e2[0]=' ';
                    c1=1;
                }
                if(e1[0]==e2[len2-1]){
                    e2[len2-1]=' ';
                    c2=1;
                }
            }
        }
        else{
            len=2;
        for (k=0; k<2; k++){
            if ((e1[len1-1-k-1]==e2[k]) && (e1[len1-1-k]==e2[k+1]) && e2[k]!=' ' && e2[k+1]!=' ' && f1!=1){
                e2[k]=' ';
                e2[k+1]=' ';
                k=len;
                f1=1;
            }
            else{
                if ((e1[k]==e2[len2-1-k-1]) && (e1[k+1]==e2[len2-1]) && e1[k]!=' ' && e1[k+1]!=' ' && f2!=1){
                    e2[len2-1]=' ';
                    e2[len2-1-k-1]=' ';
                    k=len;
                    f2=1;
                }
                else{
                    if (e1[len1-1-k]==e2[k] && e2[k]!=' ' && c1!=1 && f1!=1){
                            e2[k]=' ';
                            k=len;
                            c1=1;
                    }
                    else{
                        if (e2[len2-1-k]==e1[k] && e1[k]!=' ' && c2!=1 && f2!=1){
                            e2[len2-1-k]=' ';
                            c2=1;
                        }
                        k=len;
                    }
                }
            }
        }
        }
        f=f1+f2;
        c=c1+c2;
    }
    }
    char res[1000];
    strcpy(res, str[0]);
    for (i=1; i<n; i++){
        strcat(res, str[i]);
    }
    len=strlen(res);
    for (i=0; i<len; i++){
        if (res[i]!=' '){
            sum+=1;
        }
    }
    printf("%d\n", sum);
    //printf("%s\n", res);
    }
    return 0;
}
