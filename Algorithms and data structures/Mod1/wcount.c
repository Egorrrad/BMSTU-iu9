#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int wcount(char *s)
{
    int count;
    count = 0;
    int len=strlen(s);
    if (len==1 && s[0]!=' '){
        count=1;
    }
    for (int i = 0; i<len - 1; i++){
        if ((s[i] == ' ') && (s[i + 1] != ' '))
            count+=1;
        if ((s[0] != ' ') && (i==0))
            count+=1;
    }
    return count;
}
 
int main()
{
    char s[2000];
    fgets(s, 1000, stdin);
    int len=strlen(s);
    s[len-1]='\0';
    printf("%d\n",wcount(s));
    return 0;
}

//++++++
//@%< !x 74. ; s0 [ P. ke \ t p r = XeD gcB 4
//      c]bf& N 5hpHAhV20? 6 }IH0 Q< } 6   