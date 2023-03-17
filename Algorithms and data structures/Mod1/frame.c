#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void frame(int(str), int(col), char *slovo){
    int len=strlen(slovo);
    int j=0;
    if ((len+1<col) && (str>2)){
        int c1;
        if ((col-len)%2==0){
            c1=(col-len)/2;
        }
        else{
            c1=(col-len)/2;
        }
        int c2;
        if (str%2==0){
            c2=(str/2)-1;
        }
        else{
            c2=(str/2);
        }
        char array[60][60]={" "};
        for (int i=0; i<str; i++){
            for (int k=0; k<col; k++){
                if ((i==0) || (i==str-1) || (k==0) || (k==col-1)){ 
                    array[i][k]='*';
                    }
                else{
                    if (((k==c1) && (i==c2)) && (j<len)){
                        array[i][k]=slovo[j];
                        j+=1;
                        c1+=1;
                        }
                    else{
                        array[i][k]=' ';
                    }
            
                }
        }
    }
    for (int i=0; i<str; i++){
        if (i>0){
            printf("\n");
        }
        for (int k=0;k<col;k++){
            printf("%c", array[i][k]);
            }
        }
    }
    else{
        printf("Error");
    }
    printf("\n");
}

int main(int argc, char *argv[]){
    if (argc!=4){
        printf("Usage: frame <height> <width> <text>\n");
    }
    else{
        frame(atoi (argv[1]), atoi (argv[2]), argv[3]);
    }
    return 0;
}

//25 50 The quick brown fox jumps over the lazy dog
