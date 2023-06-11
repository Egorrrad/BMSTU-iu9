#include <iostream>
#include "SparseTable.h"

int main() {
    SparseTable<int> m1= SparseTable<int>(10);
    printf("%d\n",m1.getRazm());

    SparseTable<char> m2= SparseTable<char>(10);
    printf("%d\n",m2.getRazm());
    for(int i=0;i<10;i++){
        for(int k=0;k<10;k++){
            m1.setElem(i,k,rand());
            m2.setElem(i,k,char(rand()%20));
        }
    }
    printf("%d\n",m1.maximum(4,8));
    printf("%c\n",m2.maximum(7,10));
    return 0;
}
