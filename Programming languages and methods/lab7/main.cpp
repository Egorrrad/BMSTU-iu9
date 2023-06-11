#include <iostream>
#include "declaration.h"


int main() {
    Matrix m=*new Matrix(9,10);
    std::cout << m.getM() << std::endl;
    std::cout << m.getN() << std::endl;
    int& b=m.returner(0,0);
    for(int i=0;i<m.getM();i++){
        for(int j=0;j<m.getN();j++){
            b=m.returner(i,j);
            printf("%d ",b);
        }
        printf("\n");
    }
    printf("\n");
    m.swapStroki(0,1);
    m.swapStolb(1,2);
    for(int i=0;i<m.getM();i++){
        for(int j=0;j<m.getN();j++){
            b=m.returner(i,j);
            printf("%d ",b);
        }
        printf("\n");
    }
    std::cout << b << std::endl;
    return 0;
}
