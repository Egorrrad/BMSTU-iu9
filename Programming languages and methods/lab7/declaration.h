//
// Created by Egor Diachkov on 07/04/2023.
//

#ifndef UNTITLED_DECLARATION_H
#define UNTITLED_DECLARATION_H

class Matrix{
private: int m, n, **mas;
    class Row {
    private:
        Matrix *matrix;
        int i;
    public:
        Row(Matrix * matrix, int i);
        int& operator [] (int j);
    };
public:
    Matrix::Row operator[](int i) {
        return Row(this, i);
    }
public:
    Matrix(int k,int k1){
        m = k;
        n = k1;
        mas=new int*[n];
        for(int i=0;i<m;i++){
            mas[i]=new int[n];
            for(int k=0;k<n;k++){
                mas[i][k]=i*10+k;
            }
        }
    }
    int getM(){
        return m;
    }
    int getN(){
        return n;
    }
    void swapStroki(int i, int j){
        int* a=mas[i];
        mas[i]=mas[j];
        mas[j]=a;
    }
    void swapStolb(int i, int j){
        int a;
        for(int k=0;k<m;k++){
            a=mas[k][i];
            mas[k][i]=mas[k][j];
            mas[k][j]=a;
        }
    }
    int& returner(int i,int j){
        return mas[i][j];
    }
};


#endif //UNTITLED_DECLARATION_H
