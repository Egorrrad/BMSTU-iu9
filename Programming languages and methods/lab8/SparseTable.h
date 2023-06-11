

#ifndef LAB8_SPARSETABLE_H
#define LAB8_SPARSETABLE_H

template<typename T>
class SparseTable{
private:
    T** mas;
    int n;
    T max(T e1, T e2){
        if (e1>e2){
            return e1;
        }
        return e2;
    }
public:
    SparseTable(int k){
        n=k;
        mas=new T*[n];
        for(int i=0;i<n;i++){
            mas[i]=new T[10];
        }
    }
    int getRazm(){
        return n;
    }
    T getElem(int i, int j){
        return this->mas[i][j];
    }
    void setElem(int i,int j, T e){
        this->mas[i][j]=e;
    }
    T maximum(int k1,int k2){
        T m=mas[k1][k2];
        for (int i=k1;i<k2;i++){
            for (int k=0;k<10;k++){
                m=max(m,mas[i][k]);
            }
        }
    }
};



#endif //LAB8_SPARSETABLE_H
