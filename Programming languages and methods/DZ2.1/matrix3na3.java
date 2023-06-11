package org.example;

public class matrix3na3 {
    private int[][] matr;
    public matrix3na3(){
        matr=new int[3][3];
        for (int i=0;i<3;i++){
            for (int k=0;k<3;k++){
                matr[i][k]=10+(int)(Math.random()*30);
            }
        }
    }
    public int simetric(){
        for (int i=0;i<3;i++) {
            for (int k = i; k < 3; k++) {
                if (matr[i][k]!=matr[k][i]){
                        return 0;
                }
            }
        }
        return 1;
    }

    public String toString() {
        String res="\n";
        for (int i=0;i<3;i++){
            for (int k=0;k<3;k++){
                res+=matr[i][k]+" ";
            }
            res+="\n";
        }
        return res;
    }
}
