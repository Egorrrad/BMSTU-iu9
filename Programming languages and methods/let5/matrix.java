public class matrix {
    private int matr[][];
    public matrix(int n){
        matr=new int[2][2];
    }
    public void setValue(int i, int j, int el){
        matr[i][j]=el;
    }

    public int[][] getMatrix(){
        return matr;
    }

    public int determ(){
        return matr[0][0]*matr[1][1]-matr[0][1]*matr[1][0];
    }

    public void PRINT(){
        for (int i=0;i<2;i++){
            for (int k=0;k<2;k++){
                System.out.printf("%d ",matr[i][k]);
            }
            System.out.printf("\n");
        }
        System.out.printf("\n");
    }
}
