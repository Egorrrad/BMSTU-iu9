
public class Main
{
    //4 вариант
	public static void main(String[] args) {
	    Matrixname A=new Matrixname("AAAAA");
	    System.out.println(A.name);
	    System.out.println(A.vid);
	    A.Null();
	    System.out.println(A.getSum());

		
	}
}

class Matrix{
    public String vid;
    public int [][]matr;
    public Matrix(String vid){
        matr=new int[3][3];
        this.vid = vid;
    }
    public void Null(){
        for(int i=0; i<3; i++){
            for(int j=0; j<3; j++){
                matr[i][j]=0;
            }
        }
    }
}

class Matrixname extends Matrix{
    public String name;
    public Matrixname(String name){
        super("diag");
        this.name = name;
    }
    public int getSum(){
        int s=0;
        for(int i=0; i<3; i++){
            for(int j=0; j<3; j++){
                if (i==j){
                    s+=matr[i][j];
                }
            }
        }
        return s;
    }
}
