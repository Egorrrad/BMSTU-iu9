public class Chess {
    private int[][]mas;
    private int n;
    private int m;
    public Chess(int a, int b){
        this.n=a;
        this.m=b;
        mas=new int[n][m];
        int c=1;
        for (int i = 0; i < 2; i++){
            for (int j = 0; j < m;j++){
                mas[i][j]=c;
                c+=1;
            }
        }
        for (int i = n-2; i < n; i++){
            for (int j = 0; j < m;j++){
                mas[i][j]=c;
                c+=1;
            }
        }
    }
    public void getPosition(int pos) {
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                if (mas[i][j] == pos) {
                    System.out.println(i+" "+j);
                }
            }
        }
    }
    public void changePosition(int posi, int a, int b){
        mas[a][b]=posi;
        System.out.println("Новая позиция: "+a+" "+b);
    }
    public void getPole(){
        for (int i = 0; i < n; i++){
            String s=" ";
            for (int j = 0; j < m;j++){
                s+=mas[i][j]+" ";
            }
            System.out.println(s);
        }
    }
}
