import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

class AutomatMoore{
    private int n;
    private String[] sost;
    private int m1;
    private String[] alphavit;
    private int m2;
    private int[][] pereh;
    private int[][] out;
    public AutomatMoore(Scanner in){
        n=in.nextInt();
        sost=new String[n];
        for (int i=0;i<n;i++){
            sost[i]=in.next();
        }
        m1=in.nextInt();
        alphavit=new String[m1];
        for(int i=0;i<m1;i++){
            alphavit[i]= in.next();
        }
        m2= in.nextInt();
        pereh=new int[m2][n];
        out=new int[m2][n];
        String num;
        for(int i=0;i<m2;i++){
            for (int k=0;k<n;k++) {
                num=in.next();
                pereh[i][k] = Integer.parseInt(num);
            }
        }
        for(int i=0;i<m2;i++){
            for (int k=0;k<n;k++) {
                out[i][k] = in.nextInt();
            }
        }
    }
    public void printDotAutomat(){
        System.out.println("digraph {\n\trankdir = LR");
        Map<String,Integer> m=new HashMap<>();
        int count=0;
        for (int i=0;i<m2;i++){
            for (int k=0;k<n;k++){
                //String s="("+i+","+alphavit[k]+")";
                String s="("+pereh[i][k] + "," + alphavit[out[i][k]]+")";
                if(m.get(s)==null) {
                    m.put(s, count);
                    System.out.println("\t"+count+" "+"[label = \""+s+"\"]");
                    count++;
                }
            }
        }
        for (int i = 0; i < m2; i++){
            for (int k = 0; k < n; k++){
                String lab=sost[k];
                for (int j=0;j<m1;j++) {
                    if(m.get("("+i + "," + alphavit[j]+")")!=null) {
                        String s = "\t" + m.get("(" + i + "," + alphavit[j] + ")") + " -> " + m.get("(" + pereh[i][k] + "," + alphavit[out[i][k]] + ")") + " [label = \"" + lab + "\"]";
                        System.out.println(s);
                    }
                }
            }
        }
        System.out.println("}");
    }
}



public class Mealy2Moore {
    public static void main(String[] args){
        Scanner in=new Scanner(System.in);
        AutomatMoore a=new AutomatMoore(in);
        a.printDotAutomat();
    }
}
