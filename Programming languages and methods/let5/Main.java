/*
Дано множество матриц размера 2х2.
Необходимо реализовать поток структурированного
вывода значений элементов этих матриц,
ранг которых не равен нул􏰚.
 */


import java.util.ArrayList;

public class Main {
    public static void main(String[] args) {
        ArrayList<matrix> matres = new ArrayList<matrix>();
        matrix m1=new matrix(2);
        matrix m2=new matrix(2);
        matrix m3=new matrix(2);
        m3.setValue(0,0,0);
        m3.setValue(0,1,0);
        m3.setValue(1,1,0);
        m3.setValue(1,0,0);
        int c=1;
        for (int i=0;i<2;i++){
            for (int k=0;k<2;k++){
                c++;
                m1.setValue(i,k,c);
                c++;
                m2.setValue(i,k,c);
            }
        }
        matres.add(m1);
        matres.add(m2);
        matres.stream().filter(s->s.determ()!=0).forEach(s->s.PRINT());
    }
}
