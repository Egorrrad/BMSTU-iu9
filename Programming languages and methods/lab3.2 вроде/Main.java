
public class Main {
    public static void main(String[] args) {
        sentence s1= new sentence("s1,s2   s3 s4, s5");
        System.out.println(s1.getSentence());
        System.out.println(s1.getCount());

        sentence s2= new sentence("s1,s2 s3 s4, s5,s6,  s7, s8");
        System.out.println(s2.getSentence());
        System.out.println(s2.getCount());

        sentence s3= new sentence("s1,s2 ,,s3 s4, s5,s6,s7");
        System.out.println(s3.getSentence());
        System.out.println(s3.getCount());

        sentence mas[]={s3,s1,s2};

        mas= s1.sortArr(mas);

        for (int i=0; i< mas.length;i++){
            System.out.println(mas[i].getSentence());
        }
    }
}
