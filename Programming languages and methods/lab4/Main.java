public class Main {
    public static void main(String[] args) {
        Sentence stroka=new Sentence("aaaa bbb ddd");
        stroka.Iterate();
        System.out.println(stroka.getS());
        stroka.changeS("aaaaa wwww tttt");
        System.out.println(stroka.getS());
        stroka.Iterate();

    }
}
