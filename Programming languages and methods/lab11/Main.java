public class Main {
    public static void main(String[] args) {
        String s;
        s="x: \"y\" + (z: \"a\" + \"b\" + \"c\")";
        System.out.println(new Parser(s).parse());

        s="x: \"y\" + \"(z: \"a\" + \"b\" + \"c\")";
        System.out.println(new Parser(s).parse());

    }
}
