public class Main {
    public static void main(String[] args) {
        Universe u1=new Universe("AAAAA");
        System.out.println(u1.getName());
        System.out.println(u1.getCounter());
        u1.addParticle(1,0);

        System.out.println(u1.getCounter());
        u1.addParticle(1,1);
        System.out.println(u1.getCounter());

        System.out.println(u1.getRadius());
    }
}
