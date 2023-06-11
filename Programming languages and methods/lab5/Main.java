import java.util.ArrayList;


public class Main {
    public static void main(String[] args) {
        MnogoBus Parking=new MnogoBus();
        ArrayList<Bus> potok=Parking.Stream(3,20,20);
        potok.forEach(s->System.out.println("Bus "+s.getNum()));
        System.out.println("Need to "+Parking.getMinBuses(20)+" buses");
    }
}

