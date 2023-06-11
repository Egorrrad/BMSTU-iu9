import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

public class MnogoBus {
    private int k;
    private Bus mas[]=new Bus[5];
    public MnogoBus(){
        mas[0]=new Bus(10, 2,20,30);
        mas[1]=new Bus (15, 1,40,30);
        mas[2]=new Bus (10, 1,70,10);
        mas[3]=new Bus (15, 2,10,40);
        mas[4]=new Bus (10, 2,80,5);
    }

    public ArrayList<Bus> Stream(int k, int shir, int dolg){
        ArrayList<Bus> Buses = new ArrayList<Bus>();
        ArrayList<Bus> Busess = new ArrayList<Bus>();
        Collections.addAll(Buses, mas[0],mas[1],mas[2],mas[3],mas[4]);
        Buses.stream().filter(s->s.getDolgota()-dolg<10 || s.getShirota()-shir<10).limit(k).forEach(s->Collections.addAll(Busess, s));
        return Busess;
    }
    public int getMinBuses(int n){
        AtomicInteger k= new AtomicInteger();
        AtomicInteger c = new AtomicInteger();
        ArrayList<Bus> Buses = new ArrayList<Bus>();
        Collections.addAll(Buses, mas[0],mas[1],mas[2],mas[3],mas[4]);
        Buses.stream().sorted(new BusesComparator())
        .forEach(s->{
            if (k.get()<=n){
                k.addAndGet(s.getVmest());
                c.addAndGet(1);
            }
        });
        return c.get();
    }

    class BusesComparator implements Comparator<Bus> {
        public int compare(Bus a, Bus b){
            return a.compareTo(b);
        }
    }
}

