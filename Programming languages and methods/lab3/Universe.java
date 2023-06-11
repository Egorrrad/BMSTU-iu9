
public class Universe {
    private int[][] mas;
    public static int count=0;
    private String name;
    public Universe(String nam){
        mas=new int[10][2];
        System.out.println("Создана вселенная: "+nam);
        name=nam;
    }
    public void addParticle(int x, int y){
        mas[count][0]=x;
        mas[count][1]=y;
        count++;
        System.out.println("Создана частица: "+Integer.toString(x)+" "+Integer.toString(y));
    }
    public String getName(){
        return "Имя вселенной: "+name;
    }
    public String getCounter(){
        return "Количество частиц во вселенной: " + Integer.toString(count);
    }
    private double r;
    public String getRadius(){
        double sx, sy;
        sx=sy=0;
        for (int i = 0; i < mas.length; i++){
            sx+=mas[i][0];
            sy+=mas[i][1];
        }
        sx=sx/count;
        sy=sy/count;
        double s=0;
        for (int i = 0; i < mas.length; i++){
            s+=Math.sqrt(Math.pow(sx-mas[i][0],2)+Math.pow(sy-mas[i][1],2));

        }
        r=s/count;
        return "Средний радиус: " + Double.toString(r);
    }
}
