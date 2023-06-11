public class Bus {
    private int vmest;
    private int num;
    private int shirota;
    private int dolgota;
    public Bus(int v, int n, int s, int d){
        this.vmest=v;
        this.num=n;
        this.shirota=s;
        this.dolgota=d;
    }
    public void changeNum(int n){
        this.num=n;
    }
    public int getNum(){
        return num;
    }
    public int getShirota(){
        return shirota;
    }
    public int getDolgota(){
        return dolgota;
    }

    public int getVmest() {
        return vmest;
    }
    public int compareTo(Bus obj){
        return obj.vmest-this.vmest;
    }
}
