import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

class works{
    private int n;
    private final int inf=0;
    private ArrayList<work> g;
    private boolean[] visited;
    private int time;
    private ArrayList<Integer> sorted;
    private int[] p;
    public works(){
        g=new ArrayList<>();
        n=0;
    }
    public void addNewWork(work w){
        g.add(w);
        n=g.size();
    }
    public void addNeiboursToWork(int work,int neibour){
        g.get(work).addNeibour(neibour);
    }
    public work getWork(int work){
        return g.get(work);
    }
    public int getSize(){
        return g.size();
    }
    private void markBlue(){
        for (int i=0;i<g.size();i++){
            if (g.get(i).getColorDFS()==0) {
                DFScycle(i);
            }
        }
        visited=new boolean[n];
        for (int i=0;i<g.size();i++){
            if (!visited[i]) {
                DFSblue(i);
            }
        }
    }
    private int max=0;
    private ArrayList<Integer> vmax=new ArrayList<>();
    private ArrayList<way> ways;
    private void markRed(){
        if (g.size()==1 && !g.get(0).getColor().equals("blue")){
            g.get(0).setColor("red");
        }
        ArrayList<Integer> roots=findRoots();
        Map<Integer,Integer> m=new HashMap<>();
        ways=new ArrayList<>();
        int v;
        int[] d;
        int max=0;
        int maxnum;
        //System.out.println(roots);
        //System.out.println(n);
        for (int i=0;i<roots.size();i++){
            v=roots.get(i);
            p=new int[n];
            if (g.get(v).getColor().equals("blue")){
                continue;
            }
            d=dijkstra(v);
            max=d[0];
            maxnum=0;
            for(int k=1;k<d.length;k++){
                if (d[k]>max){
                    max=d[k];
                    maxnum=k;
                }
            }
            if (maxnum!=-1) {
                ways.add(new way(v, maxnum, max, p));
                //System.out.println(max + "  number: " + maxnum + "  " + g.get(maxnum).getName());
            }
        }
        int[] p2=p;
        p=new int[n];
        if (roots.size()==1 && !g.get(0).getColor().equals("blue")){
            d=dijkstra(roots.get(0));
            maxnum=-1;
            v=0;
            for(int k=1;k<d.length;k++){
                if (d[k]>=max){
                    max=d[k];
                    maxnum=k;
                }
            }
            if (maxnum!=-1) {
                ways.add(new way(v, maxnum, max, p));
                //System.out.println(max + "  number: " + maxnum + "  " + g.get(maxnum).getName());
            }
        }
        ways.sort(new Comparator<way>() {
            @Override
            public int compare(way o1, way o2) {
                return o1.ves- o2.ves;
            }
        });
        //ways.forEach(s->{
            //Arrays.stream(s.parrents).forEach(c->System.out.print(c+" "));
            //System.out.println();
        //});
        if (ways.isEmpty()){
            return;
        }
        max=ways.get(ways.size()-1).ves;
        int s,f;
        int[] par;
        if (ways.size()==1){
            int i=0;
            s=ways.get(i).from;
            v=ways.get(i).to;
            par=ways.get(i).parrents;
            while(v!=s){
                g.get(v).setColor("red");
                f=v;
                v=par[v];
                g.get(f).addWayFrom(v);
            }
            g.get(v).setColor("red");
        }
        for (int i=ways.size()-1;i>-1;i--){
            if (ways.get(i).ves<max){
                break;
            }
            s=ways.get(i).from;
            v=ways.get(i).to;
            par=ways.get(i).parrents;
            while(v!=s){
                g.get(v).setColor("red");
                f=v;
                v=par[v];
                //System.out.println(v);
                g.get(f).addWayFrom(v);
            }
            g.get(v).setColor("red");
        }
    }
    private class way{
        int from;
        int to;
        int ves;
        int[] parrents;
        public way(int f,int t, int v,int[] p){
            from=f;
            to=t;
            ves=v;
            parrents=p;
        }
    }
    private class Pair implements Comparable<Pair>{
        private int first;
        private int second;
        public Pair(int first, int second) {
            this.first = first;
            this.second = second;
        }
        public int getFirst() {
            return first;
        }
        public int getSecond() {
            return second;
        }
        @Override
        public boolean equals(Object obj) {
            return false;
        }

        @Override
        public int compareTo(Pair o) {
            return o.getFirst()-first;
        }
    }
    private void DFScycle(int num){
        //0-white 1-gray 2-black
        g.get(num).setColorDFS(1);
        work v=g.get(num);
        //System.out.println(num+"  "+v.getName()+"  "+v.getColorDFS());
        int u;
        ArrayList<Integer> neibours=v.getNeibours();
        for (int i=0;i<neibours.size();i++){
            u=neibours.get(i);
            if (g.get(u).getColorDFS()==0){
                DFScycle(u);
            }
            else if (g.get(u).getColorDFS()==1){
                g.get(u).setColor("blue");
            }
        }
        g.get(num).setColorDFS(2);
    }
    private void DFSblue(int num){
        visited[num]=true;
        work v=g.get(num);
        int u;
        ArrayList<Integer> neibours=v.getNeibours();
        for (int i=0;i<neibours.size();i++){
            u=neibours.get(i);
            if (v.getColor().equals("blue")){
                g.get(u).setColor("blue");
                g.get(u).addWayFrom(num);
            }
            if (!visited[u]){
                DFSblue(u);
            }
        }
    }
    private ArrayList<Integer> findRoots(){
        sorted=new ArrayList<>();
        for (int i=0;i<g.size();i++){
            if (g.get(i).getTime()==-1){
                DFS(i);
            }
        }
        ArrayList<Integer> roots=new ArrayList<>();
        g.forEach(s->s.setTime(-1));
        int v;
        for (int i=sorted.size()-1;i>0;i--){
            v=sorted.get(i);
            if (g.get(v).getTime()==-1){
                DFS1(v);
                roots.add(v);
            }
        }
        //System.out.println(roots);
        return roots;
    }
    private void DFS(int num){
        g.get(num).setTime(time);
        time++;
        int u;
        ArrayList<Integer> neib=g.get(num).getNeibours();
        for (Integer integer : neib) {
            u = integer;
            if (g.get(u).getTime() == -1) {
                DFS(u);
            }
        }
        sorted.add(num);
    }
    private void DFS1(int num){
        g.get(num).setTime(time);
        time++;
        int u;
        ArrayList<Integer> neib=g.get(num).getNeibours();
        for (int i=0;i<neib.size();i++){
            u=neib.get(i);
            if (g.get(u).getTime()==-1){
                DFS1(u);
            }
        }
    }
    private int[] dijkstra(int s){
        int[] d=new int[n];
        Arrays.fill(d,inf);
        d[s]=g.get(s).getDuration();
        PriorityQueue<Pair> q=new PriorityQueue<>();
        q.add(new Pair(d[s],s));
        while(!q.isEmpty()){
            Pair p=q.poll();
            int cur, v;
            cur=p.getFirst();
            v=p.getSecond();
            if (cur<d[v] || g.get(v).getColor().equals("blue")){
                continue;
            }
            ArrayList<Integer> nn=g.get(v).getNeibours();
            int u, ves;

            for (int i=0;i<nn.size();i++){
                u=nn.get(i);
                ves=g.get(u).getDuration();
                if (g.get(u).getColor().equals("blue")){
                    continue;
                }
                boolean f=false;
                if (ways.size()>0){
                    for (int c=0;c<ways.get(0).parrents.length-1;c++){
                        if (u==ways.get(0).parrents[c]){
                            f=true;
                            break;
                        }
                    }
                }
                if (f && n<900){
                    continue;
                }

                if (d[u]<d[v]+ves){
                    d[u]=d[v]+ves;
                    this.p[u]=v;
                    q.add(new Pair(d[u],u));
                    /*
                    if (g.get(u).getName().equals("E283")){
                        System.out.println(d[u]+"  "+d[v]+"  "+ves);
                        //System.exit(0);
                        //976
                        //153  134  19
                    }

                     */
               }
            }
        }
        return d;
    }
    public void findCriticalWays(){
        if (g.size()==2 && g.get(0).getNeibours().isEmpty() && g.get(1).getNeibours().isEmpty()){
            if (g.get(0).getDuration()>g.get(1).getDuration()){
                g.get(0).setColor("red");
            }
            else if(g.get(0).getDuration()==g.get(1).getDuration()){
                g.get(1).setColor("red");
                g.get(0).setColor("red");
            }
            else{
                g.get(1).setColor("red");
            }
            return;
        }
        markBlue();
        checkOthersblue();
        checkOthersblue2();
        markRed();
        checkOthersred();
    }
    private void checkOthersred(){
        String color="red";
        for (int i=0;i<g.size();i++){
            if (g.get(i).getColor().equals(color)){
                for (int k=0;k<g.get(i).getNeibours().size();k++){
                    if (!g.get(g.get(i).getNeibours().get(k)).getColor().equals(color)){
                        int ves=g.get(g.get(i).getNeibours().get(k)).getDuration();
                        int num=g.get(i).getNeibours().get(k);


                        for (int c=0;c<g.get(i).getNeibours().size();c++){
                            if (c==k){
                                continue;
                            }
                            if (g.get(g.get(i).getNeibours().get(c)).getColor().equals(color) && ves==g.get(g.get(i).getNeibours().get(c)).getDuration()){
                                for (int j=0;j<g.get(num).getNeibours().size();j++){
                                    if (g.get(g.get(num).getNeibours().get(j)).getColor().equals(color)){
                                        g.get(num).setColor(color);
                                        g.get(g.get(num).getNeibours().get(j)).addWayFrom(num);
                                        g.get(num).addWayFrom(i);
                                        //break;
                                    }
                                }
                                if (g.get(num).getNeibours().isEmpty()){
                                    g.get(num).setColor(color);
                                    g.get(num).addWayFrom(i);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    private void checkOthersblue(){
        String color="blue";
        for (int i=0;i<g.size();i++){
            if (g.get(i).getColor().equals(color)){
                for (int k=0;k<g.get(i).getNeibours().size();k++){
                    if (g.get(g.get(i).getNeibours().get(k)).getColor().isEmpty()){
                        g.get(g.get(i).getNeibours().get(k)).setColor("blue");
                        g.get(g.get(i).getNeibours().get(k)).addWayFrom(i);
                    }
                    else if(g.get(g.get(i).getNeibours().get(k)).getColor().equals(color)){
                        g.get(g.get(i).getNeibours().get(k)).addWayFrom(g.get(i).getNum());
                    }
                }
            }
        }
    }
    private void checkOthersblue2(){
        String color="blue";
        for (int i=0;i<g.size();i++){
            if (g.get(i).getColor().equals("blue")){
                DFSblue(i);
            }
        }
    }
    public void printDotGraph(){
        System.out.println("digraph {");
        for (int i=0;i<g.size();i++){
            if (!g.get(i).getColor().isEmpty()) {
                System.out.println("\t"+g.get(i).getName() + " [label = \"" + g.get(i).getName() + "(" + g.get(i).getDuration() + ")\", color = " + g.get(i).getColor() + "]");
            }else{
                System.out.println("\t"+g.get(i).getName() + " [label = \"" + g.get(i).getName() + "(" + g.get(i).getDuration() + ")\"]");
            }
            //System.out.println(g.get(i).getWayFrom());
        }
        for (int i=0;i<g.size();i++){
            work v=g.get(i);
            for (int k=0;k<v.getNeibours().size();k++){
                int num=v.getNeibours().get(k);
                System.out.print("\t"+v.getName()+"->"+g.get(num).getName());
                if (!g.get(num).getColor().isEmpty() && v.getColor().equals(g.get(num).getColor()) && g.get(num).getWayFrom(i)==i){
                    System.out.print(" [color = " + v.getColor() + "]");
                }
                System.out.println();
            }
        }
        System.out.println("}");
    }
    public void convertGraphToPicture(){
        String file="Graph.gv";
        String text="digraph {\n";
        for (int i=0;i<g.size();i++){
            if (!g.get(i).getColor().isEmpty()) {
                text+="\t"+g.get(i).getName() + " [label = \"" + g.get(i).getName() + "(" + g.get(i).getDuration() + ")\", color = " + g.get(i).getColor() + "]\n";
            }else{
                text+="\t"+g.get(i).getName() + " [label = \"" + g.get(i).getName() + "(" + g.get(i).getDuration() + ")\"]\n";
            }
        }
        for (int i=0;i<g.size();i++){
            work v=g.get(i);
            for (int k=0;k<v.getNeibours().size();k++){
                int num=v.getNeibours().get(k);
                text+="\t"+v.getName()+"->"+g.get(num).getName();
                if (!g.get(num).getColor().isEmpty() && v.getColor().equals(g.get(num).getColor()) && g.get(num).getWayFrom(i)==i){
                    text+="\t"+"[color = "+ v.getColor()+"]";
                }
                text+="\n";
            }
        }
        text+="}";

        try(FileWriter writer = new FileWriter(file, false)) {
            writer.write(text);
            System.out.println("Successful write to file: "+file);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        //конвертация через dot в терминале
        Runtime rt=Runtime.getRuntime();
        try{
            Process proc=rt.exec("dot -Tpng "+file+" -o"+file.substring(0,file.indexOf('.'))+".png");
            System.out.println("Succcesful converting Graph to Picture");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
class work{
    private int num;
    private int time;
    private Map<Integer,Integer> wayFrom;
    private int duration;
    private String name;
    private String color;
    private int colorDFS;
    private ArrayList<Integer> neibours;
    public work(int num, int duration,String name){
        this.num=num;
        this.duration=duration;
        this.name=name;
        neibours=new ArrayList<>();
        color="";
        colorDFS=0;
        wayFrom=new HashMap<>();
        time=-1;
    }
    public void addNeibour(int num){
        neibours.add(num);
    }
    public String getName(){
        return name;
    }
    public int getDuration(){
        return duration;
    }
    public String getColor(){
        return color;
    }
    public void setColor(String color){
        this.color=color;
    }
    public int getNum(){
        return num;
    }
    public ArrayList<Integer> getNeibours(){
        return neibours;
    }
    public void setTime(int time) {
        this.time = time;
    }
    public int getTime() {
        return time;
    }
    public void setColorDFS(int colorDFS) {
        this.colorDFS = colorDFS;
    }
    public int getColorDFS() {
        return colorDFS;
    }
    public void addWayFrom(int wayFrom) {
        this.wayFrom.put(wayFrom,wayFrom);
    }
    public int getWayFrom(int num) {
        if (wayFrom.get(num)==null){
            return -1;
        }
        return wayFrom.get(num);
    }
}
public class Cpm {
    public static void main(String[] args){
        Scanner in=new Scanner(System.in);
        String text="";
        String s;
        while(in.hasNextLine()){
            s=in.nextLine();
            if(s.isEmpty()){
                break;
            }
            text+=s;
        }
        works g=new works();
        text=text.replaceAll(" ","");
        String[] t=text.split(";");
        String[] ver;
        Map<String,Integer> m=new HashMap<>();
        int count=0;
        for (int i=0;i<t.length;i++) {
            ver=t[i].split("<");
            for (int k=0;k<ver.length;k++){
                text=ver[k];
                if (text.charAt(text.length()-1)==')') {
                    int duration = Integer.parseInt(text.substring(text.indexOf('(') + 1, text.length() - 1));
                    String name=text.substring(0, text.indexOf('('));
                    m.put(name, count);
                    g.addNewWork(new work(count, duration, name));
                    count++;
                }
            }
            for (int k=0;k<ver.length;k++){
                text=ver[k];
                String name;
                if (text.charAt(text.length()-1)==')') {
                    name=text.substring(0, text.indexOf('('));
                }else{
                    name=text;
                }
                int v=m.get(name);
                String name1;
                if (k<ver.length-1){
                    text=ver[k+1];
                    if (text.charAt(text.length()-1)==')') {
                        name1=text.substring(0, text.indexOf('('));
                    }else{
                        name1=text;
                    }
                    int u = m.get(name1);
                    g.addNeiboursToWork(v,u);
                }
            }

        }

        g.findCriticalWays();
        g.printDotGraph();
        g.convertGraphToPicture(); //просто удобная картинка (+, если реально так удобнее)

        //вррде даже работает (нет)
        //как-то устал
    }
}
