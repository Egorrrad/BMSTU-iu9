import java.util.*;

class state{
    int number;
    int number2;
    state parent;
    int depth;
}
class Automat{
    private int n, m, q0;
    private int[][] input;
    private String[][] output;
    private state[] states;
    private int[] canon;
    public Automat(int n, int m, int q0){
        this.n=n;
        this.m=m;
        this.q0=q0;
        this.input=new int[n][m];
        this.output=new String[n][m];
        states=new state[n];
        for (int i=0;i<n;i++){
            states[i]=new state();
            states[i].number=i;
        }

    }
    public void createAutomat(Scanner in){
        for (int i=0;i<n;i++){
            for(int k=0;k<m;k++){
                int x=in.nextInt();
                input[i][k]=x;
            }
        }
        for (int i=0;i<n;i++){
            for(int k=0;k<m;k++){
                String x=in.next();
                output[i][k]=x;
            }
        }
    }
    public void printAutomat(){
        System.out.printf("%d\n%d\n%d\n",n,m,q0);
        for (int i=0;i<input.length;i++){
            for(int k=0;k<m;k++){
                System.out.printf("%d ",input[i][k]);
            }
            System.out.printf("\n");
        }
        for (int i=0;i<output.length;i++){
            for(int k=0;k<m;k++){
                System.out.printf("%s ",output[i][k]);
            }
            System.out.printf("\n");
        }
    }
    public void printDotAutomat(){
        System.out.println("digraph {\n\trankdir = LR");
        for (int i = 0; i < n; i++){
            for (int k = 0; k < m; k++){
                int num='a';
                char p=(char)(num+k);
                String s = "\t" + i + " -> " + input[i][k] + " [label = \"" + p + "(" + output[i][k] + ")\"]";
                System.out.println(s);
            }
        }
        System.out.println("}");
    }
    public void canonAutomat(){
        canon=new int[n];
        for (int i=0;i<n;i++){
            canon[i]=-1;
        }
        count=0;
        DFS(q0);
        q0=0;
        n=count;
        int[][] newInput=new int[n][m];
        String[][] newOutput=new String[n][m];

        for (int i=0;i<canon.length;i++){
            int novi=canon[i];
            if (novi!=-1) {
                newOutput[novi]=output[i];
                for (int k = 0; k < m; k++) {
                    newInput[novi][k]=canon[input[i][k]];
                }
            }
        }
        input=newInput;
        output=newOutput;
    }
    public boolean equal(Automat b){
        if (n==b.n && m==b.m && q0==b.q0){
            for (int i=0;i<n;i++){
                if (!Arrays.equals(input[i], b.input[i])){
                    return false;
                }
                if (!Arrays.equals(output[i],b.output[i])){
                    return false;
                }
            }
            return true;
        }
        return false;
    }
    private int count;
    private void DFS(int num){
        canon[num]=count;
        count++;
        for (int i=0; i<m;i++){
            int v=input[num][i];
            if (canon[v]==-1){
                DFS(v);
            }
        }
    }
    private boolean prov(state stat, state[] states1){
        for (int i=0;i<states1.length;i++){
            if (stat==states1[i]){
                return true;
            }
        }
        return false;
    }
    private void Aufenkamp(){
        //Automat e=new Automat();
        ressplit res=split1();
        int m1= res.m1;
        state[] per= res.per;
        int m2;
        while(true) {
            res = split(per);
            m2 = res.m1;
            per = res.per;
            if (m1 == m2) {
                break;
            }
            m1 = m2;
        }
        per=norm(per);
        state[] newstates=new state[n];
        int[][] newinput=new int[n][m];
        String[][] newoutput=new String[n][m];
        int newn=m1;
        int count=0;
        for (int i=0;i<states.length;i++){
            state newstate=per[states[i].number];
            if (!prov(newstate,newstates)){
                newstates[i]=newstate;
                for (int j=0;j<m;j++){
                    newinput[i][j]=per[input[states[i].number][j]].number;
                    newoutput[i][j]=output[states[i].number][j];
                }
                //count++;
            }
        }
        input=newinput;
        output=newoutput;
        //n=newn;
        //q0=per[q0].number2;

    }
    private state find(state st){
        state e=st;
        while (e.parent!=e){
            e=e.parent;
        }
        return e;
    }
    private state[] union(state r1, state r2){
        r1=find(r1);
        r2=find(r2);
        if (r1.depth>r2.depth){
            r2.parent=r1;
        } else if (r1.depth<r2.depth) {
            r1.parent=r2;
        }else{
            r1.depth++;
            r2.parent=r1;
        }
        state[] res=new state[2];
        res[0]=r1;
        res[1]=r2;
        return res;
    }
    private class ressplit{
        int m1;
        state[] per;
        public ressplit(int m1,state[] per){
            this.m1=m1;
            this.per=per;
        }
    }
    private ressplit split1(){
        int m1=states.length;
        state[] per=new state[m1];
        for (int i=0;i<m1;i++){
            states[i].parent=states[i];
        }
        for (int i=0;i<n;i++){
            for (int j=0;j<n;j++){
                if (find(states[i])!=find(states[j])){
                    boolean eq=true;
                    if (!Arrays.equals(output[i], output[j])){
                        eq=false;
                    }
                    if (eq){
                        state[] res=union(states[i],states[j]);
                        states[i]=res[0];
                        states[j]=res[1];
                        m1--;
                    }
                }
            }
        }
        for (int i=0;i<n;i++){
            per[states[i].number]=find(states[i]);
        }
        return new ressplit(m1,per);
    }
    private ressplit split(state[] per){
        int m1=states.length;
        for (int i=0;i<m1;i++){
            states[i].depth=0;
            states[i].parent=states[i];
        }
        for (int i=0;i<n;i++){
            for (int j=0;j<n;j++){
                if ((find(states[i])!=find(states[j])) && (per[states[i].number]==per[states[j].number])){
                    boolean eq=true;
                    for (int k=0;k<m;k++) {
                        if (per[input[i][k]]!=per[input[j][k]]){
                            eq=false;
                            break;
                        }
                    }
                    if (eq){
                        state[] res=union(states[i],states[j]);
                        states[i]=res[0];
                        states[j]=res[1];
                        m1--;
                    }
                }
            }
        }
        for (int i=0;i<n;i++){
            per[states[i].number]=find(states[i]);
        }
        return new ressplit(m1,per);
    }
    private state[] norm(state[] per){
        int c=0;
        Map<Integer,Integer> mas=new HashMap<>();
        for (int i=0;i<per.length;i++){
            int num=states[i].number;
            if (mas.get(num)==null){
                mas.put(num,c);
                c++;
            }
            //System.out.println(i+" "+"leng: "+per.length);
            if (per[i]!=null) {
                per[i].number2 = mas.get(num);
            }
        }
        return per;
    }
    public void min(){
        Aufenkamp();
    }
}




class EqMealy{
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n= in.nextInt();
        int m=in.nextInt();
        int q0=in.nextInt();
        Automat a=new Automat(n,m,q0);
        a.createAutomat(in);

        n= in.nextInt();
        m=in.nextInt();
        q0=in.nextInt();
        Automat b=new Automat(n,m,q0);
        b.createAutomat(in);

        a.canonAutomat();
        try {
            a.min();
        }catch (ArrayIndexOutOfBoundsException e){
            System.out.println("NOT EQUAL");
            return;
        }
        a.canonAutomat();

        b.canonAutomat();
        try {
            b.min();
        }catch (ArrayIndexOutOfBoundsException e){
            System.out.println("NOT EQUAL");
            return;
        }
        b.canonAutomat();

        /*
        a.printAutomat();
        b.printAutomat();

         */

        if (a.equal(b)){
            System.out.println("EQUAL");
        }else{
            System.out.println("NOT EQUAL");
        }

    }
}


// в итоге без исключений прошло

/*
тест на котором не работает и выходит за массив без обработки этого исключения
7
3
2
5 5 2
5 2 0
3 0 1
3 0 1
5 2 0
0 2 0
6 0 1
a c o
i a q
q p y
f o r
i a q
i a r
f o r
7
3
1
3 6 2
3 4 2
4 3 3
2 0 2
0 2 1
3 6 2
5 2 1
h c e
e k t
o i e
e c h
e w h
h c e
e w h
 */
