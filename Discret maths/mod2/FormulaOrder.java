import java.util.*;

class graph{
    vertex[] g;
    int n;
    boolean cycle;
    int countExpr;
    public graph(int n, ArrayList<formula> list, Map<String,Integer> hash, boolean cycle, int countEx){
        this.n=n;
        this.cycle=cycle;
        this.countExpr=countEx;
        g=new vertex[n];
        for (int i=0;i<n;i++){
            g[i]=new vertex(i," ",0,false);
        }
        for(int i=0;i<list.size();i++){
            String per=list.get(i).perem;
            int v=hash.get(per);
            g[v].perem=per;
            g[v].exprection=list.get(i).expret;
            if (g[v].exprection.contains(",")){
                g[v].pair=true;
            }
            ArrayList expr=list.get(i).vars;
            for (int k=0;k<expr.size();k++){
                String str= (String) expr.get(k);
                str=str.trim();
                if (hash.get(str)!=null) {
                    int u = hash.get(str);
                    g[v].addNeib(u);
                }
            }
        }
        answer=new ArrayList<>();
        /*
        for (int i=0;i<n;i++){
            System.out.println(g[i].num+" "+g[i].perem+" "+g[i].neib);
        }

         */
    }
    ArrayList<String> answer;
    private boolean DFS(vertex[] g, int now, boolean f){
        g[now].color=1; //gray
        for(int i=0; i<g[now].neib.size();i++){
            int v=g[now].neib.get(i);
            if (g[v].color==0){
                f=DFS(g,v,f);
            } else if (g[v].color==1) {
                f=false;
                return f;
            }
        }
        g[now].color=2; //black
        answer.add(g[now].exprection);
        return f;
    }
    public boolean sort(){
        boolean flag=true;
        if (cycle){
            return false;
        }
        int maxneib=0;
        int numMax=0;
        for (int i=0;i<n;i++){
            String newstr=g[i].exprection.split("=")[1];
            newstr=newstr.trim();
            try {
                Integer.parseInt(newstr);
                answer.add(g[i].exprection);
                g[i].color=2;
            }
            catch (NumberFormatException e){
                continue;
            }

            if (g[i].neib.size()>maxneib){
                    maxneib = g[i].neib.size();
                    numMax = i;
            }
        }
        if (!g[numMax].pair) {
            flag = DFS(g, numMax, true);
            if (!flag) {
                return flag;
            }
        }
        for (int i=0;i<n;i++){
            if (g[i].color==0 && !g[i].pair){
                flag=DFS(g,i,true);
                if(!flag){
                    return flag;
                }
            }
        }
        if (answer.size()<countExpr){
            for (int j=0;j<n;j++){
                boolean p=false;
                for (int k=0;k<answer.size();k++){
                    if (g[j].exprection.equals(answer.get(k))){
                        p=true;
                    }
                }
                if (!p){
                    answer.add(g[j].exprection);
                }
            }
        }
        return flag;
    }

}
class vertex{
    int num;
    String perem;
    String exprection;
    int color;
    boolean pair;
    ArrayList<Integer> neib;
    public vertex(int number, String per, int col, boolean p){
        num=number;
        perem=per;
        color=col;
        neib=new ArrayList<>();
        pair=p;
    }
    public void addNeib(int num){
        neib.add(num);
    }
    public void setPerem(String p){
        perem=p;
    }
}
class formula{
    String perem;
    ArrayList<String> vars;
    String expret;
    public formula(String per, ArrayList<String> expr, String ex){
        perem=per;
        vars=expr;
        expret=ex;
    }
}
class FormulaOrder {
    static ArrayList<formula> perems=new ArrayList<>();
    static Map<String,Integer> variables=new HashMap();
    private static boolean znak(char z){
        return (z=='+' || z=='-' || z=='*' || z=='/');
    }
    static int c=0;
    static boolean cycle=false;
    private static boolean parseText(String text1){
        String text=text1.trim();
        if (text.length()==1){
            return false;
        }
        String t=text.split("=")[0].trim();
        if (t.length()==0){
            return false;
        }
        if (t.charAt(t.length()-1)==','){
            return false;
        }
        if (znak(text.charAt(text.length()-1))){
            return false;
        }
        int i=0;
        int countPerem=0;
        int countEquals=0;
        while(i<text.length()){
            int k=0;
            String perem="";
            if (text.charAt(i)=='='){
                countEquals++;
                break;
            }
            else if(text.charAt(i)==' '){
                i++;
                continue;
            }
            while(i<text.length()){
                if (text.charAt(i)==','){
                    i++;
                    break;
                }
                else if (text.charAt(i)=='='){
                    countEquals++;
                    break;
                }
                else if (text.charAt(i)==' '){
                    i++;
                    continue;
                }
                else if (k==0 && !Character.isLetter(text.charAt(i))){
                    return false;
                }
                else if (text.charAt(i)=='_'){
                    return false;
                }
                else if(znak(text.charAt(i))){
                    return false;
                }
                else if (text.charAt(i)!=' ') {
                    perem += text.charAt(i);
                }
                k++;
                i++;
            }
            perems.add(new formula(perem,new ArrayList<>(),text1));
            if (variables.get(perem)!=null){
                return false;
            }
            variables.put(perem,c);
            c++;
            countPerem++;
        }
        i++;
        if (countEquals==0 || countEquals>2){
            return false;
        }
        int count=perems.size()-countPerem;
        int count1=count;
        if (text.charAt(text.length()-1)==','){
            return false;
        }
        while(i<text.length()){
            ArrayList<String> vars=new ArrayList<>();
            boolean skobka=false;
            int pred=-1; //0-число 1-переменная 2-закрытая скобка 3-знак 4-открытая скобка
            while (i<text.length() && text.charAt(i)!=',' ){
                if(text.charAt(i)==' '){
                    i++;
                    continue;
                }
                else if (text.charAt(i)=='('){
                    skobka=true;
                    pred=4;
                }
                else if (text.charAt(i)==')'){
                    skobka=false;
                    if (pred==4){
                        return false;
                    }
                    pred=2;
                }
                else if (text.charAt(i)=='='){
                    return false;
                }
                else if (Character.isDigit(text.charAt(i))){
                    if (pred==0 || pred==2) {
                        return false;
                    }
                    String number="";
                    while(i<text.length()){
                        if(!Character.isDigit(text.charAt(i)) || text.charAt(i)==' '){
                            i--;
                            break;
                        }
                        number+=text.charAt(i);
                        i++;
                    }
                    vars.add(number);
                    pred=0;
                }
                else if (Character.isLetter(text.charAt(i))){
                    if (pred==0 || pred==1 || pred==2){
                        //return false;
                    }
                    String v="";
                    while(i<text.length() && !znak(text.charAt(i)) && text.charAt(i)!='(' && text.charAt(i)!=')'&& text.charAt(i)!=' ' &&text.charAt(i)!=','){
                        if (text.charAt(i)=='='){
                            return false;
                        }
                        v+=text.charAt(i);
                        i++;
                    }
                    pred=1;
                    i--;
                    for (int k=count1;k<perems.size();k++){
                        if (v.equals(perems.get(k).perem)){
                            cycle=true;
                        }
                    }
                    vars.add(v);
                }
                else if(znak(text.charAt(i))){
                    if (pred==-1 && text.charAt(i)!='-'){
                        return false;
                    }
                    pred=3;
                }
                else{
                    return false;
                }
                i++;
            }
            countPerem--;
            if (skobka){
                return false;
            }
            if (perems.size()==0 || countPerem<0){
                return false;
            }
            formula f=perems.get(count);
            f.vars=vars;
            perems.set(count,f);
            count++;
            i++;
        }
        if (countPerem!=0){
            return false;
        }
        return true;
    }
    public static void test(){
        ArrayList<String> tests=new ArrayList<>();
        tests.add("V = S*h / 3");
        tests.add("a, b = 10, 15");
        tests.add("h = (a + b) / 2");
        tests.add("k = h*(i+1)");
        tests.add("x = 10");
        tests.add("a, b = x, y");
        tests.add("x = -5");
        tests.add("x = ------10");
        String text;
        for (int i=0;i<tests.size();i++) {
            text=tests.get(i);
            variables.clear();
            perems.clear();
            if (!parseText(text)) {
                System.out.println("ERROR AT: " + text);
                return;
            }
        }
        System.out.println("Good");
        tests.clear();
        tests.add("a = 10 % 3");
        tests.add("a, b, c, d = 1, 2, 3");
        tests.add("a, b, c = 1, 2, 3, 4");
        tests.add("x = 10 * (2 5)");
        tests.add("x = 10 +");
        tests.add("x = 5 * (4 - 2) 3");
        tests.add("x =");
        tests.add("a = b = c");
        tests.add("= 10");
        tests.add("x = ()");
        tests.add("x = (5");
        tests.add("x = * 5");
        tests.add("=");
        tests.add("x = 10,");
        tests.add("x + 2 = 3");
        tests.add("x, = 10");
        tests.add("alpha_beta = 20");
        for (int i=0;i<tests.size();i++) {
            text=tests.get(i);
            variables.clear();
            perems.clear();
            if (parseText(text)) {
                System.out.println("ERROR AT: " + text);
                return;
            }
        }
        System.out.println("Very good");
        tests.clear();
        tests.add("x, y = 10, x");
        tests.add("x = x");
        for (int i=0;i<tests.size();i++) {
            text=tests.get(i);
            variables.clear();
            perems.clear();
            cycle=false;
            parseText(text);
            graph g = new graph(c, perems, variables, cycle,c);
            if (g.sort()){
                System.out.println("ERROR AT: " + text);
                return;
            }
        }
        System.out.println("It is perfect!");
    }
    public static void main(String[] args){
        //test();

        Scanner in=new Scanner(System.in);
        String text;
        boolean flag=false;
        int countEx=0;
        while(in.hasNext()){
            text=in.nextLine();
            if (text.isEmpty()){
                break;
            }
            countEx++;
            if (!parseText(text)){
                flag=true;
                break;
            }
        }
        for (int i=0;i< perems.size();i++){
            ArrayList var=perems.get(i).vars;
            for (int k=0;k<var.size();k++){
                try {
                    Integer.parseInt((String)var.get(k));
                }
                catch (NumberFormatException e) {
                    if (variables.get(var.get(k)) == null) {
                        flag = true;
                        i = perems.size();
                        break;
                    }
                }
            }
        }
        if (flag){
            System.out.println("syntax error");
            return;
        }
        graph g=new graph(c,perems,variables, cycle,countEx);
        if (g.sort()){
            ArrayList<String> m=new ArrayList<>();
            for (int i=0;i<g.answer.size();i++){
                text=g.answer.get(i);
                flag=false;
                for (int k=0;k<m.size();k++){
                    if(m.get(k).equals(text)){
                        flag=true;
                        break;
                    }
                }
                if (!flag){
                    System.out.println(text);
                    m.add(text);
                }
            }
        }else{
            System.out.println("cycle");
        }
    }
}

//работает и норм
