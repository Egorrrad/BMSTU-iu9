
public class Parser {
    private char[] arr;
    private int pos, len,in;
    private String[] res;
    public Parser(String s){
        this.arr=s.toCharArray();
        this.pos=0;
        this.len=arr.length;
        this.res=new String[len];
        in=0;
    }
    public String parse(){
        Eq();
        if (pos!=len){
            System.out.println("syntax error at ("+pos+")");
        }
        String s="";
        for(int i=0;i<in;i++){
            s+=res[i]+" ";
        }
        return s;
    }
    private boolean Eq(){
        if (pos<len){
            res[in]="Eq";
            in++;
        if (arr[pos] <= 'z' && arr[pos] >= 'a') {
            pos++;
            while (pos < len) {
                if (arr[pos] == ':' || arr[pos] == ' ' || arr[pos]==')'){
                    break;
                }
                if (!((arr[pos] <= 'z' && arr[pos] >= 'a') || (arr[pos] <= '9' && arr[pos] >= '1'))) {
                    return false;
                }

            }
            res[in]="IDENT";
            in++;
            while(arr[pos]==' '){
                res[in]="ε";
                in++;
                pos++;
            }

            if (arr[pos] == ':') {
                pos++;
                //System.out.println(arr[pos]);
                return List();
            }
            } else{
                return false;
            }
        }
        return true;
    }

    private boolean List(){
        res[in]="List";
        in++;
        while(arr[pos]==' '){
            res[in]="ε";
            in++;
            pos++;
        }
        return Term();
    }
    private boolean Tail(){
        res[in]="Tail";
        in++;
        while(arr[pos]==' '){
            pos++;
        }
        return Term();
    }
    private boolean Term(){
        res[in]="Term";
        in++;
        if (arr[pos]=='\"'){
            res[in]="STRING";
            in++;
            pos++;
            while(true){
                if(pos==len) {
                    return false;
                }
                if(arr[pos]=='\n'){
                    return false;
                }
                if(arr[pos]=='\"'){
                    pos++;
                    break;
                }
                pos++;
            }
        }else{
            if(arr[pos]=='('){
                pos++;
                return Eq();
            }
        }
        if (arr[pos]==')'){
            pos++;
            if (pos==len){
                return true;
            }
        }
        while(arr[pos]==' '){
            res[in]="ε";
            in++;
            pos++;
        }
        if (arr[pos]=='+'){
            pos++;
            return Tail();
        }
        return true;
    }
}
