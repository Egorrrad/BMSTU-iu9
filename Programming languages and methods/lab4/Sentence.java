public class Sentence {
    private String s;
    private SentenceIterator it;
    public Sentence(String s){
        this.s=s+" ";
        it=new SentenceIterator();
    }
    public void changeS(String news){
        this.s=news+" ";
        it=new SentenceIterator();
    }
    public String getS(){
        return s;
    }
    public void Iterate(){
        while(it.hasNext()){
            System.out.println(it.next());
        }
    }
    private class SentenceIterator{
        private int pos;
        public SentenceIterator(){
            pos=0;
        }
        public boolean hasNext () {
            return pos < s.length();
        }
        public String next(){
            String res;
            res="";
            while(s.charAt(pos)!=' '){
                res+=s.charAt(pos);
                pos++;
            }
            pos++;
            return res;
        }
    }
}
