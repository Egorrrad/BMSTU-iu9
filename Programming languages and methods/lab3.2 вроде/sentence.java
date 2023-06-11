public class sentence {
    private String s;
    private int count;
    public sentence(String s) {
        this.s = s;
        count=0;
        s=s+" ";
        for (int i=0; i<s.length()-1;i++){
            if (s.charAt(i)!=' ' && s.charAt(i)!=',' && (s.charAt(i+1)==',' || s.charAt(i+1)==' ')){
                count++;
            }

        }
    }
    public int getCount(){
        return count;
    }
    public String getSentence(){
        return s;
    }
    public int compareTo(sentence obj){
        return this.count-obj.count;
    }
    public sentence[] sortArr(sentence[] mas) {
        for (int i = 0; i < mas.length; i++) {
            for (int j = i+1; j < mas.length; j++) {
                if (mas[i].compareTo(mas[j]) > 0) {
                    sentence a = mas[i];
                    mas[i] = mas[j];
                    mas[j] = a;
                }
            }
        }
        return mas;
    }
}
