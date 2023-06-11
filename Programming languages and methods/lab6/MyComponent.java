import javax.swing.*;
import java.awt.*;

public class MyComponent extends JComponent {
    private int x;
    private int y;
    private int w;
    private int h;
    private String colour;
    public MyComponent(int x, int y, int w, int h, String col){
        this.x=x;
        this.y=y;
        this.w=w;
        this.h=h;
        this.colour=col;
    }
    public void changeCoordinats(int x, int y){
        this.x=x;
        this.y=y;
        repaint();
    }
    public void changeRadius(int w, int h){
        this.w=w;
        this.h=h;
        repaint();
    }
    public void changeColour(String col){
        this.colour=col;
        repaint();
    }
    public int[] getCoordinats(){
        int[] mas={x,y};
        return mas;
    }
    public int getRadius(){
        return w;
    }
    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        if (colour=="red"){
            g.setColor(Color.RED);
        } else if (colour=="green"){
            g.setColor(Color.GREEN);
        } else if (colour=="blue") {
            g.setColor(Color.BLUE);
        }else g.setColor(Color.DARK_GRAY);
        g.fillOval(x,y,w,h);
    }
}