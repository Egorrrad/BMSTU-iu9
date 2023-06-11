import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;

public class PictureForm {

    private JPanel mainPanel;
    private JTextField Field;
    private JSpinner radSpiner;
    private JPanel canvasPanel;

    public PictureForm() {
        radSpiner.setValue(1);
        radSpiner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                Field.setText(""+radSpiner.getValue());
                canvasPanel.setBounds(2,2,3,3);
            }
        });
    }
    public class CanvasPanel extends JPanel{
        private int radius=5;
        public void setRadius(int r){
            radius=r;
            repaint();
        }
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            g.drawOval(10,10,radius,radius);
        }
    }


    public static void main(String[] args) {
        JFrame frame = new JFrame("3 окружности");
        frame.setContentPane(new PictureForm().mainPanel);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
        //frame.add(new MyComponent());
        //frame.add(new Circle(2,2));
    }
}


