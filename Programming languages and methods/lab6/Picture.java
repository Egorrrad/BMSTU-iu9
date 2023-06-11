import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.*;

public class Picture extends JFrame {
    private JSpinner spinner1;
    private JSpinner spinner2;
    private JSpinner spinner3;
    private JButton redButton;
    private JButton greenButton;
    private JButton blueButton;
    private JTextField TextField;
    private JPanel mainPanel;
    private JButton redButton1;
    private JButton redButton2;
    private JButton blueButton1;
    private JButton greenButton1;
    private JButton greenButton2;
    private JButton blueButton2;
    public Picture(String name){
        super(name);
        setBounds(0,0,500,1000);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(null);
        spinner1.setValue(100);
        spinner2.setValue(100);
        spinner3.setValue(100);
        MyComponent krug1=new MyComponent(50,50,100,100,"r");
        MyComponent krug2=new MyComponent(150,50,100,100,"e");
        MyComponent krug3=new MyComponent(100,150,100,100,"d");
        mainPanel.setBounds(0,0,500,400);
        add(mainPanel,0);
        krug1.setBounds(0,401,500,600);
        krug2.setBounds(0,401,500,600);
        krug3.setBounds(0,401,500,600);
        add(krug1);
        add(krug2,2);
        add(krug3,3);
        setVisible(true);
        redButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                TextField.setText("вы нажали красный для круга 1");
                krug1.changeColour("red");
            }
        });
        greenButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                TextField.setText("вы нажали зеленый для круга 1");
                krug1.changeColour("green");
            }
        });
        blueButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                TextField.setText("вы нажали синий для круга 1");
                krug1.changeColour("blue");
            }
        });
        redButton1.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                TextField.setText("вы нажали красный для круга 2");
                krug2.changeColour("red");
            }
        });

        greenButton1.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                TextField.setText("вы нажали зеленый для круга 1");
                krug2.changeColour("green");
            }
        });
        blueButton1.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                TextField.setText("вы нажали синий для круга 2");
                krug2.changeColour("blue");
            }
        });
        redButton2.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                TextField.setText("вы нажали красный для круга 3");
                krug3.changeColour("red");
            }
        });
        greenButton2.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                TextField.setText("вы нажали зеленый для круга 3");
                krug3.changeColour("green");
            }
        });
        blueButton2.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                TextField.setText("вы нажали синий для круга 3");
                krug3.changeColour("blue");
            }
        });
        spinner1.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                int rp= krug1.getRadius();
                int r1=(int)spinner1.getValue();
                TextField.setText("вы изменили радиус для круга 1 на "+r1);
                krug1.changeRadius(r1,r1);
                int r2= krug2.getRadius();
                int r3= krug3.getRadius();
                krug1.changeCoordinats(0,0);
                krug2.changeCoordinats(r1-5,0);
                krug3.changeCoordinats(3*r1/4-5,r1-r2/4-5);
            }
        });
        spinner2.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                int rp=krug2.getRadius();
                int r=(int)spinner2.getValue();
                TextField.setText("вы изменили радиус для круга 2 на "+r);
                krug2.changeRadius(r,r);
                int r1= krug2.getRadius();
                int r2= krug2.getRadius();
                int r3= krug3.getRadius();
                krug1.changeCoordinats(0,0);
                krug2.changeCoordinats(r1-5,0);
                krug3.changeCoordinats(3*r1/4-5,r1-r2/4-5);
            }
        });
        spinner3.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                int rp=krug3.getRadius();
                int r=(int)spinner3.getValue();
                TextField.setText("вы изменили радиус для круга 3 на "+r);
                krug3.changeRadius(r,r);
                int r1= krug2.getRadius();
                int r2= krug2.getRadius();
                int r3= krug3.getRadius();
                krug1.changeCoordinats(0,0);
                krug2.changeCoordinats(r1-5,0);
                krug3.changeCoordinats(3*r1/4-5,r1-r2/4-5);
            }
        });

    }
}
