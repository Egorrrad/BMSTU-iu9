package org.example;

import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

public class PublisherMatrix {
    private static String broker="tcp://broker.emqx.io:1883";
    private static String topic="IU/9/Egor";
    private static String content;

    public static void main(String[] args) {
        int qos = 2;
        String clientId = "Eg";
        MemoryPersistence persistence = new MemoryPersistence();

        try {
            MqttClient client = new MqttClient(broker, clientId, persistence);


            MqttConnectOptions connOpts = new MqttConnectOptions();

            connOpts.setCleanSession(true);

            System.out.println("Connecting to broker: " + broker);
            client.connect(connOpts);

            System.out.println("Connected");

            MqttMessage message;
            matrix3na3 matr;
            for (int i=0;i<5;i++) {
                matr=new matrix3na3();
                content=matr+"Simetric: "+matr.simetric()+"\n";
                System.out.println("Publishing message: " + content);
                message = new MqttMessage(content.getBytes());
                message.setQos(qos);
                client.publish(topic, message);
                System.out.println("Message published");
                Thread.sleep(10000);
            }
            client.disconnect();
            System.out.println("Disconnected");
            client.close();
            System.exit(0);
        } catch (MqttException me) {
            System.out.println("reason " + me.getReasonCode());
            System.out.println("msg " + me.getMessage());
            System.out.println("loc " + me.getLocalizedMessage());
            System.out.println("cause " + me.getCause());
            System.out.println("excep " + me);
            me.printStackTrace();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
