package org.example;

import org.eclipse.paho.client.mqttv3.*;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

public class SubscriberMatrix implements MqttCallback {
    private static String broker="tcp://broker.emqx.io:1883";
    private static String topic="IU/9/Egor";
    private static String clientId="ppppp";
    public void connectionLost(Throwable cause) {
        System.out.println("disconnect，you can reconnect");
    }

    public void messageArrived(String topic, MqttMessage message) throws Exception {
        System.out.println("Received message topic:" + topic);
        System.out.println("Received message content:" + new String(message.getPayload()));
    }

    public void deliveryComplete(IMqttDeliveryToken token) {
        System.out.println("deliveryComplete---------" + token.isComplete());
    }
    public static void main(String[] args) {
        MemoryPersistence persistence = new MemoryPersistence();
        try {
            MqttClient client = new MqttClient(broker, clientId, persistence);

            MqttConnectOptions connOpts = new MqttConnectOptions();

            connOpts.setCleanSession(true);
            client.setCallback(new SubscriberMatrix());


            System.out.println("Connecting to broker: " + broker);
            client.connect(connOpts);
            System.out.println("Connected");
            client.subscribe(topic);


        } catch (MqttSecurityException e) {
            throw new RuntimeException(e);
        } catch (MqttException e) {
            throw new RuntimeException(e);
        }

    }
}
