package org.example;

import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;

public class Main {
    public static void main(String[] args) throws MqttException {
        //MqttClient c=new
        MqttClient client = new MqttClient("tcp://broker.emqx.io:1883", MqttClient.generateClientId());
        client.connect();
        MqttMessage message = new MqttMessage();
        message.setPayload("Hello world from Java".getBytes());

        client.publish("iot_data", message);
        client.subscribe("iot_data");

        client.disconnect();
    }
}


