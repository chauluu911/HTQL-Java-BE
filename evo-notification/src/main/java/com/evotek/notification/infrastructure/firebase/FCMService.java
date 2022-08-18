package com.evotek.notification.infrastructure.firebase;

import com.google.firebase.messaging.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.Duration;
import java.util.concurrent.ExecutionException;

@Service
@Slf4j
public class FCMService {

    private final FirebaseMessaging firebaseMessaging;

    public FCMService(FirebaseMessaging firebaseMessaging) {
        this.firebaseMessaging = firebaseMessaging;
    }

    public void sendToDevice(FirebaseData firebaseData, String tokenDevice) {
        String response;
        try {
            Notification notification = Notification.builder()
                    .setTitle(firebaseData.getTitle())
                    .setBody(firebaseData.getBody())
                    .build();

            Message message = Message.builder()
                    .setToken(tokenDevice)
                    .setNotification(notification)
                    .putAllData(firebaseData.getData())
                    .build();
            getMessageBuilder(firebaseData.getTitle(), firebaseData.getBody());
            response = sendMessage(message);
            log.info("Message sent successfully with messageId --->" + response.substring(response.lastIndexOf("/") + 1));
        } catch (Exception e) {
            log.error("Fail to send firebase notification", e);
            Thread.currentThread().interrupt();
        }
    }

    /**
     * send a message
     *
     * @param message
     * @return
     * @throws InterruptedException
     * @throws ExecutionException
     */
    private String sendMessage(Message message) throws InterruptedException, ExecutionException {
        return firebaseMessaging.sendAsync(message).get();
    }

    /**
     * get Android Config
     *
     * @return
     */
    private AndroidConfig getAndroidConfig() {
        return AndroidConfig.builder()
                .setTtl(Duration.ofMinutes(2).toMillis())
                .setPriority(AndroidConfig.Priority.HIGH)
                .setNotification(AndroidNotification.builder().build())
                .build();
    }

    /**
     * get IOS config
     *
     * @param title
     * @param body
     * @return
     */
    private ApnsConfig getApnsConfig(String title, String body) {
        ApsAlert alert = ApsAlert.builder()
                .setTitle(title)
                .setBody(body)
                .build();

        Aps aps = Aps.builder()
                .setAlert(alert)
                .build();

        return ApnsConfig.builder()
                .setAps(aps)
                .build();
    }

    /**
     * get Builder of Message with android and ios config
     *
     * @param title
     * @param body
     * @return
     */
    private Message.Builder getMessageBuilder(String title, String body) {
        AndroidConfig androidConfig = getAndroidConfig();
        ApnsConfig apnsConfig = getApnsConfig(title, body);
        return Message.builder()
                .setApnsConfig(apnsConfig)
                .setAndroidConfig(androidConfig);
    }

}
