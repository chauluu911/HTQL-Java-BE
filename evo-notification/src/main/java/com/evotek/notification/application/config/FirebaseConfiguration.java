package com.evotek.notification.application.config;

import com.google.auth.oauth2.GoogleCredentials;
import com.google.firebase.FirebaseApp;
import com.google.firebase.FirebaseOptions;
import com.google.firebase.messaging.FirebaseMessaging;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;

import java.io.IOException;

@Configuration
@Slf4j
public class FirebaseConfiguration {

    @Value("${firebase.fcm-account-service-path}")
    private String amcFcmAccountServicePath;

    @Bean
    FirebaseMessaging firebaseMessaging() throws IOException {
        GoogleCredentials googleCredentials = GoogleCredentials
                .fromStream(new ClassPathResource(amcFcmAccountServicePath).getInputStream());
        FirebaseOptions firebaseOptions = FirebaseOptions
                .builder()
                .setCredentials(googleCredentials)
                .build();

        // init firebase if not existed
        FirebaseApp firebaseApp;
        if (FirebaseApp.getApps().isEmpty()) {
            log.info("Init Firebase Application");
            firebaseApp = FirebaseApp.initializeApp(firebaseOptions);
        } else {
            firebaseApp = FirebaseApp.getInstance();
        }

        return FirebaseMessaging.getInstance(firebaseApp);
    }
}
