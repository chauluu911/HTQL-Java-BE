package com.evotek.notification;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.scheduling.annotation.EnableAsync;

@SpringBootApplication
@EnableAsync
@EnableCaching
@EnableDiscoveryClient
@EnableEurekaClient
public class EvotekNotificationApplication {

    public static void main(String[] args) {
        SpringApplication.run(EvotekNotificationApplication.class, args);
    }
}
