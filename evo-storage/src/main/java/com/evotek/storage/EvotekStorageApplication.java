package com.evotek.storage;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.scheduling.annotation.EnableAsync;

@SpringBootApplication
@EnableAsync
@EnableDiscoveryClient
public class EvotekStorageApplication {

    public static void main(String[] args) {
        SpringApplication.run(EvotekStorageApplication.class, args);
    }

}
