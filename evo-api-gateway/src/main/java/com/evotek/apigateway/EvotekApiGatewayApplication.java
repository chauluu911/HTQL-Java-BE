package com.evotek.apigateway;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.scheduling.annotation.EnableAsync;

@EnableAsync
@EnableCaching
@EnableDiscoveryClient
@EnableEurekaClient
@SpringBootApplication
public class EvotekApiGatewayApplication {

    public static void main(String[] args) {
        SpringApplication.run(EvotekApiGatewayApplication.class, args);
    }

}
