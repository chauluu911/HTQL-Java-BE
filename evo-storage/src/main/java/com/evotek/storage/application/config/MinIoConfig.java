package com.evotek.storage.application.config;

import io.minio.MinioClient;
import okhttp3.OkHttpClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.concurrent.TimeUnit;

@Configuration
public class MinIoConfig {
    @Value("${minio.access-key}")
    String accessKey;
    @Value("${minio.secret-key}")
    String accessSecret;
    @Value("${minio.endpoint}")
    String minioUrl;

    @Bean
    public MinioClient generateMinioClient() {
        try {
            OkHttpClient httpClient = new OkHttpClient.Builder()
                    .connectTimeout(10, TimeUnit.MINUTES)
                    .writeTimeout(10, TimeUnit.MINUTES)
                    .readTimeout(30, TimeUnit.MINUTES)
                    .build();
            MinioClient client = MinioClient.builder()
                    .endpoint(minioUrl)
                    .httpClient(httpClient)
                    .credentials(accessKey, accessSecret).build();
            return client;
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage());
        }

    }

}
