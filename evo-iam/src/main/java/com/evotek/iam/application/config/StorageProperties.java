package com.evotek.iam.application.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.Map;

@Data
@Component
@ConfigurationProperties(prefix = "storage")
public class StorageProperties {
    private String folderUpload;

    private String template;

    private int maxFilePerFolder;

    private Map<String, String[]> extensions;
}
