package com.evotek.storage.application.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.List;

@Data
@Component
@ConfigurationProperties(prefix = "app.storage")
public class StorageProperties {

    private String folderUpload;

    private List<String> whitelistMimeTypes;

    private String publicStorageUrl;

}
