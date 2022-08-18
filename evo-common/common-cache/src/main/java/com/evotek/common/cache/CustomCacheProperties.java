package com.evotek.common.cache;

import lombok.Data;
import org.springframework.boot.autoconfigure.cache.CacheProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.Map;

@Data
@ConfigurationProperties(prefix = "spring.cache")
public class CustomCacheProperties {

    private Map<String, CacheProperties.Redis> customCache;
}