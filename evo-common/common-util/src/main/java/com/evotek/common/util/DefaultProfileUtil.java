package com.evotek.common.util;

import org.springframework.boot.SpringApplication;

import java.util.HashMap;
import java.util.Map;

public final class DefaultProfileUtil {
    private static final String SPRING_PROFILE_DEFAULT = "spring.profiles.default";

    private DefaultProfileUtil() {
        throw new IllegalStateException("Utility class");
    }

    public static void addDefaultProfile(SpringApplication app) {
        Map<String, Object> defProperties = new HashMap<>();
        defProperties.put(SPRING_PROFILE_DEFAULT, EnvConstants.SPRING_PROFILE_DEVELOPMENT);
        app.setDefaultProperties(defProperties);
    }
}
