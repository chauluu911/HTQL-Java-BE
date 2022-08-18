package com.evotek.common.util;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;
import org.zalando.problem.ProblemModule;
import org.zalando.problem.violations.ConstraintViolationProblemModule;

public class MapperFactoryUtil {

    private MapperFactoryUtil() {
    }

    public static ObjectMapper jacksonMapper() {
        ObjectMapper objectMapper = new ObjectMapper()
                .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        objectMapper.activateDefaultTyping(
                objectMapper.getPolymorphicTypeValidator(),
                ObjectMapper.DefaultTyping.NON_FINAL,
                JsonTypeInfo.As.PROPERTY);
        objectMapper.setVisibility(PropertyAccessor.GETTER, JsonAutoDetect.Visibility.ANY);
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        objectMapper.registerModules(
                new JavaTimeModule(),
                new Jdk8Module(),
                new ConstraintViolationProblemModule(),
                new ProblemModule(),
                new AfterburnerModule());
        return objectMapper;
    }
}
