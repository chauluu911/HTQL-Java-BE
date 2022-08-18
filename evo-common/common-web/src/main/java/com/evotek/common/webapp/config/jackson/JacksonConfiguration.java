package com.evotek.common.webapp.config.jackson;

import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.datatype.hibernate5.Hibernate5Module;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.zalando.problem.ProblemModule;
import org.zalando.problem.violations.ConstraintViolationProblemModule;

import java.time.Instant;
import java.time.LocalDate;
import java.util.UUID;

@Configuration
public class JacksonConfiguration {

    @Bean
    public JavaTimeModule javaTimeModule() {
        return new JavaTimeModule();
    }

    @Bean
    public Jdk8Module jdk8TimeModule() {
        return new Jdk8Module();
    }

    @Bean
    public AfterburnerModule afterburnerModule() {
        return new AfterburnerModule();
    }

    @Bean
    ProblemModule problemModule() {
        return new ProblemModule();
    }

    @Bean
    ConstraintViolationProblemModule constraintViolationProblemModule() {
        return new ConstraintViolationProblemModule();
    }

    @Bean
    public Hibernate5Module hibernate5Module() {
        return new Hibernate5Module();
    }

    @Bean
    public SimpleModule stringDeserializer() {
        SimpleModule module = new SimpleModule();
        module.addDeserializer(String.class, new CustomStringDeserializer());
        return module;
    }

    @Bean
    public SimpleModule longDeserializer() {
        SimpleModule module = new SimpleModule();
        module.addDeserializer(Long.class, new CustomLongDeserializer());
        return module;
    }

    @Bean
    public SimpleModule integerDeserializer() {
        SimpleModule module = new SimpleModule();
        module.addDeserializer(Integer.class, new CustomIntegerDeserializer());
        return module;
    }

    @Bean
    public SimpleModule uuidDeserializer() {
        SimpleModule module = new SimpleModule();
        module.addDeserializer(UUID.class, new CustomUUIDDeserializer());
        return module;
    }

    @Bean
    public SimpleModule instantDeserializer() {
        SimpleModule module = new SimpleModule();
        module.addDeserializer(Instant.class, new CustomInstantDeserializer());
        return module;
    }

    @Bean
    public SimpleModule localDateDeserializer() {
        SimpleModule module = new SimpleModule();
        module.addDeserializer(LocalDate.class, new CustomLocalDateDeserializer());
        return module;
    }

    @Bean
    public SimpleModule localDateSerializer() {
        SimpleModule module = new SimpleModule();
        module.addSerializer(LocalDate.class, new CustomLocalDateSerializer());
        return module;
    }
}
