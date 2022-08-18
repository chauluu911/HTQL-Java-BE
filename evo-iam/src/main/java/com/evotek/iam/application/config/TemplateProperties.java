package com.evotek.iam.application.config;

import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Data
@Component
@ConfigurationProperties(prefix = "template")
public class TemplateProperties {
    private Template user;

    @Getter
    @Setter
    public static class Template {

        private String templateFileExcel;

        private String templateFileName;
    }
}
