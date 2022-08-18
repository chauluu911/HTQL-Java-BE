package com.evotek.storage.presentation.web;


import com.evotek.common.webapp.config.AbstractSwaggerConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.context.annotation.Configuration;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.service.ApiInfo;

@Configuration
@ConditionalOnExpression(value = "${spring.swagger.enabled:false}")
public class SwaggerConfig extends AbstractSwaggerConfig {

    @Override
    protected ApiInfo metadata() {
        return new ApiInfoBuilder()
                .title("Evotek File Management Service")
                .description("Evotek FileManagement service provides REST APIs to manage" +
                        " file management resources")
                .version("0.0.1")
                .build();
    }
}
