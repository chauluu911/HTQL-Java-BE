package com.evotek.notification.presentation.web;

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
                .title("MB AMC Notification Management Service")
                .description("MB AMC Notification service provides REST APIs to manage" +
                        " notification resources")
                .version("0.0.1")
                .build();
    }
}
