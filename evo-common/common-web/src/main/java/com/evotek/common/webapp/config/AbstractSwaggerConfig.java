package com.evotek.common.webapp.config;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpHeaders;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.RequestMappingInfoHandlerMapping;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.builders.RequestParameterBuilder;
import springfox.documentation.schema.ScalarType;
import springfox.documentation.service.*;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spi.service.RequestHandlerProvider;
import springfox.documentation.spi.service.contexts.SecurityContext;
import springfox.documentation.spring.web.WebMvcRequestHandler;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.spring.web.plugins.DocumentationPluginsBootstrapper;
import springfox.documentation.spring.web.plugins.WebMvcRequestHandlerProvider;
import springfox.documentation.spring.web.readers.operation.HandlerMethodResolver;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

import javax.servlet.ServletContext;
import java.util.*;

import static java.util.stream.Collectors.toList;
import static springfox.documentation.RequestHandler.byPatternsCondition;
import static springfox.documentation.spring.web.paths.Paths.ROOT;

// link /swagger-ui/index.html

@EnableSwagger2
public abstract class AbstractSwaggerConfig {

    private static final String BASE_CLASS = "com.evotek";

    protected abstract ApiInfo metadata();

    @Bean
    public Docket api() {
        AuthorizationScope[] authScopes = new AuthorizationScope[0];
        SecurityReference securityReference = SecurityReference.builder()
                .reference("bearer_token")
                .scopes(authScopes)
                .build();

        List<SecurityContext> securityContexts = Collections.singletonList(
                SecurityContext.builder()
                        .securityReferences(Collections.singletonList(securityReference))
                        .build());

        return new Docket(DocumentationType.SWAGGER_2)
                .select()
                .apis(RequestHandlerSelectors.basePackage(basePackage()))
                .paths(PathSelectors.any())
                .build()
                .apiInfo(metadata())
                .globalRequestParameters(defaultParameters())
                .useDefaultResponseMessages(false)
                .securitySchemes(Collections.singletonList(securityScheme()))
                .securityContexts(securityContexts)
                .genericModelSubstitutes(Optional.class);
    }

    protected String basePackage() {
        return BASE_CLASS;
    }

    private SecurityScheme securityScheme() {
        return new ApiKey("bearer_token", "Authorization", "header");
    }

    protected List<RequestParameter> defaultParameters() {
        RequestParameter parameter = new RequestParameterBuilder()
                .name(HttpHeaders.ACCEPT_LANGUAGE)
                .query(simpleParameterSpecificationBuilder -> simpleParameterSpecificationBuilder
                        .model(modelSpecificationBuilder -> modelSpecificationBuilder.scalarModel(ScalarType.STRING))
                        .defaultValue("vi"))
                .in(ParameterType.HEADER)
                .required(true)
                .build();
        return Collections.singletonList(parameter);
    }

    @Bean
    public InitializingBean removeSpringfoxHandlerProvider(DocumentationPluginsBootstrapper bootstrapper) {
        return () -> bootstrapper.getHandlerProviders().removeIf(WebMvcRequestHandlerProvider.class::isInstance);
    }

    @Bean
    public RequestHandlerProvider customRequestHandlerProvider(Optional<ServletContext> servletContext,
                                                               HandlerMethodResolver methodResolver,
                                                               List<RequestMappingInfoHandlerMapping> handlerMappings) {
        String contextPath = servletContext.map(ServletContext::getContextPath).orElse(ROOT);
        return () -> handlerMappings.stream()
                .filter(mapping -> !Objects.equals(mapping.getClass().getSimpleName(), "IntegrationRequestMappingHandlerMapping"))
                .map(mapping -> mapping.getHandlerMethods().entrySet())
                .flatMap(Set::stream)
                .map(entry
                        -> new WebMvcRequestHandler(contextPath, methodResolver, tweakInfo(entry.getKey()), entry.getValue()))
                .sorted(byPatternsCondition())
                .collect(toList());
    }

    RequestMappingInfo tweakInfo(RequestMappingInfo info) {
        if (info.getPathPatternsCondition() == null) {
            return info;
        }
        String[] patterns = info.getPathPatternsCondition().getPatternValues().toArray(String[]::new);
        return info.mutate().options(new RequestMappingInfo.BuilderConfiguration()).paths(patterns).build();
    }
}
