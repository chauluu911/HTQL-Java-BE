package com.evotek.apigateway.config;

import org.springframework.cloud.gateway.discovery.DiscoveryClientRouteDefinitionLocator;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;
import springfox.documentation.swagger.web.SwaggerResource;
import springfox.documentation.swagger.web.SwaggerResourcesProvider;

import java.util.ArrayList;
import java.util.List;

@Component
@Primary
public class SwaggerProvider implements SwaggerResourcesProvider {
    public static final String API_URI = "/v2/api-docs";

    public static final String EUREKA_SUB_PREFIX = "ReactiveCompositeDiscoveryClient_";

    private final DiscoveryClientRouteDefinitionLocator routeLocator;

    public SwaggerProvider(DiscoveryClientRouteDefinitionLocator routeLocator) {
        this.routeLocator = routeLocator;
    }

    @Override
    public List<SwaggerResource> get() {
        List<SwaggerResource> resources = new ArrayList<>();
        List<String> routes = new ArrayList<>();
        //Remove routes from DiscoveryClientRouteDefinitionLocator and construct swaggerResource
        routeLocator.getRouteDefinitions().subscribe(routeDefinition -> {
            String resourceName = routeDefinition.getId().substring(EUREKA_SUB_PREFIX.length());
            String location = routeDefinition.getPredicates().get(0).getArgs().get("pattern").replace("/**", API_URI);
            resources.add(swaggerResource(resourceName, location));
        });
        return resources;
    }

    private SwaggerResource swaggerResource(String name, String location) {
        SwaggerResource swaggerResource = new SwaggerResource();
        swaggerResource.setName(name);
        swaggerResource.setLocation(location);
        swaggerResource.setSwaggerVersion("2.0");
        return swaggerResource;

    }

}
