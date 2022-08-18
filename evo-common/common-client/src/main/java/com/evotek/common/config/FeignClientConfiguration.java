package com.evotek.common.config;

import com.evotek.common.config.security.ClientAuthentication;
import com.evotek.common.exception.ForwardInnerAlertException;
import feign.Logger;
import feign.RequestInterceptor;
import feign.Retryer;
import feign.codec.ErrorDecoder;
import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.timelimiter.TimeLimiterConfig;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.cloud.circuitbreaker.resilience4j.Resilience4JCircuitBreakerFactory;
import org.springframework.cloud.circuitbreaker.resilience4j.Resilience4JConfigBuilder;
import org.springframework.cloud.client.circuitbreaker.Customizer;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

@Configuration
@EnableFeignClients(basePackages = {"com.evotek"})
@AutoConfigureAfter(ClientAuthentication.class)
@Slf4j
public class FeignClientConfiguration {

    private final ClientAuthentication clientAuthentication;

    public FeignClientConfiguration(ClientAuthentication clientAuthentication) {
        this.clientAuthentication = clientAuthentication;
    }

    @Bean
    public RequestInterceptor requestInterceptor() {
        FeignClientInterceptor feignClientInterceptor = new FeignClientInterceptor();
        feignClientInterceptor.setClientAuthentication(this.clientAuthentication);
        return feignClientInterceptor;
    }

    @Bean
    Logger.Level feignLoggerLevel() {
        return Logger.Level.BASIC;
    }

    @Bean
    public ErrorDecoder errorDecoder() {
        return new CustomFeignErrorDecoder();
    }

    @Bean
    public Retryer retryer() {
        return new CustomFeignRetryer();
    }

    /*****************************************************************************
     @Bean public ServiceInstanceListSupplier discoveryClientServiceInstanceListSupplier(
     ConfigurableApplicationContext context) {
     log.info("Configuring Load balancer to prefer same instance");
     return ServiceInstanceListSupplier.builder()
     .withBlockingDiscoveryClient()
     .withSameInstancePreference()
     .build(context);
     }
     ******************************************************************************/

    @Bean
    public Customizer<Resilience4JCircuitBreakerFactory> defaultCustomizer() {
        CircuitBreakerConfig circuitBreakerConfig = CircuitBreakerConfig.custom()
                .slidingWindowType(CircuitBreakerConfig.SlidingWindowType.COUNT_BASED)
                .slidingWindowSize(100)
                .minimumNumberOfCalls(100)
                .failureRateThreshold(50)
                .permittedNumberOfCallsInHalfOpenState(3)
                .waitDurationInOpenState(Duration.ofSeconds(30))
                .ignoreExceptions(ForwardInnerAlertException.class)
                .build();

        TimeLimiterConfig timeLimiterConfig = TimeLimiterConfig.custom()
                .timeoutDuration(Duration.ofSeconds(60))
                .build();

        return factory ->
                factory.configureDefault(id -> new Resilience4JConfigBuilder(id)
                        .circuitBreakerConfig(circuitBreakerConfig)
                        .timeLimiterConfig(timeLimiterConfig)
                        .build());
    }
}
