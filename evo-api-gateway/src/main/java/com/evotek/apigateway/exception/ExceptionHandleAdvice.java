package com.evotek.apigateway.exception;

import com.evotek.apigateway.dto.ErrorResponse;
import com.evotek.apigateway.error.ResponseError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.annotation.Order;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.util.LinkedHashSet;

@ControllerAdvice
@Slf4j
@Order
public class ExceptionHandleAdvice {

    @ExceptionHandler(ResponseException.class)
    public Mono<ResponseEntity<ErrorResponse<Object>>> handleResponseException(
            ResponseException e, ServerWebExchange failedExchange) {
        URI originalUri = (URI) ((LinkedHashSet<String>) failedExchange.getAttributes()
                .get("org.springframework.cloud.gateway.support.ServerWebExchangeUtils.gatewayOriginalRequestUrl")).toArray()[0];
        log.error("Failed to handle request " + originalUri + ": " + e.getMessage(), e);
        ResponseError error = e.getError();
        return Mono.just(ResponseEntity.status(error.getStatus())
                .body(ErrorResponse.builder()
                        .code(error.getCode())
                        .error(error.getName())
                        .message(e.getMessage())
                        .build()));
    }
}
