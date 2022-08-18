package com.evotek.apigateway.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.evotek.apigateway.error.ResponseError;
import lombok.Data;

import java.io.Serializable;
import java.time.Instant;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Data
public class Response<T> implements Serializable {

    protected T data;
    private boolean success;
    private int code;
    private String message;
    private long timestamp;

    public Response() {
        timestamp = Instant.now().toEpochMilli();
        success = true;
        code = 200;
    }

    public static <T> Response<T> of(T res) {
        Response<T> response = new Response<>();
        response.data = res;
        response.success();
        return response;
    }

    public static <T> Response<T> ok() {
        Response<T> response = new Response<>();
        response.success();
        return response;
    }

    public static <T> Response<T> fail() {
        Response<T> response = new Response<>();
        response.setSuccess(false);
        return response;
    }

    public Response<T> data(T res) {
        data = res;
        return this;
    }

    public Response<T> success() {
        success = true;
        code = 200;
        return this;
    }

    public Response<T> success(String message) {
        success = true;
        this.message = message;
        code = 200;
        return this;
    }

    public Response<T> fail(ResponseError responseError) {
        success = false;
        code = responseError.getCode();
        this.message = responseError.getMessage();
        return this;
    }

    public Response<T> fail(String message, ResponseError responseError) {
        success = false;
        code = responseError.getCode();
        this.message = responseError.getMessage();
        return this;
    }

    public Response<T> fail(Exception ex, ResponseError responseError) {
        success = false;
        code = responseError.getCode();
        this.message = ex.getMessage();
        return this;
    }
}
