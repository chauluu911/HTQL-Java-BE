package com.evotek.apigateway.error;

public enum InternalServerError implements ResponseError {
    
    SERVICE_UNAVAILABLE_ERROR(503, "Service unavailable"),
    ;

    private final Integer code;
    private final String message;

    InternalServerError(Integer code, String message) {
        this.code = code;
        this.message = message;
    }

    @Override
    public String getName() {
        return this.name();
    }

    @Override
    public String getMessage() {
        return message;
    }

    @Override
    public int getStatus() {
        return this.code;
    }

    @Override
    public Integer getCode() {
        return this.code;
    }
}
