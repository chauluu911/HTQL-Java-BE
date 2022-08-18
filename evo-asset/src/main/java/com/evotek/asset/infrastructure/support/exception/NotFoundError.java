package com.evotek.asset.infrastructure.support.exception;


import com.evotek.common.error.ResponseError;

public enum NotFoundError implements ResponseError {
    ;
    private final Integer code;
    private final String message;

    NotFoundError(Integer code, String messgae) {
        this.code = code;
        this.message = messgae;
    }

    @Override
    public String getName() {
        return name();
    }

    @Override
    public String getMessage() {
        return message;
    }

    @Override
    public int getStatus() {
        return 404;
    }

    @Override
    public Integer getCode() {
        return this.code;
    }

}
