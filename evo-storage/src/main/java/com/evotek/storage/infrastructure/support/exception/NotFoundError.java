package com.evotek.storage.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;

public enum NotFoundError implements ResponseError {

    FILE_NOT_FOUND(40403001, "File not found"),
    ;

    private final Integer code;
    private final String message;

    NotFoundError(Integer code, String message) {
        this.code = code;
        this.message = message;
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
        return code;
    }
}
