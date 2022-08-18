package com.evotek.storage.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;
import lombok.Getter;

@Getter
public enum BadRequestError implements ResponseError {

    FILE_UPLOAD_INVALID(40003001, "File is invalid"),
    EXTENSION_FILE_UPLOAD_INVALID(40003002, "Extension file is invalid"),
    ;

    private final Integer code;
    private final String message;

    BadRequestError(Integer code, String message) {
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
        return 400;
    }

    @Override
    public Integer getCode() {
        return code;
    }
}
