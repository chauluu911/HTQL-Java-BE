package com.evotek.asset.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;

public enum BadRequestError implements ResponseError {
    ASSET_TYPE_NOT_EXIST(40006001, "Asset type not exist"),
    ASSET_TYPE_CODE_ALREADY_EXISTED(40006002, "Asset type code already existed"),
    ASSET_NOT_EXIST(40006003, "Asset not exist"),
    ASSET_HISTORY_NOT_EXIST(40006004, "Asset history not exist"),
    ASSET_WAS_DESTROYED(40006005, "Asset was destroyed"),
    ASSET_WAS_LIQUIDATED(40006006, "Asset was liquidated"),
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
        return this.message;
    }

    @Override
    public int getStatus() {
        return 400;
    }

    @Override
    public Integer getCode() {
        return this.code;
    }
}
