package com.evotek.common.error;

public enum AuthenticationError implements ResponseError {
    UNKNOWN(40100001, "UNKNOWN"),
    UNAUTHORISED(40100002, "Unauthorised"),
    FORBIDDEN_ACCESS_TOKEN(40100003, "Access token has been forbidden due to user has logged out or deactivated"),
    FORBIDDEN_REFRESH_TOKEN(40100004, "Refresh token has been forbidden"),
    INVALID_REFRESH_TOKEN(40100005, "Refresh token has been forbidden"),
    VALIDATE_EXPIRATION_TIME(40100006, "validate expiration time"),
    VALIDATE_TOKEN_ID(40100007, "JWT Token is not an ID Token"),
    VALIDATE_ISSUER(40100008, "Issuer does not match idp"),
    ;

    private final Integer code;
    private final String message;

    AuthenticationError(Integer code, String message) {
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
        return 401;
    }

    @Override
    public Integer getCode() {
        return code;
    }
}
