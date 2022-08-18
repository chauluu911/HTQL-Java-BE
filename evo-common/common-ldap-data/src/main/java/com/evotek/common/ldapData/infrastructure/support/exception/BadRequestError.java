package com.evotek.common.ldapData.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;
import lombok.Getter;

@Getter
public enum BadRequestError implements ResponseError {
    NOT_CREATE_CONTEXT_SOURCE_LDAP(40001001, "Can not create context"),
    ACCOUNT_LDAP_CAN_NOT_CHANGE_PASSWORD(40001002, "Account LDAP can not execute the function changing password"),
    NOT_UPDATE_CONTEXT_SOURCE_LDAP(40001003, "Can not update context"),
    CONNECT_LDAP_FAIL(40001004, "CONNECT LDAP FAIL"),
    CREATE_USER_LDAP_FAIL(40001005, "Create user LDAP fail"),
    USER_IS_ALREADY_IN_GROUP(40001006, "The user is already in the group."),
    CHANGE_PASSWORD_NOT_SUPPORTED(40001007, "Change password is supported");

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
