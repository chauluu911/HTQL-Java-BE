package com.evotek.iam.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;

public enum NotFoundError implements ResponseError {

    USER_NOT_FOUND(40401001, "User not found: {0}"),
    EMPLOYEE_NOT_FOUND(40401001, "Employee not found: {0}"),
    ROLE_NOT_FOUND(40401002, "Role not found: {0}"),
    ORGANIZATION_NOT_EXITED(40401003, "Organization not exited"),
    CONTACT_NOT_EXITED(40401004, "Contact not exited"),
    UNIT_NOT_EXITED(40401005, "Unit not exited"),
    ORGANIZATION_NOT_FOUND(40401006, "Organization not found: {0}"),
    DEPARTMENT_NOT_FOUND(40401007, "Department not found"),
    FILE_NOT_FOUND(40401008, "File not found"),
    USER_GROUP_NOT_FOUND(40401009, "Group user not found"),
    JOB_TITLE_NOT_FOUND(40401009, "Job title not found");

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
