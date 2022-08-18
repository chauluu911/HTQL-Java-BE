package com.evotek.notification.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;
import lombok.Getter;

@Getter
public enum BadRequestError implements ResponseError {
    EVENT_NOT_EXISTED(40004001, "Event not existed"),
    EVENT_CAN_NOT_CHANGE(40004002, "Event can not change"),
    EVENT_CAN_NOT_CANCEL(40004003, "Event can't cancel"),
    DEVICE_TOKEN_NOT_FOUND(40004004, "Device Token not found!"),
    TARGETS_REQUIRE(40004005, "targets required"),
    FILE_MUST_BE_PDF(40004006, "File must be PDF format"),
    EVENT_CAN_NOT_SEND_BEFORE_PRESENT_TIME(40004007, "Event can not send before the present time"),
    NO_RECEIVER_FOUND(40004008, "No receiver found"),
    USER_NOT_PERMISSION(40004009, "User not permission"),
    BUILDING_ID_INVALID(40004010, "Building id invalid"),
    BUILDING_ID_REQUIRED(40004011, "Building id required"),
    FLOOR_ID_INVALID(40004012, "Floor id invalid"),
    ORGANIZATION_ID_INVALID(40004013, "Organization id invalid"),
    EVENT_CAN_NOT_DELETE(40004014, "Event can't delete"),
    FILE_NOT_EXIST(40004015, "File not existed"),
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
