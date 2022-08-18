package com.evotek.meet.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;

public enum BadRequestError implements ResponseError {

    ROOM_CODE_EXISTED(40006001, "Room code existed"),
    ROOM_SCHEDULER_CONFLICT(40006002, "New Scheduler of room conflict with existed scheduler"),
    REQUIRE_USER_NOT_EXISTED(40006003, "Require user not existed"),
    OPTIONAL_USER_NOT_EXISTED(40006004, "optional user not existed"),
    USER_SCHEDULER_CONFLICT(40006005, "New Scheduler of user conflict with existed scheduler"),
    INVALID_INPUT(40006006, "Invalid input"),
    MEETING_USING(40006007, "meeting using"),

    START_AT_AND_FINISH_AT_INVALID(40006008, "StartAt greater than or equal to finishAt"),

    MEETING_USER_INVALID(40006009, "User attend meeting invalid");
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
