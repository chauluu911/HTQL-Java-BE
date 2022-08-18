package com.evotek.meet.infrastructure.support.exception;

import com.evotek.common.error.ResponseError;

public enum NotFoundError implements ResponseError {

    ROOM_NOT_FOUND(40406001, "Room not found"),
    ORGANIZER_NOT_FOUND(40406002, "Organizer id not found"),
    USER_SCHEDULER_NOT_FOUND(40406003, "User scheduler not found"),

    USER_NOT_FOUND(40406004, "User not found"),
    MEETING_NOT_FOUND(40406005, "Meeting not found");

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
