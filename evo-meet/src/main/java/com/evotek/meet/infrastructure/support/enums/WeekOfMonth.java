package com.evotek.meet.infrastructure.support.enums;

public enum WeekOfMonth {
    FIRST(1),
    SECOND(2),
    THIRD(3),
    FOURTH(4),
    LAST(5);

    private final Integer value;

    WeekOfMonth(Integer value) {
        this.value = value;
    }

    public Integer getValue() {
        return value;
    }
}
