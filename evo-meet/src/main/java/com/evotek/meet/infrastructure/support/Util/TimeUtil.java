package com.evotek.meet.infrastructure.support.Util;

import java.time.Instant;

public class TimeUtil {
    public static Boolean checkTime(Instant schedulerInputStartAt, Instant schedulerInputFinishAt, Instant schedulerStartAt, Instant schedulerFinishAt) {
        return schedulerInputStartAt.compareTo(schedulerStartAt) >= 0 && schedulerInputStartAt.compareTo(schedulerFinishAt) <= 0
                || schedulerInputFinishAt.compareTo(schedulerStartAt) >= 0 && schedulerInputFinishAt.compareTo(schedulerFinishAt) <= 0
                || schedulerInputStartAt.compareTo(schedulerStartAt) <= 0 && schedulerInputFinishAt.compareTo(schedulerFinishAt) >= 0;
    }
}
