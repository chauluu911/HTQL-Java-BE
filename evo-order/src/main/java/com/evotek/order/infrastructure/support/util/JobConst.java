package com.evotek.order.infrastructure.support.util;

public interface JobConst {
    String SEND_NOTIFICATION_TASK = "send-notification-task";
    Integer TIME_RE_RUN_TASK_DEAD = 15; // (realtime = TIME_RE_RUN_TASK_DEAD * 4)
    Integer MAX_RETRIES_TASK = 3;
    Integer RETRY_DELAY = 3;

}
