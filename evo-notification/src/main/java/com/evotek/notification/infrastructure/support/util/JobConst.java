package com.evotek.notification.infrastructure.support.util;

public final class JobConst {
    public static final String SEND_EVENT_TASK = "send-event-task";
    public static final String POLLING_UPDATE_RECEIVED_TELEGRAM = "polling-update-received-telegram";
    public static final Integer TIME_RE_RUN_TASK_DEAD = 15; // (realtime = TIME_RE_RUN_TASK_DEAD * 4)
    public static final Integer MAX_RETRIES_TASK = 3;
    public static final Integer RETRY_DELAY = 60;
    public static final Integer SEND_DELAY = 5;
}
