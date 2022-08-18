package com.evotek.meet.infrastructure.support.Util;

public interface JobConst {
    String SEND_MAIL_TASK = "send-mail-task";
    String SEND_NOTIFICATION_TASK = "send-notification-task";
    String SEND_USER_SCHEDULER_TASK = "send-user-scheduler-task";
    String SEND_REJECT_USER_SCHEDULER_TASK = "send-reject-user-scheduler-task";
    String SEND_APPROVE_USER_SCHEDULER_TASK = "send-approve-user-scheduler-task";
    Integer TIME_RE_RUN_TASK_DEAD = 15; // (realtime = TIME_RE_RUN_TASK_DEAD * 4)
    Integer MAX_RETRIES_TASK = 3;
    Integer RETRY_DELAY = 60;
    Integer SEND_DELAY = 5;
}
