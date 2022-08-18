package com.evotek.meet.application.service;

import com.evotek.meet.domain.UserSchedulerApproveTaskData;
import com.evotek.meet.domain.UserSchedulerRejectOrCancelTaskData;
public interface SendService {
    void sendMail(String id);

    void sendUserScheduler(String id);

    void sendRejectOrCancelUserScheduler(UserSchedulerRejectOrCancelTaskData data);

    void sendApproveUserScheduler(UserSchedulerApproveTaskData data);

    void sendNotification(String eventId);
}
