package com.evotek.meet.domain;

import lombok.Builder;
import lombok.Data;

import java.io.Serializable;

@Data
@Builder
public class UserSchedulerApproveTaskData implements Serializable {
    private String currentUserId;
    private String meetingId;
}
