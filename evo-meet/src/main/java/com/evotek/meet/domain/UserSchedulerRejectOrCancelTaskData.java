package com.evotek.meet.domain;

import lombok.Builder;
import lombok.Data;

import java.io.Serializable;

@Data
@Builder
public class UserSchedulerRejectOrCancelTaskData implements Serializable {
    private String userSchedulerId;
    private String currentUserId;
    private String meetingId;
    private Boolean isOrganizer;
    private Boolean isCancel = false;
    private Boolean isReject = false;
}
