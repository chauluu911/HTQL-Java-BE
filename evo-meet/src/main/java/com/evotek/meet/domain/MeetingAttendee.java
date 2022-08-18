package com.evotek.meet.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.util.IdUtils;
import com.evotek.meet.infrastructure.support.enums.ApproveStatus;
import com.evotek.meet.infrastructure.support.enums.AttendeeType;
import lombok.*;
import lombok.experimental.SuperBuilder;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class MeetingAttendee extends AuditableDomain {

    private String id;
    private String userId;
    private String meetingId;
    private AttendeeType attendeeType;
    private Boolean deleted;
    private ApproveStatus approveStatus;

    public MeetingAttendee(String userId, String meetingId, AttendeeType attendeeType, ApproveStatus approveStatus) {
        this.id = IdUtils.nextId();
        this.userId = userId;
        this.meetingId = meetingId;
        this.attendeeType = attendeeType;
        this.approveStatus = approveStatus;
        this.deleted = false;
    }

    void delete() {
        this.deleted = true;
    }

    void unDelete() {
        this.deleted = false;
    }

    public void approve() {
        this.approveStatus = ApproveStatus.APPROVED;
    }

    public void reject() {
        this.approveStatus = ApproveStatus.REJECTED;
    }
}
