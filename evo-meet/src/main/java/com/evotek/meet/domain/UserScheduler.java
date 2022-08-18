package com.evotek.meet.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.meet.domain.command.UserCreateOrUpdateSchedulerCmd;
import com.evotek.meet.infrastructure.support.enums.ApproveStatus;
import com.evotek.meet.infrastructure.support.enums.AttendeeType;
import com.evotek.meet.infrastructure.support.enums.MeetingType;
import com.evotek.meet.infrastructure.support.exception.BadRequestError;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.Instant;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class UserScheduler extends AuditableDomain {

    private String id;
    private String userId;
    private String meetingId;
    private String roomId;
    private Instant startAt;
    private Instant finishAt;
    private AttendeeType attendeeType;
    private ApproveStatus approveStatus;
    private String link;
    private Boolean deleted;

    public UserScheduler(String userId, AttendeeType attendeeType,
                         UserCreateOrUpdateSchedulerCmd cmd,
                         Instant startAt, Instant finishAt) {
        // Check startAt and finishAt
        if (startAt.compareTo(finishAt) < 0) {
            this.startAt = startAt;
            this.finishAt = finishAt;
        } else {
            throw new ResponseException(BadRequestError.START_AT_AND_FINISH_AT_INVALID);
        }
        this.id = IdUtils.nextId();
        this.userId = userId;
        this.meetingId = cmd.getMeetingId();
        this.attendeeType = attendeeType;
        if (cmd.getMeetingType().equals(MeetingType.OFFLINE)) {
            this.roomId = cmd.getRoomId();
        } else {
            this.link = cmd.getLink();
        }
        this.deleted = false;
        if (userId.equals(cmd.getOrganizerId())) {
            this.approveStatus = ApproveStatus.APPROVED;
        } else {
            this.approveStatus = ApproveStatus.UNCONFIRMED;
        }
    }

    public void approve() {
        this.approveStatus = ApproveStatus.APPROVED;
    }

    public void reject() {
        this.approveStatus = ApproveStatus.REJECTED;
    }

    void delete() {
        this.deleted = true;
    }
}
