package com.evotek.meet.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
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
public class RoomScheduler extends AuditableDomain {

    private String id;
    private String meetingId;
    private String roomId;
    private Instant startAt;
    private Instant finishAt;
    private Boolean deleted;

    public RoomScheduler(Instant startAt, Instant finishAt, String roomId, String meetingId) {
        // Check startAt and finishAt
        if (startAt.compareTo(finishAt) < 0) {
            this.startAt = startAt;
            this.finishAt = finishAt;
        } else {
            throw new ResponseException(BadRequestError.START_AT_AND_FINISH_AT_INVALID);
        }
        this.id = IdUtils.nextId();
        this.meetingId = meetingId;
        this.roomId = roomId;
        this.deleted = false;
    }

    public void delete() {
        this.deleted = true;
    }
}
