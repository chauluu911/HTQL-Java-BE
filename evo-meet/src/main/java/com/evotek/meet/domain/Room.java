package com.evotek.meet.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.meet.domain.command.RoomCreateCmd;
import com.evotek.meet.domain.command.RoomUpdateCmd;
import com.evotek.meet.domain.event.BookedTimeEvent;
import com.evotek.meet.infrastructure.support.Util.TimeUtil;
import com.evotek.meet.infrastructure.support.enums.RoomStatus;
import com.evotek.meet.infrastructure.support.exception.BadRequestError;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Room extends AuditableDomain {

    private String id;
    private String code;
    private String name;
    private String location;
    private RoomStatus status;
    private Boolean deleted;
    private Long version;
    private List<RoomScheduler> roomSchedulers;

    public Room(RoomCreateCmd cmd) {
        this.id = IdUtils.nextId();
        this.code = cmd.getCode();
        this.name = cmd.getName();
        this.location = cmd.getLocation();
        this.status = cmd.getStatus() != null ? cmd.getStatus() : RoomStatus.ACTIVE;
        this.deleted = false;
    }

    public void reserve(String meetingId, List<BookedTimeEvent> bookedTimeEvents) {
        if (CollectionUtils.isEmpty(this.roomSchedulers)) {
            this.roomSchedulers = new ArrayList<>();
        }
        for (RoomScheduler roomScheduler : this.roomSchedulers) {
            if (roomScheduler.getMeetingId().equals(meetingId)) {
                roomScheduler.delete();
            }
        }
        List<RoomScheduler> newRoomSchedulers = this.createRoomScheduler(meetingId, bookedTimeEvents);
        Boolean result = isConflictRoomScheduler(newRoomSchedulers);
        if (!Boolean.FALSE.equals(result)) {
            throw new ResponseException(BadRequestError.ROOM_SCHEDULER_CONFLICT);
        }

        this.roomSchedulers.addAll(newRoomSchedulers);
    }

    public void update(RoomUpdateCmd cmd) {
        this.name = cmd.getName();
        this.location = cmd.getLocation();
        this.status = cmd.getStatus();
    }

    public void delete(String meetingId) {
        List<RoomScheduler> suRoomSchedulers = this.roomSchedulers.stream()
                .filter(r -> Objects.equals(r.getMeetingId(), meetingId) && r.getStartAt().compareTo(Instant.now()) > 0).collect(Collectors.toList());
        suRoomSchedulers.forEach(RoomScheduler::delete);
    }

    public void active() {
        this.status = RoomStatus.ACTIVE;
    }

    public void inactive() {
        this.status = RoomStatus.INACTIVE;
    }

    public void enrichRoomSchedulers(List<RoomScheduler> roomSchedulers) {
        this.roomSchedulers = roomSchedulers;
    }

    private Boolean isConflictRoomScheduler(List<RoomScheduler> newRoomSchedulers) {
        List<RoomScheduler> undeletedRoomSchedulers = this.roomSchedulers.stream()
                .filter(item -> Boolean.FALSE.equals(item.getDeleted())).collect(Collectors.toList());
        for (RoomScheduler newRoomScheduler : newRoomSchedulers) {
            for (RoomScheduler existedRoomScheduler : undeletedRoomSchedulers) {
                Instant newSchedulerStartAt = newRoomScheduler.getStartAt();
                Instant newRoomSchedulerFinishAt = newRoomScheduler.getFinishAt();
                Instant existedSchedulerStartAt = existedRoomScheduler.getStartAt();
                Instant existedSchedulerFinishAt = existedRoomScheduler.getFinishAt();

                if (Boolean.TRUE.equals(TimeUtil.checkTime(newSchedulerStartAt, newRoomSchedulerFinishAt, existedSchedulerStartAt, existedSchedulerFinishAt))) {
                    return true;
                }
            }
        }
        return false;
    }

    private List<RoomScheduler> createRoomScheduler(String meetingId, List<BookedTimeEvent> bookedTimeEvents) {
        List<RoomScheduler> roomSchedulerList = new ArrayList<>();
        for (BookedTimeEvent bookedTimeEvent : bookedTimeEvents) {
            roomSchedulerList.add(new RoomScheduler(bookedTimeEvent.getStartAt(), bookedTimeEvent.getFinishAt(), this.id, meetingId));
        }
        return roomSchedulerList;
    }
}
