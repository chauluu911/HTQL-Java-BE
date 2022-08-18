package com.evotek.meet.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.meet.domain.command.UserCreateOrUpdateSchedulerCmd;
import com.evotek.meet.domain.event.BookedTimeEvent;
import com.evotek.meet.infrastructure.support.Util.TimeUtil;
import com.evotek.meet.infrastructure.support.enums.ApproveStatus;
import com.evotek.meet.infrastructure.support.enums.AttendeeType;
import com.evotek.meet.infrastructure.support.exception.BadRequestError;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import javax.persistence.Column;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class User extends AuditableDomain {

    private String id;
    private List<UserScheduler> userSchedulers;
    private Long version;
    private Instant lastModifiedSchedulerAt;
    private Boolean deleted;

    public void joinRequireMeeting(UserCreateOrUpdateSchedulerCmd cmd) {
        if (CollectionUtils.isEmpty(this.userSchedulers)) {
            this.userSchedulers = new ArrayList<>();
        }

        for (UserScheduler userScheduler : this.userSchedulers) {
            if (userScheduler.getMeetingId().equals(cmd.getMeetingId())) {
                userScheduler.delete();
            }
        }

        if (Boolean.TRUE.equals(isConflictScheduler(cmd.getBookedTimeEvents()))) {
            throw new ResponseException(BadRequestError.USER_SCHEDULER_CONFLICT);
        }

        List<UserScheduler> newUserSchedulers = new ArrayList<>();
        for (BookedTimeEvent bookedTimeEvent : cmd.getBookedTimeEvents()) {
            newUserSchedulers.add(new UserScheduler(this.id,
                    AttendeeType.REQUIRED,
                    cmd,
                    bookedTimeEvent.getStartAt(), bookedTimeEvent.getFinishAt()));
        }
        this.userSchedulers.addAll(newUserSchedulers);
    }

    public void joinOptionMeeting(UserCreateOrUpdateSchedulerCmd cmd) {
        if (CollectionUtils.isEmpty(this.userSchedulers)) {
            this.userSchedulers = new ArrayList<>();
        }

        for (UserScheduler userScheduler : this.userSchedulers) {
            if (userScheduler.getMeetingId().equals(cmd.getMeetingId())) {
                userScheduler.delete();
            }
        }

        for (BookedTimeEvent bookedTimeEvent : cmd.getBookedTimeEvents()) {
            this.userSchedulers.add(new UserScheduler(this.id,
                    AttendeeType.OPTIONAL,
                    cmd,
                    bookedTimeEvent.getStartAt(), bookedTimeEvent.getFinishAt()));
        }
    }

    public void enrichUserSchedulers(List<UserScheduler> userSchedulers) {
        this.userSchedulers = userSchedulers.stream()
                .filter(item -> Objects.equals(item.getUserId(), this.getId()))
                .collect(Collectors.toList());
    }

    public void unDelete(){
        this.deleted = false;
    }

    private Boolean isConflictScheduler(List<BookedTimeEvent> bookedTimeEvents) {
        if (CollectionUtils.isEmpty(this.userSchedulers)) {
            this.userSchedulers = new ArrayList<>();
        }
        List<UserScheduler> subUserSchedulers = this.userSchedulers.stream()
                .filter(item -> Boolean.FALSE.equals(item.getDeleted())
                        && AttendeeType.REQUIRED.equals(item.getAttendeeType())
                        && ApproveStatus.APPROVED.equals(item.getApproveStatus()))
                .collect(Collectors.toList());
        for (BookedTimeEvent bookedTimeEvent : bookedTimeEvents) {
            for (UserScheduler userScheduler : subUserSchedulers) {
                Instant newSchedulerStartAt = bookedTimeEvent.getStartAt();
                Instant newSchedulerFinishAt = bookedTimeEvent.getFinishAt();
                Instant userSchedulerStartAt = userScheduler.getStartAt();
                Instant userSchedulerFinishAt = userScheduler.getFinishAt();

                if (Boolean.TRUE.equals(TimeUtil.checkTime(newSchedulerStartAt, newSchedulerFinishAt, userSchedulerStartAt, userSchedulerFinishAt))) {
                    return true;
                }
            }
        }

        return false;
    }

    public void approve(String meetingId) {
        if (CollectionUtils.isEmpty(this.userSchedulers)) {
            this.userSchedulers = new ArrayList<>();
        }
        List<UserScheduler> subUserSchedulers = new ArrayList<>();

        for (UserScheduler userScheduler : this.userSchedulers) {
            if (Objects.equals(userScheduler.getMeetingId(), meetingId)) {
                userScheduler.approve();
                subUserSchedulers.add(userScheduler);
            }
        }

        for (UserScheduler userScheduler : this.userSchedulers) {
            for (UserScheduler userSchedulerApprove : subUserSchedulers) {
                if (!Objects.equals(userScheduler.getMeetingId(), meetingId)) {
                    Instant schedulerStartAt = userScheduler.getStartAt();
                    Instant schedulerFinishAt = userScheduler.getFinishAt();
                    Instant existedSchedulerStartAt = userSchedulerApprove.getStartAt();
                    Instant existedSchedulerFinishAt = userSchedulerApprove.getFinishAt();

                    if (Boolean.TRUE.equals(TimeUtil.checkTime(schedulerStartAt, schedulerFinishAt, existedSchedulerStartAt, existedSchedulerFinishAt))) {
                        userScheduler.reject();
                    }
                }
            }
        }
        this.lastModifiedSchedulerAt = Instant.now();
    }

    public void reject(String meetingId) {
        if (CollectionUtils.isEmpty(this.userSchedulers)) {
            this.userSchedulers = new ArrayList<>();
        }
        List<UserScheduler> subUserSchedulers = this.userSchedulers.stream()
                .filter(u -> Objects.equals(u.getMeetingId(), meetingId)).collect(Collectors.toList());
        subUserSchedulers.forEach(UserScheduler::reject);
        this.lastModifiedSchedulerAt = Instant.now();
    }

    public void rejectScheduler(String userSchedulerId) {
        if (CollectionUtils.isEmpty(this.userSchedulers)) {
            this.userSchedulers = new ArrayList<>();
        }

        Optional<UserScheduler> optionalUserScheduler = this.userSchedulers.stream()
                .filter(item -> Objects.equals(userSchedulerId, item.getId()))
                .findFirst();
        optionalUserScheduler.ifPresent(UserScheduler::reject);
        this.lastModifiedSchedulerAt = Instant.now();
    }

    public void delete(String meetingId) {
        if (CollectionUtils.isEmpty(this.userSchedulers)) {
            this.userSchedulers = new ArrayList<>();
        }
        List<UserScheduler> subUserSchedulers = this.userSchedulers.stream()
                .filter(u -> Objects.equals(u.getMeetingId(), meetingId) && u.getStartAt().compareTo(Instant.now()) > 0).collect(Collectors.toList());
        subUserSchedulers.forEach(UserScheduler::delete);
    }
}
