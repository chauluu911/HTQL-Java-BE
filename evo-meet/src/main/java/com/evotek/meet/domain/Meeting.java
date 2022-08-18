package com.evotek.meet.domain;

import com.evotek.common.domain.AuditableDomain;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.IdUtils;
import com.evotek.common.util.StrUtils;
import com.evotek.meet.domain.command.MeetingCreateOrUpdateCmd;
import com.evotek.meet.domain.command.UserCreateOrUpdateSchedulerCmd;
import com.evotek.meet.domain.event.BookedTimeEvent;
import com.evotek.meet.infrastructure.support.enums.*;
import com.evotek.meet.infrastructure.support.exception.BadRequestError;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.springframework.util.CollectionUtils;

import java.time.*;
import java.time.temporal.TemporalAdjuster;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import static java.time.temporal.ChronoField.DAY_OF_MONTH;
import static java.time.temporal.ChronoField.DAY_OF_WEEK;

@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Setter(AccessLevel.PRIVATE)
@Getter
public class Meeting extends AuditableDomain {

    private String id;
    private String title;
    private String description;
    private Instant startAt;
    private Instant finishAt;
    private LocalDate endDate;
    private RepeatType repeatType;
    private MeetingType meetingType;
    private String link;
    private Integer repeatMeasure;
    private String roomId;
    private String organizerId;
    private String presiderId;
    private List<DayOfWeek> dayOfWeeks = new ArrayList<>();
    private Integer dayOfMonth;
    private WeekOfMonth weekOfMonth;
    private Month monthOfYear;
    private Boolean deleted;
    private ApproveStatus approveStatus;

    private MeetingStatus meetingStatus;
    private List<MeetingAttendee> meetingAttendees;

    @JsonIgnore
    private List<User> users;
    @JsonIgnore
    private Room room;

    public Meeting(MeetingCreateOrUpdateCmd cmd, Room room, String organizerId, List<User> existedUsers) {
        this.id = IdUtils.nextId();
        this.roomId = cmd.getRoomId();
        updateOrCreate(cmd, organizerId);

        List<BookedTimeEvent> bookedTimeEvents = this.createEventScheduler(cmd);
        room.reserve(this.id, bookedTimeEvents);
        this.updateUserScheduler(cmd, bookedTimeEvents, existedUsers);
        this.updateMeetingAttendee(cmd.getRequiredUserIds(), cmd.getOptionalUserIds());
        this.room = room;
        this.users = existedUsers;
    }

    public Meeting(MeetingCreateOrUpdateCmd cmd, String organizerId, List<User> existedUsers) {
        this.id = IdUtils.nextId();
        this.link = cmd.getLink();
        updateOrCreate(cmd, organizerId);

        List<BookedTimeEvent> bookedTimeEvents = this.createEventScheduler(cmd);
        this.updateUserScheduler(cmd, bookedTimeEvents, existedUsers);
        this.updateMeetingAttendee(cmd.getRequiredUserIds(), cmd.getOptionalUserIds());
        this.users = existedUsers;
    }

    public void updateUserScheduler(MeetingCreateOrUpdateCmd cmd, List<BookedTimeEvent> bookedTimeEvents, List<User> existedUsers) {

        UserCreateOrUpdateSchedulerCmd userCmd = UserCreateOrUpdateSchedulerCmd.builder()
                .meetingId(this.id).roomId(cmd.getRoomId())
                .meetingType(cmd.getMeetingType()).organizerId(this.organizerId)
                .link(cmd.getLink())
                .bookedTimeEvents(bookedTimeEvents).build();

        if (!CollectionUtils.isEmpty(cmd.getRequiredUserIds())) {
            for (String userId : cmd.getRequiredUserIds()) {
                Optional<User> optionalUser = existedUsers.stream().filter(item -> Objects.equals(item.getId(), userId)).findFirst();
                if (optionalUser.isEmpty()) {
                    throw new ResponseException(BadRequestError.REQUIRE_USER_NOT_EXISTED);
                }
                User user = optionalUser.get();
                user.joinRequireMeeting(userCmd);
            }
        }

        if (!CollectionUtils.isEmpty(cmd.getOptionalUserIds())) {
            for (String userId : cmd.getOptionalUserIds()) {
                Optional<User> optionalUser = existedUsers.stream().filter(item -> Objects.equals(item.getId(), userId)).findFirst();
                if (optionalUser.isEmpty()) {
                    throw new ResponseException(BadRequestError.OPTIONAL_USER_NOT_EXISTED);
                }
                User user = optionalUser.get();
                user.joinOptionMeeting(userCmd);
            }
        }
    }

    public void update(MeetingCreateOrUpdateCmd cmd, Room room, String organizerId, List<User> existedUsers) {
        this.roomId = cmd.getRoomId();
        updateOrCreate(cmd, organizerId);

        List<BookedTimeEvent> bookedTimeEvents = this.createEventScheduler(cmd);
        room.reserve(this.id, bookedTimeEvents);
        this.updateUserScheduler(cmd, bookedTimeEvents, existedUsers);
        this.updateMeetingAttendee(cmd.getRequiredUserIds(), cmd.getOptionalUserIds());
        this.room = room;
        this.users = existedUsers;
    }

    public void update(MeetingCreateOrUpdateCmd cmd, String organizerId, List<User> existedUsers) {
        updateOrCreate(cmd, organizerId);
        this.link = cmd.getLink();

        List<BookedTimeEvent> bookedTimeEvents = this.createEventScheduler(cmd);
        this.updateUserScheduler(cmd, bookedTimeEvents, existedUsers);
        this.updateMeetingAttendee(cmd.getRequiredUserIds(), cmd.getOptionalUserIds());
        this.users = existedUsers;
    }

    private void updateOrCreate(MeetingCreateOrUpdateCmd cmd, String organizerId) {
        this.title = cmd.getTitle();
        this.description = cmd.getDescription();
        this.organizerId = organizerId;
        this.repeatType = cmd.getRepeatType();
        this.repeatMeasure = cmd.getRepeatMeasure();
        this.dayOfWeeks = cmd.getDayOfWeekList();
        this.dayOfMonth = cmd.getDayOfMonth();
        this.weekOfMonth = cmd.getWeekOfMonth();
        this.monthOfYear = cmd.getMonthOfYear();
        this.startAt = cmd.getStartAt();
        this.finishAt = cmd.getFinishAt();
        this.endDate = cmd.getEndDate();
        this.meetingType = cmd.getMeetingType();
        this.meetingStatus = MeetingStatus.ACTIVE;
        this.deleted = false;

        if (Objects.nonNull(cmd.getPresiderId())) {
            this.presiderId = cmd.getPresiderId();
        } else {
            this.presiderId = organizerId;
        }
    }

    public void updateMeetingAttendee(List<String> requiredUserIds, List<String> optionalUserIds) {
        if (!CollectionUtils.isEmpty(this.meetingAttendees)) {
            this.meetingAttendees.forEach(MeetingAttendee::delete);
        } else {
            this.meetingAttendees = new ArrayList<>();
        }

        for (String userId : requiredUserIds) {
            resolveMeetingAttendee(userId, AttendeeType.REQUIRED);
        }

        for (String userId : optionalUserIds) {
            resolveMeetingAttendee(userId, AttendeeType.OPTIONAL);
        }
    }

    private void resolveMeetingAttendee(String userId, AttendeeType attendeeType) {
        List<String> attendeeIds = this.meetingAttendees.stream().map(MeetingAttendee::getUserId).collect(Collectors.toList());
        Optional<MeetingAttendee> optionalMeetingAttendee = this.meetingAttendees.stream().
                filter(item -> Objects.equals(item.getUserId(), userId)).findFirst();
        if (optionalMeetingAttendee.isEmpty()) {
            if (userId.equals(this.organizerId)) {
                this.meetingAttendees.add(new MeetingAttendee(userId, this.id, attendeeType, ApproveStatus.APPROVED));
            } else {
                this.meetingAttendees.add(new MeetingAttendee(userId, this.id, attendeeType, ApproveStatus.UNCONFIRMED));
            }
        } else {
            if(attendeeIds.contains(userId)){
                if (Objects.equals(userId, this.organizerId)) {
                    this.meetingAttendees.add(new MeetingAttendee(userId, this.id, attendeeType, ApproveStatus.APPROVED));
                } else {
                    this.meetingAttendees.add(new MeetingAttendee(userId, this.id, attendeeType, ApproveStatus.UNCONFIRMED));
                }
            }else{
                MeetingAttendee meetingAttendee = optionalMeetingAttendee.get();
                meetingAttendee.unDelete();
            }
        }
    }

    public void delete() {
        if(this.meetingStatus.equals(MeetingStatus.CANCELED)){
            this.deleted = true;
        } else {
            throw new ResponseException(BadRequestError.MEETING_USING);
        }

    }

    public void cancel(String userId) {
        if(!StrUtils.isBlank(userId)){
            if (!CollectionUtils.isEmpty(this.meetingAttendees)) {
                for (MeetingAttendee meetingAttendee: this.meetingAttendees) {
                    if(Objects.equals(userId, meetingAttendee.getUserId())){
                        meetingAttendee.delete();
                        break;
                    }
                }
            }
            if (!CollectionUtils.isEmpty(this.users)) {
                for (User user: this.users) {
                    if(Objects.equals(userId, user.getId())){
                        user.delete(this.id);
                        break;
                    }
                }
            }
        } else {
            if (!CollectionUtils.isEmpty(this.meetingAttendees)) {
                this.meetingAttendees.forEach(MeetingAttendee::delete);
            }
            if (!CollectionUtils.isEmpty(this.users)) {
                this.users.forEach(user -> user.delete(this.id));
            }
            if (!Objects.isNull(this.room)) {
                this.room.delete(this.id);
            }
            this.meetingStatus = MeetingStatus.CANCELED;
        }
    }

    public void approve() {
        if (!CollectionUtils.isEmpty(this.users)) {
            this.users.forEach(u -> u.approve(this.id));
        }
        List<String> rejectMeetingIds = new ArrayList<>();
        for (User user : this.users) {
            for (UserScheduler userScheduler : user.getUserSchedulers()) {
                if (userScheduler.getApproveStatus().equals(ApproveStatus.REJECTED) && !rejectMeetingIds.contains(userScheduler.getMeetingId())) {
                    rejectMeetingIds.add(userScheduler.getMeetingId());
                }
            }
        }

        for (MeetingAttendee meetingAttendee : this.meetingAttendees) {
            if (Objects.equals(meetingAttendee.getMeetingId(), this.id)) {
                meetingAttendee.approve();
            }
            if (rejectMeetingIds.contains(meetingAttendee.getMeetingId())) {
                meetingAttendee.reject();
            }
        }
    }

    public void reject() {
        for (MeetingAttendee meetingAttendee : this.meetingAttendees) {
            meetingAttendee.reject();
        }

        if (!CollectionUtils.isEmpty(this.users)) {
            this.users.forEach(user -> user.reject(this.id));
        }
    }

    public void enrichMeetingAttendees(List<MeetingAttendee> meetingAttendees) {
        this.meetingAttendees = meetingAttendees;
    }

    public void enrichApproveStatus(ApproveStatus approveStatus) {
        this.approveStatus = approveStatus;
    }

    public void enrichRoom(Room room) {
        this.room = room;
    }

    public void enrichUsers(List<User> existedUsers) {
        if (CollectionUtils.isEmpty(existedUsers)) {
            return;
        }

        List<String> userIds = new ArrayList<>();
        if (!CollectionUtils.isEmpty(this.meetingAttendees)) {
            userIds = this.meetingAttendees.stream().map(MeetingAttendee::getUserId).collect(Collectors.toList());
        }

        List<String> finalUserIds = userIds;
        this.users = existedUsers.stream().filter(u -> finalUserIds.contains(u.getId())).collect(Collectors.toList());
    }

    private List<BookedTimeEvent> createEventScheduler(MeetingCreateOrUpdateCmd cmd) {
        List<BookedTimeEvent> bookedTimeEvents = new ArrayList<>();
        LocalDateTime tmpStartAt = LocalDateTime.ofInstant(cmd.getStartAt(), ZoneOffset.UTC);
        LocalDateTime tmpFinishAt = LocalDateTime.ofInstant(cmd.getFinishAt(), ZoneOffset.UTC);
        bookedTimeEvents.add(new BookedTimeEvent(cmd.getStartAt(), cmd.getFinishAt()));
        Instant tmpEndDate = cmd.getEndDate().atStartOfDay().with(LocalTime.MAX).toInstant(ZoneOffset.UTC);
        Instant startAt = cmd.getStartAt();
        Instant finishAt;
        if (cmd.getRepeatType().equals(RepeatType.DAILY)) {
            while (startAt.compareTo(tmpEndDate) <= 0) {
                tmpStartAt = tmpStartAt.plusDays(cmd.getRepeatMeasure());
                tmpFinishAt = tmpFinishAt.plusDays(cmd.getRepeatMeasure());
                if (tmpFinishAt.toInstant(ZoneOffset.UTC).compareTo(tmpEndDate) > 0) {
                    return bookedTimeEvents;
                }
                startAt = tmpStartAt.toInstant(ZoneOffset.UTC);
                finishAt = tmpFinishAt.toInstant(ZoneOffset.UTC);
                bookedTimeEvents.add(new BookedTimeEvent(startAt, finishAt));
            }
        } else if (cmd.getRepeatType().equals(RepeatType.WEEKLY)) {
            while (startAt.compareTo(tmpEndDate) <= 0) {
                int i = 0;
                for (DayOfWeek daysOfWeek : cmd.getDayOfWeekList()) {
                    tmpStartAt = tmpStartAt.with(temporal -> temporal.with(DAY_OF_WEEK, daysOfWeek.getValue()));
                    tmpFinishAt = tmpFinishAt.with(temporal -> temporal.with(DAY_OF_WEEK, daysOfWeek.getValue()));
                    if (startAt.compareTo(tmpStartAt.toInstant(ZoneOffset.UTC)) >= 0) {
                        i++;
                        if (cmd.getDayOfWeekList().size() == i) {
                            tmpStartAt = tmpStartAt.plusWeeks(cmd.getRepeatMeasure());
                            tmpFinishAt = tmpFinishAt.plusWeeks(cmd.getRepeatMeasure());
                        }
                        continue;
                    }
                    if (tmpFinishAt.toInstant(ZoneOffset.UTC).compareTo(tmpEndDate) > 0) {
                        return bookedTimeEvents;
                    }
                    startAt = tmpStartAt.toInstant(ZoneOffset.UTC);
                    finishAt = tmpFinishAt.toInstant(ZoneOffset.UTC);
                    bookedTimeEvents.add(new BookedTimeEvent(startAt, finishAt));
                    i++;
                    if (cmd.getDayOfWeekList().size() == i) {
                        tmpStartAt = tmpStartAt.plusWeeks(cmd.getRepeatMeasure());
                        tmpFinishAt = tmpFinishAt.plusWeeks(cmd.getRepeatMeasure());
                    }
                }
            }
        } else if (cmd.getRepeatType().equals(RepeatType.MONTHLY)) {
            createEventSchedulerWithMonthly(tmpStartAt, tmpFinishAt, bookedTimeEvents, cmd, tmpEndDate);
        } else if (cmd.getRepeatType().equals(RepeatType.YEARLY)) {
            createEventSchedulerWithYearly(tmpStartAt, tmpFinishAt, bookedTimeEvents, cmd, tmpEndDate);
        }
        return bookedTimeEvents;
    }

    private TemporalAdjuster createTemporalAdjuster(MeetingCreateOrUpdateCmd cmd) {
        if (cmd.getDayOfMonth() != null) {
            return temporal -> temporal.with(DAY_OF_MONTH, cmd.getDayOfMonth());
        } else {
            return TemporalAdjusters.dayOfWeekInMonth(cmd.getWeekOfMonth().getValue(), DayOfWeek.of(cmd.getDayOfWeekList().get(0).getValue()));
        }
    }

    private void createEventSchedulerWithMonthly(LocalDateTime tmpStartAt, LocalDateTime tmpFinishAt, List<BookedTimeEvent> bookedTimeEvents, MeetingCreateOrUpdateCmd cmd, Instant endDate) {
        TemporalAdjuster temporalAdjuster = createTemporalAdjuster(cmd);
        Instant startAt = cmd.getStartAt();
        Instant finishAt;
        while (startAt.compareTo(endDate) <= 0) {
            tmpStartAt = tmpStartAt.with(temporalAdjuster);
            tmpFinishAt = tmpFinishAt.with(temporalAdjuster);
            if (tmpFinishAt.toInstant(ZoneOffset.UTC).compareTo(endDate) > 0) {
                return;
            }
            if (LocalDateTime.ofInstant(startAt, ZoneOffset.UTC).compareTo(tmpStartAt) > 0) {
                tmpStartAt = tmpStartAt.plusMonths(cmd.getRepeatMeasure());
                tmpFinishAt = tmpFinishAt.plusMonths(cmd.getRepeatMeasure());
                tmpStartAt = tmpStartAt.with(temporalAdjuster);
                tmpFinishAt = tmpFinishAt.with(temporalAdjuster);
            }
            startAt = tmpStartAt.toInstant(ZoneOffset.UTC);
            finishAt = tmpFinishAt.toInstant(ZoneOffset.UTC);
            bookedTimeEvents.add(new BookedTimeEvent(startAt, finishAt));
            tmpStartAt = tmpStartAt.plusMonths(cmd.getRepeatMeasure());
            tmpFinishAt = tmpFinishAt.plusMonths(cmd.getRepeatMeasure());
        }
    }

    private void createEventSchedulerWithYearly(LocalDateTime tmpStartAt, LocalDateTime tmpFinishAt, List<BookedTimeEvent> bookedTimeEvents, MeetingCreateOrUpdateCmd cmd, Instant endDate) {
        TemporalAdjuster temporalAdjuster = createTemporalAdjuster(cmd);
        Instant startAt = cmd.getStartAt();
        Instant finishAt;
        while (startAt.compareTo(endDate) <= 0) {
            tmpStartAt = tmpStartAt.with(temporal -> temporal.with(cmd.getMonthOfYear()));
            tmpFinishAt = tmpFinishAt.with(temporal -> temporal.with(cmd.getMonthOfYear()));
            tmpStartAt = tmpStartAt.with(temporalAdjuster);
            tmpFinishAt = tmpFinishAt.with(temporalAdjuster);
            if (tmpFinishAt.toInstant(ZoneOffset.UTC).compareTo(endDate) > 0) {
                return;
            }
            if (LocalDateTime.ofInstant(startAt, ZoneOffset.UTC).compareTo(tmpStartAt) > 0) {
                tmpStartAt = tmpStartAt.plusYears(cmd.getRepeatMeasure());
                tmpFinishAt = tmpFinishAt.plusYears(cmd.getRepeatMeasure());
                tmpStartAt = tmpStartAt.with(temporal -> temporal.with(cmd.getMonthOfYear()));
                tmpFinishAt = tmpFinishAt.with(temporal -> temporal.with(cmd.getMonthOfYear()));
                tmpStartAt = tmpStartAt.with(temporalAdjuster);
                tmpFinishAt = tmpFinishAt.with(temporalAdjuster);
            }
            startAt = tmpStartAt.toInstant(ZoneOffset.UTC);
            finishAt = tmpFinishAt.toInstant(ZoneOffset.UTC);
            bookedTimeEvents.add(new BookedTimeEvent(startAt, finishAt));
            tmpStartAt = tmpStartAt.plusYears(cmd.getRepeatMeasure());
            tmpFinishAt = tmpFinishAt.plusYears(cmd.getRepeatMeasure());
        }
    }
}
