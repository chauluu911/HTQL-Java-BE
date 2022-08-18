package com.evotek.meet.application.service.impl;

import com.evotek.common.dto.response.system.ConfigurationDTO;
import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.client.system.SystemClient;
import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.common.enums.ConfigurationType;
import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.meet.application.dto.request.MeetingCreateOrUpdateRequest;
import com.evotek.meet.application.dto.request.MeetingSearchRequest;
import com.evotek.meet.application.mapper.*;
import com.evotek.meet.application.service.MeetingService;
import com.evotek.meet.domain.*;
import com.evotek.meet.domain.command.MeetingCreateOrUpdateCmd;
import com.evotek.meet.domain.repository.MeetingDomainRepository;
import com.evotek.meet.domain.repository.RoomDomainRepository;
import com.evotek.meet.domain.repository.UserDomainRepository;
import com.evotek.meet.infrastructure.persistence.entity.*;
import com.evotek.meet.domain.query.MeetingSearchQuery;
import com.evotek.meet.infrastructure.persistence.mapper.*;
import com.evotek.meet.infrastructure.persistence.repository.*;
import com.evotek.meet.infrastructure.support.enums.MeetingType;
import com.evotek.meet.infrastructure.support.exception.BadRequestError;
import com.github.kagkarlsson.scheduler.Scheduler;
import com.github.kagkarlsson.scheduler.task.Task;
import lombok.extern.slf4j.Slf4j;


import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class MeetingServiceImpl extends AbstractDomainService<Meeting, MeetingEntity, String> implements MeetingService {
    private final MeetingEntityRepository meetingEntityRepository;
    private final MeetingAttendeeEntityRepository meetingAttendeeEntityRepository;
    private final UserSchedulerEntityRepository userSchedulerEntityRepository;
    private final RoomSchedulerEntityRepository roomSchedulerEntityRepository;
    private final AutoMapper autoMapper;
    private final MeetingEntityMapper meetingEntityMapper;
    private final MeetingAttendeeEntityMapper meetingAttendeeEntityMapper;
    private final UserSchedulerEntityMapper userSchedulerEntityMapper;
    private final RoomSchedulerEntityMapper roomSchedulerEntityMapper;
    private final UserEntityMapper userEntityMapper;

    private final UserEntityRepository userEntityRepository;
    private final IAMClient iamClient;
    private final SystemClient systemClient;
    private final AutoQueryMapper autoQueryMapper;
    private final Scheduler scheduler;
    private final Task<String> sendMailTask;
    private final Task<String> sendUserSchedulerTask;
    private final Task<UserSchedulerApproveTaskData> sendApproveUserSchedulerTask;
    private final MeetingDomainRepository meetingDomainRepository;
    private final RoomDomainRepository roomDomainRepository;
    private final UserDomainRepository userDomainRepository;
    private final Task<String> sendNotificationTask;
    private final Task<UserSchedulerRejectOrCancelTaskData> sendRejectOrCancelUserSchedulerTask;

    public MeetingServiceImpl(MeetingEntityRepository meetingEntityRepository,
                              MeetingAttendeeEntityRepository meetingAttendeeEntityRepository,
                              UserSchedulerEntityRepository userSchedulerEntityRepository,
                              RoomSchedulerEntityRepository roomSchedulerEntityRepository,
                              AutoMapper autoMapper,
                              MeetingEntityMapper meetingEntityMapper,
                              MeetingAttendeeEntityMapper meetingAttendeeEntityMapper,
                              UserEntityMapper userEntityMapper, UserEntityRepository userEntityRepository, IAMClient iamClient,
                              UserSchedulerEntityMapper userSchedulerEntityMapper,
                              RoomSchedulerEntityMapper roomSchedulerEntityMapper, SystemClient systemClient, AutoQueryMapper autoQueryMapper, Scheduler scheduler, Task<String> sendMailTask, Task<String> sendUserSchedulerTask, Task<UserSchedulerApproveTaskData> sendApproveUserSchedulerTask, MeetingDomainRepository meetingDomainRepository, RoomDomainRepository roomDomainRepository, UserDomainRepository userDomainRepository, Task<String> sendNotificationTask, Task<UserSchedulerRejectOrCancelTaskData> sendRejectOrCancelUserSchedulerTask) {
        super(meetingEntityRepository, meetingEntityMapper);
        this.meetingEntityRepository = meetingEntityRepository;
        this.meetingAttendeeEntityRepository = meetingAttendeeEntityRepository;
        this.userSchedulerEntityRepository = userSchedulerEntityRepository;
        this.roomSchedulerEntityRepository = roomSchedulerEntityRepository;
        this.autoMapper = autoMapper;
        this.meetingEntityMapper = meetingEntityMapper;
        this.meetingAttendeeEntityMapper = meetingAttendeeEntityMapper;
        this.userEntityMapper = userEntityMapper;
        this.userEntityRepository = userEntityRepository;
        this.userSchedulerEntityMapper = userSchedulerEntityMapper;
        this.roomSchedulerEntityMapper = roomSchedulerEntityMapper;
        this.iamClient = iamClient;
        this.systemClient = systemClient;
        this.autoQueryMapper = autoQueryMapper;
        this.scheduler = scheduler;
        this.sendMailTask = sendMailTask;
        this.sendUserSchedulerTask = sendUserSchedulerTask;
        this.sendApproveUserSchedulerTask = sendApproveUserSchedulerTask;
        this.meetingDomainRepository = meetingDomainRepository;
        this.roomDomainRepository = roomDomainRepository;
        this.userDomainRepository = userDomainRepository;
        this.sendNotificationTask = sendNotificationTask;
        this.sendRejectOrCancelUserSchedulerTask = sendRejectOrCancelUserSchedulerTask;
    }

    @Override
    @Transactional
    public Meeting create(MeetingCreateOrUpdateRequest request) {
        MeetingCreateOrUpdateCmd cmd = autoMapper.from(request);
        List<String> requiredUserIds = cmd.getRequiredUserIds();
        String currentUserId = getCurrentUserId();
        if (Objects.nonNull(cmd.getPresiderId()) && !requiredUserIds.contains(cmd.getPresiderId())) {
            requiredUserIds.add(cmd.getPresiderId());
        }
        if (!requiredUserIds.contains(currentUserId)) {
            requiredUserIds.add(currentUserId);
        }
        cmd.setRequiredUserIds(requiredUserIds);
        Meeting meeting;

        List<User> existedMeetingUsers = getExistedUsers(cmd);
        if (request.getMeetingType().equals(MeetingType.ONLINE)) {
            meeting = new Meeting(cmd, currentUserId, existedMeetingUsers);
        } else {
            Room room = ensureAndEnrichRoomById(request.getRoomId());
            meeting = new Meeting(cmd, room, currentUserId, existedMeetingUsers);
        }
        this.meetingDomainRepository.save(meeting);
        sendMailAndTelegram(meeting);
        return meeting;
    }

    @Override
    @Transactional
    public Meeting update(String id, MeetingCreateOrUpdateRequest request) {
        if (Objects.isNull(request)) {
            throw new ResponseException(String.format(BadRequestError.INVALID_INPUT.getMessage(), "meeting"),
                    BadRequestError.INVALID_INPUT);
        }
        MeetingCreateOrUpdateCmd cmd = autoMapper.from(request);
        Meeting meeting = ensureExisted(id);
        String currentUserId = getCurrentUserId();
        if (!currentUserId.equals(meeting.getOrganizerId()) && Boolean.FALSE.equals(SecurityUtils.isAdmin())
                && !currentUserId.equals(meeting.getPresiderId())) {
            throw new ResponseException(AuthenticationError.UNAUTHORISED);
        }
        List<User> existedUsers = getExistedUsers(cmd);
        this.enrichAttendeesForMeeting(meeting);
        if (request.getMeetingType().equals(MeetingType.ONLINE)) {
            meeting.update(cmd, currentUserId, existedUsers);
        } else {
            Room room = ensureAndEnrichRoomById(request.getRoomId());
            meeting.update(cmd, room, currentUserId, existedUsers);
        }
        this.meetingDomainRepository.save(meeting);
        // send attachment
        sendMailAndTelegram(meeting);
        return meeting;
    }

    private void sendMailAndTelegram(Meeting meeting) {

        scheduler.schedule(this.sendMailTask.instance(UUID.randomUUID().toString(), meeting.getId()),
                Instant.now());
        scheduler.schedule(this.sendNotificationTask.instance(UUID.randomUUID().toString(), meeting.getId()),
                Instant.now());
        Response<ConfigurationDTO> response = this.systemClient.findByCode(ConfigurationType.TELEGRAM_USER_SCHEDULER);
        ConfigurationDTO configuration = response.getData();
        User user = meeting.getUsers().get(0);
        List<UserScheduler> userSchedulers = user.getUserSchedulers().stream().filter(item -> Objects.equals(item.getMeetingId(), meeting.getId())).collect(Collectors.toList());
        for (UserScheduler userScheduler : userSchedulers) {
            if (response.isSuccess() && Objects.nonNull(response.getData())) {
                Duration delayTime = Duration.between(Instant.now().plusSeconds(
                        Long.parseLong(configuration.getValue())), userScheduler.getStartAt());
                if (Math.abs(delayTime.toSeconds()) <= Long.parseLong(configuration.getValue()) && delayTime.toSeconds() <= 0) {
                    scheduler.schedule(this.sendUserSchedulerTask.instance(UUID.randomUUID().toString(), meeting.getId()),
                            Instant.now());
                } else if (Math.abs(delayTime.toSeconds()) > Long.parseLong(configuration.getValue()) && delayTime.toSeconds() < 0) {
                    continue;
                } else {
                    scheduler.schedule(this.sendUserSchedulerTask.instance(UUID.randomUUID().toString(), meeting.getId()),
                            Instant.now().plusSeconds(delayTime.toSeconds()));
                }
            }
        }
    }

    @Override
    @Transactional
    public void delete(String id) {
        Meeting meeting = ensureExisted(id);
        String currentUserId = getCurrentUserId();
        if (!currentUserId.equals(meeting.getOrganizerId()) || Boolean.FALSE.equals(SecurityUtils.isAdmin())) {
            throw new ResponseException(AuthenticationError.UNAUTHORISED);
        }
        meeting.delete();
        this.meetingDomainRepository.save(meeting);
    }

    @Override
    public void cancel(String id) {
        Meeting meeting = ensureExisted(id);
        String currentUserId = getCurrentUserId();
        String userId = "";
        boolean isCancel = true;
        boolean isOrganizer = true;
        if (!currentUserId.equals(meeting.getOrganizerId()) && Boolean.FALSE.equals(SecurityUtils.isAdmin())) {
            userId = currentUserId;
            isOrganizer = false;
        }
        this.enrichForMeeting(meeting);
        meeting.cancel(userId);
        Optional<User> user = meeting.getUsers().stream().filter(item -> Objects.equals(currentUserId, item.getId())).findFirst();
        List<UserScheduler> userSchedulers = new ArrayList<>();
        if (user.isPresent()) {
            userSchedulers = user.get().getUserSchedulers();
        }
        Instant minStartAt = Collections.min(userSchedulers.stream().map(UserScheduler::getStartAt).collect(Collectors.toList()));
        Optional<UserScheduler> minStartAtUserScheduler = userSchedulers.stream().filter(item -> item.getStartAt().compareTo(minStartAt) == 0).findFirst();
        String minStartAtUserSchedulerId = "";
        if (minStartAtUserScheduler.isPresent()) {
            minStartAtUserSchedulerId = minStartAtUserScheduler.get().getId();
        }
        this.meetingDomainRepository.save(meeting);
        UserSchedulerRejectOrCancelTaskData data = UserSchedulerRejectOrCancelTaskData.builder()
                .currentUserId(currentUserId)
                .userSchedulerId(minStartAtUserSchedulerId)
                .meetingId(meeting.getId())
                .isOrganizer(isOrganizer)
                .isCancel(isCancel)
                .build();
        scheduler.schedule(this.sendRejectOrCancelUserSchedulerTask.instance(UUID.randomUUID().toString(), data),
                Instant.now());
    }

    private List<User> getExistedUsers(MeetingCreateOrUpdateCmd cmd) {
        List<User> existedUsers = new ArrayList<>();
        List<String> userIds = new ArrayList<>();
        userIds.addAll(cmd.getRequiredUserIds());
        userIds.addAll(cmd.getOptionalUserIds());
        Response<List<UserDTO>> responseUsers = iamClient.findByUserIds(new FindByIdsRequest(userIds));
        List<UserEntity> existedUserAttendeeEntity = this.userEntityRepository.findAllByUserIdIn(userIds);
        List<User> existedUserAttendees = this.userEntityMapper.toDomain(existedUserAttendeeEntity);
        if (responseUsers.isSuccess() && Objects.nonNull(responseUsers.getData())) {
            List<UserScheduler> allUserSchedulers
                    = userSchedulerEntityMapper.toDomain(userSchedulerEntityRepository.findAllByUserIdIn(userIds));
            for (User existedUser : existedUserAttendees) {
                existedUser.enrichUserSchedulers(allUserSchedulers);
                existedUser.unDelete();
                existedUsers.add(existedUser);
            }
            List<String> existedUserIds = existedUsers.stream().map(User::getId).collect(Collectors.toList());
            for (UserDTO userDTO : responseUsers.getData()) {
                User user = autoMapper.from(userDTO);
                if (!existedUserIds.contains(user.getId())) {
                    user.enrichUserSchedulers(allUserSchedulers);
                    user.unDelete();
                    existedUsers.add(user);
                }
            }
        }
        return existedUsers;
    }

    @Override
    public Meeting ensureExisted(String id) {
        if (Objects.isNull(id)) {
            throw new ResponseException(String.format(BadRequestError.INVALID_INPUT.getMessage(), "id"),
                    BadRequestError.INVALID_INPUT);
        }
        return this.meetingDomainRepository.getById(id);
    }

    @Override
    public Meeting findByMeetingId(String id) {
        Meeting meeting = ensureExisted(id);
        this.enrichAttendeesForMeeting(meeting);
        String currentUserId = getCurrentUserId();
        for (MeetingAttendee meetingAttendee : meeting.getMeetingAttendees()) {
            if (meetingAttendee.getUserId().equals(currentUserId)) {
                meeting.enrichApproveStatus(meetingAttendee.getApproveStatus());
            }
        }
        return meeting;
    }

    @Override
    @Transactional
    public Meeting approve(String id) {
        String currentUserId = getCurrentUserId();
        Meeting meeting = ensureExisted(id);
        enrichCurrentAttendees(meeting, currentUserId);
        enrichCurrentUserScheduler(meeting, currentUserId);
        meeting.approve();
        this.meetingDomainRepository.save(meeting);
        UserSchedulerApproveTaskData data = UserSchedulerApproveTaskData.builder()
                .meetingId(meeting.getId())
                .currentUserId(currentUserId)
                .build();
        scheduler.schedule(this.sendApproveUserSchedulerTask.instance(UUID.randomUUID().toString(), data),
                Instant.now());
        return meeting;
    }


    @Override
    @Transactional
    public Meeting reject(String id) {
        String currentUserId = getCurrentUserId();
        Meeting meeting = ensureExisted(id);
        enrichCurrentAttendeesForMeeting(meeting, currentUserId);
        enrichCurrentUserSchedulerForMeeting(meeting, currentUserId);
        meeting.reject();
        this.meetingDomainRepository.save(meeting);
        return meeting;
    }

    private void enrichCurrentAttendeesForMeeting(Meeting meeting, String currentUserId) {
        List<MeetingAttendeeEntity> meetingAttendeeEntities
                = meetingAttendeeEntityRepository.findByMeetingIdAndUserId(meeting.getId(), currentUserId);
        if (!CollectionUtils.isEmpty(meetingAttendeeEntities)) {
            meeting.enrichMeetingAttendees(this.meetingAttendeeEntityMapper.toDomain(meetingAttendeeEntities));
        }
    }

    private void enrichCurrentAttendees(Meeting meeting, String currentUserId) {
        List<MeetingAttendeeEntity> meetingAttendeeEntities
                = meetingAttendeeEntityRepository.findByUserId(currentUserId);
        if (!CollectionUtils.isEmpty(meetingAttendeeEntities)) {
            meeting.enrichMeetingAttendees(this.meetingAttendeeEntityMapper.toDomain(meetingAttendeeEntities));
        }

    }

    @Override
    public PageDTO<Meeting> search(MeetingSearchRequest searchRequest) {
        MeetingSearchQuery query = autoQueryMapper.from(searchRequest);
        if (Boolean.FALSE.equals(SecurityUtils.isAdmin()) || Boolean.TRUE.equals(searchRequest.getIsSearchOfMe())) {
            query.setUserId(getCurrentUserId());
        }

        List<MeetingEntity> meetingEntities = this.meetingEntityRepository.search(query);

        List<String> meetingIds = meetingEntities.stream().map(MeetingEntity::getId).collect(Collectors.toList());
        List<MeetingAttendee> meetingAttendees = meetingAttendeeEntityMapper.toDomain(
                meetingAttendeeEntityRepository.findByListMeetingIds(meetingIds));
        List<Meeting> meetings = this.meetingEntityMapper.toDomain(meetingEntities);

        // enrich approve status for meeting with current user
        String currentUserId = getCurrentUserId();
        for (Meeting meeting : meetings) {
            for (MeetingAttendee meetingAttendee : meetingAttendees) {
                if (meeting.getId().equals(meetingAttendee.getMeetingId()) &&
                        meetingAttendee.getUserId().equals(currentUserId)) {
                    meeting.enrichApproveStatus(meetingAttendee.getApproveStatus());
                }
            }
        }

        return PageDTO.of(meetings, searchRequest.getPageIndex(), searchRequest.getPageSize(), this.meetingEntityRepository.count(query));
    }

    private String getCurrentUserId() {
        Optional<String> currentUserLoginId = SecurityUtils.getCurrentUserLoginId();
        if (currentUserLoginId.isEmpty()) {
            throw new ResponseException(AuthenticationError.UNAUTHORISED);
        }
        return currentUserLoginId.get();
    }

    private Room ensureAndEnrichRoomById(String roomId) {
        if (Objects.isNull(roomId)) {
            throw new ResponseException(String.format(BadRequestError.INVALID_INPUT.getMessage(), "id"),
                    BadRequestError.INVALID_INPUT);
        }
        Room room = this.roomDomainRepository.getById(roomId);
        List<RoomSchedulerEntity> roomSchedulerEntities = this.roomSchedulerEntityRepository.findAllByRoomId(roomId);
        List<RoomScheduler> roomSchedulers = this.roomSchedulerEntityMapper.toDomain(roomSchedulerEntities);
        room.enrichRoomSchedulers(roomSchedulers);
        return room;
    }


    private void enrichAttendeesForMeeting(Meeting meeting) {
        List<MeetingAttendeeEntity> meetingAttendeeEntities = meetingAttendeeEntityRepository.findByMeetingId(meeting.getId());
        if (!CollectionUtils.isEmpty(meetingAttendeeEntities)) {
            meeting.enrichMeetingAttendees(this.meetingAttendeeEntityMapper.toDomain(meetingAttendeeEntities));
        }
    }

    private void enrichCurrentUserScheduler(Meeting meeting, String currentUserId) {
        List<UserSchedulerEntity> userSchedulerEntities
                = userSchedulerEntityRepository.findByUserId(currentUserId);
        List<UserScheduler> userSchedulers = new ArrayList<>();
        if (!CollectionUtils.isEmpty(userSchedulerEntities)) {
            userSchedulers = this.userSchedulerEntityMapper.toDomain(userSchedulerEntities);
        }
        User user = this.userDomainRepository.getById(currentUserId);
        user.enrichUserSchedulers(userSchedulers);
        meeting.enrichUsers(List.of(user));
    }

    private void enrichCurrentUserSchedulerForMeeting(Meeting meeting, String currentUserId) {

        // enrich user schedulers
        List<UserSchedulerEntity> userSchedulerEntities
                = userSchedulerEntityRepository.findByMeetingIdAndCurrentId(meeting.getId(), currentUserId);
        List<UserScheduler> userSchedulers = new ArrayList<>();
        if (!CollectionUtils.isEmpty(userSchedulerEntities)) {
            userSchedulers = this.userSchedulerEntityMapper.toDomain(userSchedulerEntities);
        }
        User user = this.userDomainRepository.getById(currentUserId);
        user.enrichUserSchedulers(userSchedulers);
        meeting.enrichUsers(List.of(user));
    }

    private List<User> findAllUserByMeetingId(String meetingId) {
        List<UserSchedulerEntity> userSchedulerEntities = userSchedulerEntityRepository.findAllByMeetingId(meetingId);
        List<String> userIds = userSchedulerEntities.stream()
                .map(UserSchedulerEntity::getUserId).distinct().collect(Collectors.toList());

        List<UserScheduler> userSchedulers = this.userSchedulerEntityMapper.toDomain(userSchedulerEntities);
        List<User> users = new ArrayList<>();
        for (String userId : userIds) {
            List<UserScheduler> subUserSchedulers = userSchedulers.stream()
                    .filter(u -> Objects.equals(u.getUserId(), userId)).collect(Collectors.toList());
            User user = this.userDomainRepository.getById(userId);
            user.enrichUserSchedulers(subUserSchedulers);
            users.add(user);
        }
        return users;
    }

    private void enrichForMeeting(Meeting meeting) {
        // enrich meeting Attendees
        List<MeetingAttendeeEntity> meetingAttendeeEntities = meetingAttendeeEntityRepository.findByMeetingId(meeting.getId());
        if (!CollectionUtils.isEmpty(meetingAttendeeEntities)) {
            meeting.enrichMeetingAttendees(this.meetingAttendeeEntityMapper.toDomain(meetingAttendeeEntities));
        }
        if (!Objects.isNull(meeting.getRoomId())) {
            Room room = ensureAndEnrichRoomById(meeting.getRoomId());
            meeting.enrichRoom(room);
        }
        List<User> users = findAllUserByMeetingId(meeting.getId());
        meeting.enrichUsers(users);

    }
}
