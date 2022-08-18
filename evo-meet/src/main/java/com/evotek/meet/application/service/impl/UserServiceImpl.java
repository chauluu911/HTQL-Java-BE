package com.evotek.meet.application.service.impl;

import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.common.error.AuthenticationError;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.util.StringPool;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.common.webapp.support.SecurityUtils;
import com.evotek.meet.application.dto.request.UserSchedulerRequest;
import com.evotek.meet.application.dto.response.MeetingAttendeeResponse;
import com.evotek.meet.application.dto.response.UserSchedulerResponse;
import com.evotek.meet.application.mapper.AutoMapper;
import com.evotek.meet.application.service.UserService;
import com.evotek.meet.domain.*;
import com.evotek.meet.domain.repository.MeetingDomainRepository;
import com.evotek.meet.domain.repository.UserSchedulerDomainRepository;
import com.evotek.meet.infrastructure.persistence.entity.MeetingEntity;
import com.evotek.meet.infrastructure.persistence.entity.RoomEntity;
import com.evotek.meet.infrastructure.persistence.entity.UserSchedulerEntity;
import com.evotek.meet.infrastructure.persistence.mapper.MeetingAttendeeEntityMapper;
import com.evotek.meet.infrastructure.persistence.mapper.MeetingEntityMapper;
import com.evotek.meet.infrastructure.persistence.mapper.RoomEntityMapper;
import com.evotek.meet.infrastructure.persistence.mapper.UserSchedulerEntityMapper;
import com.evotek.meet.infrastructure.persistence.repository.MeetingAttendeeEntityRepository;
import com.evotek.meet.infrastructure.persistence.repository.MeetingEntityRepository;
import com.evotek.meet.infrastructure.persistence.repository.RoomEntityRepository;
import com.evotek.meet.infrastructure.persistence.repository.UserSchedulerEntityRepository;
import com.evotek.meet.infrastructure.support.enums.AttendeeType;
import com.evotek.meet.infrastructure.support.exception.BadRequestError;
import com.github.kagkarlsson.scheduler.Scheduler;
import com.github.kagkarlsson.scheduler.task.Task;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class UserServiceImpl extends AbstractDomainService<UserScheduler, UserSchedulerEntity, String> implements UserService {

    private final UserSchedulerEntityRepository userSchedulerEntityRepository;
    private final UserSchedulerEntityMapper userSchedulerEntityMapper;
    private final MeetingEntityRepository meetingEntityRepository;
    private final MeetingDomainRepository meetingDomainRepository;
    private final MeetingEntityMapper meetingEntityMapper;
    private final MeetingAttendeeEntityMapper meetingAttendeeEntityMapper;
    private final MeetingAttendeeEntityRepository meetingAttendeeEntityRepository;
    private final UserSchedulerDomainRepository userSchedulerDomainRepository;
    private final Scheduler scheduler;
    private final AutoMapper autoMapper;
    private final Task<UserSchedulerRejectOrCancelTaskData> sendRejectOrCancelUserSchedulerTask;
    private final IAMClient iamClient;
    private final RoomEntityRepository roomEntityRepository;
    private final RoomEntityMapper roomEntityMapper;

    public UserServiceImpl(
            UserSchedulerEntityRepository userSchedulerEntityRepository,
            UserSchedulerEntityMapper userSchedulerEntityMapper, MeetingEntityRepository meetingEntityRepository, MeetingDomainRepository meetingDomainRepository, MeetingEntityMapper meetingEntityMapper, MeetingAttendeeEntityMapper meetingAttendeeEntityMapper, MeetingAttendeeEntityRepository meetingAttendeeEntityRepository, UserSchedulerDomainRepository userSchedulerDomainRepository, Scheduler scheduler, AutoMapper autoMapper, Task<UserSchedulerRejectOrCancelTaskData> sendRejectOrCancelUserSchedulerTask, IAMClient iamClient, RoomEntityRepository roomEntityRepository, RoomEntityMapper roomEntityMapper) {
        super(userSchedulerEntityRepository, userSchedulerEntityMapper);
        this.userSchedulerEntityRepository = userSchedulerEntityRepository;
        this.userSchedulerEntityMapper = userSchedulerEntityMapper;
        this.meetingEntityRepository = meetingEntityRepository;
        this.meetingDomainRepository = meetingDomainRepository;
        this.meetingEntityMapper = meetingEntityMapper;
        this.meetingAttendeeEntityMapper = meetingAttendeeEntityMapper;
        this.meetingAttendeeEntityRepository = meetingAttendeeEntityRepository;
        this.userSchedulerDomainRepository = userSchedulerDomainRepository;
        this.scheduler = scheduler;
        this.autoMapper = autoMapper;
        this.sendRejectOrCancelUserSchedulerTask = sendRejectOrCancelUserSchedulerTask;
        this.iamClient = iamClient;
        this.roomEntityRepository = roomEntityRepository;
        this.roomEntityMapper = roomEntityMapper;
    }

    @Override
    public List<UserSchedulerResponse> getScheduler(UserSchedulerRequest request) {
        List<UserSchedulerEntity> userSchedulerEntities = userSchedulerEntityRepository
                .findByStartAtAndFinishAt(getCurrentUserId(), request.getStartAt(), request.getFinishAt());
        List<UserScheduler> userSchedulers = userSchedulerEntityMapper.toDomain(userSchedulerEntities);
        List<UserSchedulerResponse> userSchedulerResponses = this.autoMapper.from(userSchedulers);

        List<String> meetingIds = userSchedulers.stream().map(UserScheduler::getMeetingId).distinct()
                .collect(Collectors.toList());
        List<MeetingEntity> meetingEntities = this.meetingEntityRepository.findByIds(meetingIds);
        List<Meeting> meetings = this.meetingEntityMapper.toDomain(meetingEntities);

        List<String> roomIds = userSchedulers.stream().filter(item -> Objects.nonNull(item.getRoomId()))
                .map(UserScheduler::getRoomId).distinct().collect(Collectors.toList());
        List<RoomEntity> roomEntities = this.roomEntityRepository.findByRoomIds(roomIds);
        List<Room> rooms = this.roomEntityMapper.toDomain(roomEntities);

        List<MeetingAttendee> meetingAttendees = meetingAttendeeEntityMapper.toDomain(
                meetingAttendeeEntityRepository.findByListMeetingIds(meetingIds));
        List<String> userIds = meetingAttendees.stream().map(MeetingAttendee::getUserId).distinct().collect(Collectors.toList());
        Response<List<UserDTO>> responseUsers = this.iamClient.findByUserIds(new FindByIdsRequest(userIds));

        for (UserSchedulerResponse userSchedulerResponse : userSchedulerResponses) {
            if (Objects.nonNull(userSchedulerResponse.getRoomId())){
                for (Room room: rooms) {
                    if (userSchedulerResponse.getRoomId().equals(room.getId())){
                        userSchedulerResponse.setRoom(room);
                    }
                }
            }
        }

        for (Meeting meeting : meetings) {
            String organizerName = StringPool.BLANK;
            List<MeetingAttendeeResponse> meetingAttendeeResponses = new ArrayList<>();

            for (MeetingAttendee meetingAttendee: meetingAttendees) {
                if (Objects.equals(meetingAttendee.getMeetingId(), meeting.getId())) {
                    Optional<UserDTO> userDTOOptional = responseUsers.getData().stream()
                            .filter(item -> Objects.equals(item.getId(), meetingAttendee.getUserId()))
                            .findFirst();
                    if (userDTOOptional.isPresent()) {
                        UserDTO userDTO = userDTOOptional.get();
                        MeetingAttendeeResponse meetingAttendeeResponse = MeetingAttendeeResponse.builder()
                                .attendeeType(meetingAttendee.getAttendeeType())
                                .userId(userDTO.getId())
                                .employeeCode(userDTO.getEmployeeCode())
                                .fullName(userDTO.getFullName()).build();
                        meetingAttendeeResponses.add(meetingAttendeeResponse);

                        if (meeting.getOrganizerId().equals(userDTO.getId())) {
                            organizerName = userDTO.getFullName();
                        }
                    }
                }
            }

            for (UserSchedulerResponse userSchedulerResponse : userSchedulerResponses) {
                if (userSchedulerResponse.getMeetingId().equals(meeting.getId())) {
                    userSchedulerResponse.setMeetingType(meeting.getMeetingType());
                    userSchedulerResponse.setTitle(meeting.getTitle());
                    userSchedulerResponse.setOrganizerName(organizerName);
                    userSchedulerResponse.setMeetingAttendees(meetingAttendeeResponses);
                }
            }
        }

        return userSchedulerResponses;
    }

    @Override
    public UserScheduler ensureExisted(String id) {
        if (Objects.isNull(id)) {
            throw new ResponseException(String.format(BadRequestError.INVALID_INPUT.getMessage(), "id"),
                    BadRequestError.INVALID_INPUT);
        }

        return this.userSchedulerDomainRepository.getById(id);
    }

    @Override
    public void reject(String userSchedulerId) {
        UserScheduler userScheduler = ensureExisted(userSchedulerId);
        Meeting meeting = this.meetingDomainRepository.getById(userScheduler.getMeetingId());
        String currentUserId = getCurrentUserId();
        Boolean isOrganizer = true;
        Boolean isReject = true;
        if(currentUserId.equals(meeting.getOrganizerId())){
            List<MeetingAttendee> meetingAttendees = meetingAttendeeEntityMapper.toDomain(
                    meetingAttendeeEntityRepository.findByMeetingId(meeting.getId()));
            List<String> userIds = meetingAttendees.stream().map(MeetingAttendee::getUserId).collect(Collectors.toList());
            List<UserScheduler> userSchedulers = userSchedulerEntityMapper.toDomain(
                    userSchedulerEntityRepository.findAllByUserIdIn(userIds));
            for (String userId: userIds) {
                List<UserScheduler> userSchedulerList = new ArrayList<>();
                for(UserScheduler scheduler: userSchedulers){
                    if(scheduler.getUserId().equals(userId) && userScheduler.getStartAt().compareTo(scheduler.getStartAt()) == 0){
                        userSchedulerList.add(scheduler);
                    }
                }
                User user = User.builder().id(userId).userSchedulers(userSchedulerList).build();
                user.rejectScheduler(userSchedulerList.get(0).getId());
                this.userSchedulerDomainRepository.saveAll(user.getUserSchedulers());
                isOrganizer = true;
            }
        } else {
            List<UserScheduler> userSchedulers = userSchedulerEntityMapper.toDomain(
                    userSchedulerEntityRepository.findByUserId(currentUserId));
            User user = User.builder().id(getCurrentUserId()).userSchedulers(userSchedulers).build();
            user.rejectScheduler(userSchedulerId);
            this.userSchedulerDomainRepository.saveAll(user.getUserSchedulers());
            isOrganizer = false;
        }

        UserSchedulerRejectOrCancelTaskData data = UserSchedulerRejectOrCancelTaskData.builder()
                .currentUserId(currentUserId)
                .userSchedulerId(userSchedulerId)
                .meetingId(meeting.getId())
                .isOrganizer(isOrganizer)
                .isReject(isReject)
                .build();
        scheduler.schedule(this.sendRejectOrCancelUserSchedulerTask.instance(UUID.randomUUID().toString(), data),
                Instant.now());
    }

    private String getCurrentUserId() {
        Optional<String> currentUserLoginId = SecurityUtils.getCurrentUserLoginId();
        if (currentUserLoginId.isEmpty()) {
            throw new ResponseException(AuthenticationError.UNAUTHORISED);
        }
        return currentUserLoginId.get();
    }
}
