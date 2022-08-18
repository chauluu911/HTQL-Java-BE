package com.evotek.meet.application.service.impl;

import com.evotek.common.client.iam.IAMClient;
import com.evotek.common.dto.PageDTO;
import com.evotek.common.dto.request.FindByIdsRequest;
import com.evotek.common.dto.response.Response;
import com.evotek.common.dto.response.iam.UserDTO;
import com.evotek.common.exception.ResponseException;
import com.evotek.common.mapper.util.PageableMapperUtil;
import com.evotek.common.persistence.support.SqlUtils;
import com.evotek.common.util.StringPool;
import com.evotek.common.webapp.support.AbstractDomainService;
import com.evotek.meet.application.dto.request.RoomCreateRequest;
import com.evotek.meet.application.dto.request.RoomSchedulerRequest;
import com.evotek.meet.application.dto.request.RoomSearchRequest;
import com.evotek.meet.application.dto.request.RoomUpdateRequest;
import com.evotek.meet.application.dto.response.MeetingAttendeeResponse;
import com.evotek.meet.application.dto.response.RoomSchedulerResponse;
import com.evotek.meet.application.dto.response.UserSchedulerResponse;
import com.evotek.meet.application.mapper.*;
import com.evotek.meet.application.service.RoomService;
import com.evotek.meet.domain.Meeting;
import com.evotek.meet.domain.MeetingAttendee;
import com.evotek.meet.domain.Room;
import com.evotek.meet.domain.RoomScheduler;
import com.evotek.meet.domain.command.RoomCreateCmd;
import com.evotek.meet.domain.command.RoomUpdateCmd;
import com.evotek.meet.domain.repository.RoomDomainRepository;
import com.evotek.meet.infrastructure.persistence.entity.MeetingEntity;
import com.evotek.meet.infrastructure.persistence.entity.RoomEntity;
import com.evotek.meet.infrastructure.persistence.entity.RoomSchedulerEntity;
import com.evotek.meet.domain.query.RoomSearchQuery;
import com.evotek.meet.infrastructure.persistence.mapper.MeetingAttendeeEntityMapper;
import com.evotek.meet.infrastructure.persistence.mapper.MeetingEntityMapper;
import com.evotek.meet.infrastructure.persistence.mapper.RoomEntityMapper;
import com.evotek.meet.infrastructure.persistence.mapper.RoomSchedulerEntityMapper;
import com.evotek.meet.infrastructure.persistence.repository.MeetingAttendeeEntityRepository;
import com.evotek.meet.infrastructure.persistence.repository.MeetingEntityRepository;
import com.evotek.meet.infrastructure.persistence.repository.RoomEntityRepository;
import com.evotek.meet.infrastructure.persistence.repository.RoomSchedulerEntityRepository;
import com.evotek.meet.infrastructure.support.exception.BadRequestError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Slf4j
public class RoomServiceImpl extends AbstractDomainService<Room, RoomEntity, String> implements RoomService {

    private final RoomEntityRepository roomEntityRepository;
    private final RoomSchedulerEntityRepository roomSchedulerEntityRepository;
    private final RoomSchedulerEntityMapper roomSchedulerEntityMapper;
    private final AutoMapper autoMapper;
    private final RoomEntityMapper roomEntityMapper;
    private final MeetingEntityRepository meetingEntityRepository;
    private final AutoQueryMapper autoQueryMapper;
    private final MeetingEntityMapper meetingEntityMapper;
    private final RoomDomainRepository roomDomainRepository;
    private final MeetingAttendeeEntityRepository meetingAttendeeEntityRepository;
    private final MeetingAttendeeEntityMapper meetingAttendeeEntityMapper;
    private final IAMClient iamClient;

    public RoomServiceImpl(RoomEntityRepository roomEntityRepository,
                           RoomSchedulerEntityRepository roomSchedulerEntityRepository,
                           RoomSchedulerEntityMapper roomSchedulerEntityMapper,
                           RoomEntityMapper roomEntityMapper,
                           AutoMapper autoMapper,
                           MeetingEntityRepository meetingEntityRepository,
                           MeetingEntityMapper meetingEntityMapper,
                           AutoQueryMapper autoQueryMapper,
                           RoomDomainRepository roomDomainRepository,
                           MeetingAttendeeEntityRepository meetingAttendeeEntityRepository,
                           MeetingAttendeeEntityMapper meetingAttendeeEntityMapper, IAMClient iamClient) {
        super(roomEntityRepository, roomEntityMapper);
        this.roomEntityRepository = roomEntityRepository;
        this.roomSchedulerEntityRepository = roomSchedulerEntityRepository;
        this.roomSchedulerEntityMapper = roomSchedulerEntityMapper;
        this.autoMapper = autoMapper;
        this.roomEntityMapper = roomEntityMapper;
        this.meetingEntityRepository = meetingEntityRepository;
        this.meetingEntityMapper = meetingEntityMapper;
        this.autoQueryMapper = autoQueryMapper;
        this.roomDomainRepository = roomDomainRepository;
        this.meetingAttendeeEntityRepository = meetingAttendeeEntityRepository;
        this.meetingAttendeeEntityMapper = meetingAttendeeEntityMapper;
        this.iamClient = iamClient;
    }

    @Override
    @Transactional
    public Room create(RoomCreateRequest request) {
        Optional<RoomEntity> roomEntityOptional = roomEntityRepository.findByCode(request.getCode());

        if (roomEntityOptional.isPresent()) {
            throw new ResponseException(BadRequestError.ROOM_CODE_EXISTED);
        }

        RoomCreateCmd cmd = autoMapper.from(request);
        Room room = new Room(cmd);
        this.roomDomainRepository.save(room);

        return room;
    }

    @Override
    @Transactional
    public Room update(String id, RoomUpdateRequest request) {
        if (Objects.isNull(request)) {
            throw new ResponseException(String.format(BadRequestError.INVALID_INPUT.getMessage(), "meeting"),
                    BadRequestError.INVALID_INPUT);
        }
        Room room = ensureExisted(id);
        RoomUpdateCmd cmd = autoMapper.from(request);
        room.update(cmd);
        this.roomDomainRepository.save(room);

        return room;
    }

    @Override
    @Transactional
    public void active(String roomId) {
        Room room = ensureExisted(roomId);
        room.active();
        this.roomDomainRepository.save(room);
    }

    @Override
    @Transactional
    public void inactive(String roomId) {
        Room room = ensureExisted(roomId);
        room.inactive();
        this.roomDomainRepository.save(room);
    }

    @Override
    public Room ensureExisted(String id) {
        if (Objects.isNull(id)) {
            throw new ResponseException(String.format(BadRequestError.INVALID_INPUT.getMessage(), "id"),
                    BadRequestError.INVALID_INPUT);
        }
        return this.roomDomainRepository.getById(id);
    }

    @Override
    public PageDTO<Room> searchRoomByRequest(RoomSearchRequest searchRequest) {
        List<Room> rooms = this.roomEntityMapper.toDomain(this.roomEntityRepository.search(searchRequest));
        List<String> roomIds = rooms.stream().map(Room::getId).collect(Collectors.toList());

        log.info("Get Room Scheduler by roomIds {}", roomIds);
        List<RoomScheduler> roomSchedulers = this.roomSchedulerEntityMapper.toDomain(
                this.roomSchedulerEntityRepository.findAllByListRoomId(roomIds));

        log.info("Enrich room scheduler to room and check active");
        rooms.forEach(room -> {
            List<RoomScheduler> schedulerOfRooms = roomSchedulers.stream()
                    .filter(item -> Objects.equals(item.getRoomId(), room.getId()))
                    .collect(Collectors.toList());
            room.enrichRoomSchedulers(schedulerOfRooms);
        });

        return PageDTO.of(rooms, searchRequest.getPageIndex(),
                searchRequest.getPageSize(), this.roomEntityRepository.count(searchRequest));
    }

    @Override
    public PageDTO<Room> autoComplete(RoomSearchRequest searchRequest) {
        RoomSearchQuery query = autoQueryMapper.from(searchRequest);
        Pageable pageable = PageableMapperUtil.toPageable(searchRequest);
        List<Room> rooms = this.roomEntityMapper.toDomain(this.roomEntityRepository.autoComplete(SqlUtils.encodeKeyword(query.getKeyword()), query.getStatus(), pageable));
        return PageDTO.of(rooms, searchRequest.getPageIndex(),
                searchRequest.getPageSize(), this.roomEntityRepository.count(searchRequest));
    }

    @Override
    public Room findByRoomId(String roomId) {
        return ensureExisted(roomId);
    }

    @Override
    public List<RoomSchedulerResponse> findSchedulerByRequest(String roomId, RoomSchedulerRequest request) {
        List<RoomSchedulerEntity> roomSchedulerEntities = roomSchedulerEntityRepository.findByStartAtAndFinishAt(roomId,
                request.getStartAt(), request.getFinishAt());
        List<RoomScheduler> roomSchedulers = roomSchedulerEntityMapper.toDomain(roomSchedulerEntities);
        List<String> meetingIds = roomSchedulers.stream().map(RoomScheduler::getMeetingId).distinct().collect(Collectors.toList());
        List<MeetingEntity> meetingEntities = this.meetingEntityRepository.findByIds(meetingIds);
        List<Meeting> meetings = this.meetingEntityMapper.toDomain(meetingEntities);
        List<RoomSchedulerResponse> roomSchedulerResponses = this.autoMapper.map(roomSchedulers);
        List<MeetingAttendee> meetingAttendees = meetingAttendeeEntityMapper.toDomain(
                meetingAttendeeEntityRepository.findByListMeetingIds(meetingIds));
        List<String> userIds = meetingAttendees.stream().map(MeetingAttendee::getUserId).distinct().collect(Collectors.toList());
        Response<List<UserDTO>> responseUsers = this.iamClient.findByUserIds(new FindByIdsRequest(userIds));

        for (Meeting meeting : meetings) {
            String organizerName = StringPool.BLANK;

            for (MeetingAttendee meetingAttendee: meetingAttendees) {
                if (Objects.equals(meetingAttendee.getMeetingId(), meeting.getId())) {
                    Optional<UserDTO> userDTOOptional = responseUsers.getData().stream()
                            .filter(item -> Objects.equals(item.getId(), meetingAttendee.getUserId()))
                            .findFirst();
                    if (userDTOOptional.isPresent()) {
                        UserDTO userDTO = userDTOOptional.get();
                        if (meeting.getOrganizerId().equals(userDTO.getId())) {
                            organizerName = userDTO.getFullName();
                        }
                    }
                }
            }

            for (RoomSchedulerResponse roomSchedulerResponse : roomSchedulerResponses) {
                if (roomSchedulerResponse.getMeetingId().equals(meeting.getId())) {
                    roomSchedulerResponse.setTitle(meeting.getTitle());
                    roomSchedulerResponse.setOrganizerId(meeting.getOrganizerId());
                    roomSchedulerResponse.setOrganizerName(organizerName);
                }
            }
        }

        return roomSchedulerResponses;
    }
}
