package com.evotek.meet.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.meet.domain.*;
import com.evotek.meet.infrastructure.persistence.entity.*;
import com.evotek.meet.infrastructure.persistence.mapper.*;
import com.evotek.meet.domain.repository.MeetingDomainRepository;
import com.evotek.meet.infrastructure.persistence.repository.*;
import com.evotek.meet.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@Slf4j
public class MeetingDomainRepositoryImpl extends AbstractDomainRepository<Meeting, MeetingEntity, String> implements MeetingDomainRepository {

    private final MeetingEntityMapper meetingEntityMapper;
    private final MeetingEntityRepository meetingEntityRepository;
    private final MeetingAttendeeEntityRepository meetingAttendeeEntityRepository;
    private final RoomSchedulerEntityRepository roomSchedulerEntityRepository;
    private final UserSchedulerEntityRepository userSchedulerEntityRepository;
    private final UserEntityRepository userEntityRepository;
    private final MeetingAttendeeEntityMapper meetingAttendeeEntityMapper;
    private final MeetingSchedulerEntityMapper meetingSchedulerEntityMapper;
    private final UserSchedulerEntityMapper userSchedulerEntityMapper;
    private final UserEntityMapper userEntityMapper;


    public MeetingDomainRepositoryImpl(MeetingEntityMapper meetingEntityMapper, MeetingEntityRepository meetingEntityRepository, MeetingAttendeeEntityRepository meetingAttendeeEntityRepositorytityRepository, RoomSchedulerEntityRepository roomSchedulerEntityRepository, UserSchedulerEntityRepository userSchedulerEntityRepository, UserEntityRepository userEntityRepository, MeetingAttendeeEntityMapper meetingAttendeeEntityMapper, MeetingSchedulerEntityMapper meetingSchedulerEntityMapper, UserSchedulerEntityMapper userSchedulerEntityMapper, UserEntityMapper userEntityMapper) {
        super(meetingEntityRepository, meetingEntityMapper);
        this.meetingEntityRepository = meetingEntityRepository;
        this.meetingEntityMapper = meetingEntityMapper;
        this.meetingAttendeeEntityRepository = meetingAttendeeEntityRepositorytityRepository;
        this.roomSchedulerEntityRepository = roomSchedulerEntityRepository;
        this.userSchedulerEntityRepository = userSchedulerEntityRepository;
        this.userEntityRepository = userEntityRepository;
        this.meetingAttendeeEntityMapper = meetingAttendeeEntityMapper;
        this.meetingSchedulerEntityMapper = meetingSchedulerEntityMapper;
        this.userSchedulerEntityMapper = userSchedulerEntityMapper;
        this.userEntityMapper = userEntityMapper;
    }

    @Override
    public Meeting getById(String id) {
        return this.findById(id).orElseThrow(() ->
                new ResponseException(NotFoundError.MEETING_NOT_FOUND));
    }

    @Override
    @Transactional
    public Meeting save(Meeting meeting) {
        MeetingEntity meetingEntity = this.meetingEntityMapper.toEntity(meeting);
        this.meetingEntityRepository.save(meetingEntity);

        // save meetingAttendee
        List<MeetingAttendee> meetingAttendees = meeting.getMeetingAttendees();
        if (!CollectionUtils.isEmpty(meetingAttendees)) {
            List<MeetingAttendeeEntity> meetingAttendeeEntities = meetingAttendeeEntityMapper.toEntity(meetingAttendees);
            this.meetingAttendeeEntityRepository.saveAll(meetingAttendeeEntities);
        }

        if (Objects.nonNull(meeting.getRoom())) {
            // save RoomScheduler
            List<RoomScheduler> roomSchedulers = meeting.getRoom().getRoomSchedulers();
            if (!CollectionUtils.isEmpty(roomSchedulers)) {
                List<RoomSchedulerEntity> roomSchedulerEntities = meetingSchedulerEntityMapper.toEntity(roomSchedulers);
                this.roomSchedulerEntityRepository.saveAll(roomSchedulerEntities);
            }
        }

        //save userScheduler
        if (!CollectionUtils.isEmpty(meeting.getUsers())) {
            List<UserScheduler> userSchedulerList = meeting.getUsers().stream()
                    .flatMap(u -> u.getUserSchedulers().stream()).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(userSchedulerList)) {
                List<UserSchedulerEntity> userSchedulerEntities = userSchedulerEntityMapper.toEntity(userSchedulerList);
                this.userSchedulerEntityRepository.saveAll(userSchedulerEntities);
            }
        }

        //save user
        if (!CollectionUtils.isEmpty(meeting.getUsers())) {
            List<User> userList = meeting.getUsers();
            if (!CollectionUtils.isEmpty(userList)) {
                List<UserEntity> userEntities = userEntityMapper.toEntity(userList);
                this.userEntityRepository.saveAll(userEntities);
            }
        }
        return meeting;
    }
}
