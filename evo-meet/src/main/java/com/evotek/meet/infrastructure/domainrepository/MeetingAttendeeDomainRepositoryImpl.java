package com.evotek.meet.infrastructure.domainrepository;

import com.evotek.common.exception.ResponseException;
import com.evotek.common.webapp.support.AbstractDomainRepository;
import com.evotek.meet.domain.MeetingAttendee;
import com.evotek.meet.domain.Room;
import com.evotek.meet.domain.repository.MeetingAttendeeDomainRepository;
import com.evotek.meet.infrastructure.persistence.entity.MeetingAttendeeEntity;
import com.evotek.meet.infrastructure.persistence.mapper.MeetingAttendeeEntityMapper;
import com.evotek.meet.infrastructure.persistence.mapper.RoomEntityMapper;
import com.evotek.meet.infrastructure.persistence.repository.MeetingAttendeeEntityRepository;
import com.evotek.meet.infrastructure.persistence.repository.RoomEntityRepository;
import com.evotek.meet.infrastructure.support.exception.NotFoundError;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class MeetingAttendeeDomainRepositoryImpl extends AbstractDomainRepository<MeetingAttendee, MeetingAttendeeEntity, String> implements MeetingAttendeeDomainRepository {

    private final MeetingAttendeeEntityMapper meetingAttendeeEntityMapper;
    private final MeetingAttendeeEntityRepository meetingAttendeeEntityRepository;

    public MeetingAttendeeDomainRepositoryImpl(MeetingAttendeeEntityMapper meetingAttendeeEntityMapper, MeetingAttendeeEntityRepository meetingAttendeeEntityRepository) {
        super(meetingAttendeeEntityRepository, meetingAttendeeEntityMapper);
        this.meetingAttendeeEntityMapper = meetingAttendeeEntityMapper;
        this.meetingAttendeeEntityRepository = meetingAttendeeEntityRepository;
    }

    @Override
    public MeetingAttendee getById(String id) {
        return this.findById(id).orElseThrow(() ->
                new ResponseException(NotFoundError.ROOM_NOT_FOUND));
    }
}
