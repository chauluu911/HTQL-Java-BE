package com.evotek.meet.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.meet.domain.MeetingAttendee;
import com.evotek.meet.infrastructure.persistence.entity.MeetingAttendeeEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface MeetingAttendeeEntityMapper extends EntityMapper<MeetingAttendee, MeetingAttendeeEntity> {
}
