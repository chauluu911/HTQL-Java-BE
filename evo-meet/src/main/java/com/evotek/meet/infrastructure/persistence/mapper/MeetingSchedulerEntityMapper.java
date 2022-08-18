package com.evotek.meet.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.meet.domain.RoomScheduler;
import com.evotek.meet.infrastructure.persistence.entity.RoomSchedulerEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface MeetingSchedulerEntityMapper extends EntityMapper<RoomScheduler, RoomSchedulerEntity> {
}
