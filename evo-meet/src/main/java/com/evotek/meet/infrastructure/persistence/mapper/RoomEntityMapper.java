package com.evotek.meet.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.meet.domain.Room;
import com.evotek.meet.infrastructure.persistence.entity.RoomEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface RoomEntityMapper extends EntityMapper<Room, RoomEntity> {
}
