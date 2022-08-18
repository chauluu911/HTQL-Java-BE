package com.evotek.notification.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.notification.domain.EventFile;
import com.evotek.notification.infrastructure.persistence.entity.EventFileEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface EventFileEntityMapper extends EntityMapper<EventFile, EventFileEntity> {
}
