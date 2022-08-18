package com.evotek.notification.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.notification.domain.EventTarget;
import com.evotek.notification.infrastructure.persistence.entity.EventTargetEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface EventTargetEntityMapper extends EntityMapper<EventTarget, EventTargetEntity> {
}
