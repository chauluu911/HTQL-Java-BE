package com.evotek.notification.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.notification.domain.Notification;
import com.evotek.notification.infrastructure.persistence.entity.NotificationEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface NotificationEntityMapper extends EntityMapper<Notification, NotificationEntity> {
}
