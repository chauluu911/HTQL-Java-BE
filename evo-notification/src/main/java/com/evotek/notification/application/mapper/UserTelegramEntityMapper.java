package com.evotek.notification.application.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.notification.domain.UserTelegram;
import com.evotek.notification.infrastructure.persistence.entity.UserTelegramEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface UserTelegramEntityMapper extends EntityMapper<UserTelegram, UserTelegramEntity> {
}
