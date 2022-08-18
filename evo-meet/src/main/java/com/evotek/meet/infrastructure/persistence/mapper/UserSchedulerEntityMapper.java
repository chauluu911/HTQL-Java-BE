package com.evotek.meet.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.meet.domain.UserScheduler;
import com.evotek.meet.infrastructure.persistence.entity.UserSchedulerEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface UserSchedulerEntityMapper extends EntityMapper<UserScheduler, UserSchedulerEntity> {
}
