package com.evotek.meet.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.meet.domain.User;
import com.evotek.meet.infrastructure.persistence.entity.UserEntity;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface UserEntityMapper extends EntityMapper<User, UserEntity> {
}
