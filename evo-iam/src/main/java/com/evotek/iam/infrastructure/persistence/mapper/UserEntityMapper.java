package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.User;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface UserEntityMapper extends EntityMapper<User, UserEntity> {
}
