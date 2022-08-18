package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.UserGroup;
import com.evotek.iam.infrastructure.persistence.entity.UserGroupEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface UserGroupEntityMapper extends EntityMapper<UserGroup, UserGroupEntity> {
}