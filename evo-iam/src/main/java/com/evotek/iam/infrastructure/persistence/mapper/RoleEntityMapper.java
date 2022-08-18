package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.Role;
import com.evotek.iam.infrastructure.persistence.entity.RoleEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface RoleEntityMapper extends EntityMapper<Role, RoleEntity> {
}
