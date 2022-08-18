package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.RolePermission;
import com.evotek.iam.infrastructure.persistence.entity.RolePermissionEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface RolePermissionEntityMapper extends EntityMapper<RolePermission, RolePermissionEntity> {
}
