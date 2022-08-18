package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.Permission;
import com.evotek.iam.infrastructure.persistence.entity.PermissionEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface PermissionEntityMapper extends EntityMapper<Permission, PermissionEntity> {
}
