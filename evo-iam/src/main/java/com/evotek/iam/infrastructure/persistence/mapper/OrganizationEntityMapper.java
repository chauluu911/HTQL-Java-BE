package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.Organization;
import com.evotek.iam.infrastructure.persistence.entity.OrganizationEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface OrganizationEntityMapper extends EntityMapper<Organization, OrganizationEntity> {
}
