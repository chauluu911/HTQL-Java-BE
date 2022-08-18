package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.EmploymentHistory;
import com.evotek.iam.infrastructure.persistence.entity.EmploymentHistoryEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface EmploymentHistoryEntityMapper extends EntityMapper<EmploymentHistory, EmploymentHistoryEntity> {
}
