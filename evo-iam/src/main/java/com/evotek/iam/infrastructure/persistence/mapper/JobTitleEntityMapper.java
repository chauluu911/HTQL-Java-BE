package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.JobTitle;
import com.evotek.iam.infrastructure.persistence.entity.JobTitleEntity;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface JobTitleEntityMapper extends EntityMapper<JobTitle, JobTitleEntity> {

}
