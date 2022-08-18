package com.evotek.iam.infrastructure.persistence.mapper;

import com.evotek.common.mapper.EntityMapper;
import com.evotek.iam.domain.Department;
import com.evotek.iam.infrastructure.persistence.entity.DepartmentEntity;
import org.mapstruct.Mapper;


@Mapper(componentModel = "spring")
public interface DepartmentEntityMapper extends EntityMapper<Department, DepartmentEntity> {

}
