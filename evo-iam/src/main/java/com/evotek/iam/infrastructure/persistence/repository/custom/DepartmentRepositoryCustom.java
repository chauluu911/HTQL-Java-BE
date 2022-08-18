package com.evotek.iam.infrastructure.persistence.repository.custom;

import com.evotek.iam.domain.query.DepartmentSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.DepartmentEntity;

import java.util.List;

public interface DepartmentRepositoryCustom {
    List<DepartmentEntity> search(DepartmentSearchQuery request);

    Long count(DepartmentSearchQuery request);

    List<DepartmentEntity> getChildDepartmentsByParentIds(List<String> parentIds);
}
