package com.evotek.iam.infrastructure.persistence.repository.custom;

import com.evotek.iam.application.dto.request.RoleSearchRequest;
import com.evotek.iam.infrastructure.persistence.entity.RoleEntity;

import java.util.List;

public interface RoleRepositoryCustom {
    List<RoleEntity> search(RoleSearchRequest request);

    Long count(RoleSearchRequest request);
}
