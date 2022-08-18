package com.evotek.iam.infrastructure.persistence.repository.custom;

import com.evotek.iam.domain.query.UserGroupSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.UserGroupEntity;

import java.util.List;

public interface GroupRepositoryCustom {
    Long countGroup(UserGroupSearchQuery query);
    List<UserGroupEntity> search(UserGroupSearchQuery query);
}
