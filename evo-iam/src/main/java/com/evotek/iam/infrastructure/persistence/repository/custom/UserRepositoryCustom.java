package com.evotek.iam.infrastructure.persistence.repository.custom;

import com.evotek.iam.domain.query.EmployeeSearchQuery;
import com.evotek.iam.domain.query.UserSearchQuery;
import com.evotek.iam.infrastructure.persistence.entity.UserEntity;

import java.util.List;

public interface UserRepositoryCustom {
    List<UserEntity> search(UserSearchQuery query);

    Long countUser(UserSearchQuery query);

    // Employee
    List<UserEntity> searchEmployees(EmployeeSearchQuery query);

    Long countEmployees(EmployeeSearchQuery query);
}
