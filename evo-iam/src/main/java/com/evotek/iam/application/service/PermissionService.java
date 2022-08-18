package com.evotek.iam.application.service;

import com.evotek.common.webapp.support.DomainService;
import com.evotek.iam.domain.Permission;

import java.util.List;

public interface PermissionService extends DomainService<Permission, String> {

    /**
     * find all permission
     *
     * @return list permission
     */
    List<Permission> findAll();

}

