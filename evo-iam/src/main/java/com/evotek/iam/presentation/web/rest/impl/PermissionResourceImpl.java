package com.evotek.iam.presentation.web.rest.impl;

import com.evotek.common.dto.response.Response;
import com.evotek.iam.application.service.PermissionService;
import com.evotek.iam.domain.Permission;
import com.evotek.iam.presentation.web.rest.PermissionResource;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class PermissionResourceImpl implements PermissionResource {

    private final PermissionService permissionService;

    public PermissionResourceImpl(PermissionService permissionService) {
        this.permissionService = permissionService;
    }

    /**
     * find all permission
     *
     * @return list permission
     */
    @Override
    public Response<List<Permission>> findAll() {
        List<Permission> permissions = this.permissionService.findAll();
        return Response.of(permissions);
    }

}
